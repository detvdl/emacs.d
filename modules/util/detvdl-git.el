;;; detvdl-git.el --- Packages and configuration for git usage
;;; Commentary:
;;; Code:

(use-package ediff
  :ensure t
  :commands ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (progn
    (use-package fringe-helper
      :ensure t
      :functions fringe-helper-convert)
    (defconst fringe-bitmap-line
      (fringe-helper-convert "..xx...."))
    (set-face-attribute 'git-gutter:modified nil :foreground "slateblue3")
    (set-face-attribute 'git-gutter:added nil :foreground "chartreuse4")
    (set-face-attribute 'git-gutter:deleted nil :foreground "firebrick3")

    (define-fringe-bitmap 'git-gutter-fr:added fringe-bitmap-line nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified fringe-bitmap-line nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted fringe-bitmap-line nil nil '(center repeated))
    (global-git-gutter-mode +1)
    (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                          :hint nil)
      "
Git gutter:
  _n_: next hunk        _s_tage hunk     _q_uit
  _p_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _g_popup hunk
  _<_: first hunk
  _>_: last hunk        set start _R_evision
"
      ("n" git-gutter:next-hunk)
      ("p" git-gutter:previous-hunk)
      ("<" (progn (goto-char (point-min))
                  (git-gutter:next-hunk 1)))
      (">" (progn (goto-char (point-min))
                  (git-gutter:previous-hunk 1)))
      ("s" git-gutter:stage-hunk)
      ("r" git-gutter:revert-hunk)
      ("g" git-gutter:popup-hunk)
      ("R" git-gutter:set-start-revision)
      ("q" nil :color blue)
      ("Q" (progn (git-gutter-mode -1)
                  ;; git-gutter-fringe doesn't seem to
                  ;; clear the markup right away
                  (sit-for 0.1)
                  (git-gutter:clear))
       :color blue))
    (define-key global-map (kbd "C-c g") 'hydra-git-gutter/body)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  ;; :demand t
  :hook (magit-post-refresh . git-gutter:update-all-windows)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(setq vc-follow-symlinks t)
;; smart modeline uses vc to show relevant info
;; TODO: figure out a way to utilize magit/git-gutters
;;       git-specific info for sml and disable vc-backends
;; (setq vc-handled-backends nil)

(provide 'detvdl-git)
;;; detvdl-git.el ends here

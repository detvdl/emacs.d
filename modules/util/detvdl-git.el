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
      (fringe-helper-convert
       "..xx...."
       ))
    (define-fringe-bitmap 'git-gutter-fr:added fringe-bitmap-line nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified fringe-bitmap-line nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted fringe-bitmap-line nil nil '(center repeated))
    (global-git-gutter-mode +1)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows))

(setq vc-follow-symlinks t)
;; smart modeline uses vc to show relevant info
;; TODO: figure out a way to utilize magit/git-gutters
;;       git-specific info for sml and disable vc-backends
;; (setq vc-handled-backends nil)

(provide 'detvdl-git)
;;; detvdl-git.el ends here

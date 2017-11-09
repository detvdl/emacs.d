;;; detvdl-git.el --- Packages and configuration for git usage
;;; Commentary:
;;; Code:

(use-package ediff
  :ensure t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package fringe-helper
  :ensure t
  :functions fringe-helper-convert)

(use-package git-gutter-fringe
  :ensure t
  :after fringe-helper
  :diminish git-gutter-mode
  :config
  (progn
    (defconst fringe-bitmap-line
      (fringe-helper-convert
       "..xx...."
       ))
    (global-git-gutter-mode +1)
    (define-fringe-bitmap 'git-gutter-fr:added fringe-bitmap-line nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified fringe-bitmap-line nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted fringe-bitmap-line nil nil '(center repeated)))
  (global-git-gutter-mode +1))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(setq vc-follow-symlinks t
      ;; we're using magit for git
      vc-handled-backends nil)

(provide 'detvdl-git)
;;; detvdl-git.el ends here

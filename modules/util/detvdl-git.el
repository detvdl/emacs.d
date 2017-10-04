;;; detvdl-git.el --- Packages and configuration for git usage
;;; Commentary:
;;; Code:

(use-package ediff
  :ensure t
  :defer t)

(use-package fringe-helper
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
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
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(provide 'detvdl-git)
;;; detvdl-git.el ends here

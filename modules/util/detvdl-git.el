;;; detvdl-git.el --- Packages and configuration for git usage
;;; Commentary:
;;; Code:

(use-package ediff
  :ensure t
  :defer t)

(defconst fringe-bitmap-line
  (fringe-helper-convert
   "..xxx..."
   "..xxx..."
   "..xxx..."
   "..xxx..."
   "..xxx..."
   "..xxx..."
   "..xxx..."
   "..xxx..."
   "..xxx..."
   ))

(use-package git-gutter-fringe
  :ensure t
  :init
  :config
  (progn
    (global-git-gutter-mode +1)
    (define-fringe-bitmap 'git-gutter-fr:added fringe-bitmap-line)
    (define-fringe-bitmap 'git-gutter-fr:modified fringe-bitmap-line)
    (define-fringe-bitmap 'git-gutter-fr:deleted fringe-bitmap-line)
    ))

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

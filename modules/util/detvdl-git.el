;;; detvdl-git.el --- Packages and configuration for git usage
;;; Commentary:
;;; Code:

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package ediff
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(provide 'detvdl-git)
;;; detvdl-git.el ends here

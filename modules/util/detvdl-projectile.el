;;; detvdl-projectile.el --- Projectile bindings and configuration
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(use-package projectile-ripgrep
  :ensure t
  :bind (:map projectile-mode-map
         ("C-c p s s" . projectile-ripgrep)))

(provide 'detvdl-projectile)
;;; detvdl-projectile.el ends here

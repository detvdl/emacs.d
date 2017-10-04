;;; detvdl-projectile.el --- Projectile bindings and configuration
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(provide 'detvdl-projectile)
;;; detvdl-projectile.el ends here

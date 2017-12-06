;;; detvdl-projectile.el --- Projectile bindings and configuration
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file))
  :config
  (progn
    (setq projectile-completion-system 'ivy)
    (with-eval-after-load 'ivy
      (ivy-set-actions 'projectile-find-file
                       '(("j" find-file-other-window "other window")))
      (ivy-set-actions 'projectile-switch-project
                       '(("g" magit-status "magit status"))))
    (projectile-mode)))

(provide 'detvdl-projectile)
;;; detvdl-projectile.el ends here

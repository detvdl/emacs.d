;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(use-package writeroom-mode
  :ensure t
  :defer t
  :bind (("C-c w" . writeroom-mode))
  :config
  (progn
    (setq writeroom-fringes-outside-margins nil
          writeroom-restore-window-config t)))

(use-package smart-mode-line
  :ensure t
  :after doom-themes
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          sml/theme 'dark)
    (sml/setup)))

(use-package plan9-theme
  :ensure t
  :no-require t
  :defer t
  ;; :init
  ;; (load-theme 'plan9 t)
  :config
  (custom-theme-set-faces
   'plan9
   `(fringe ((t (:background "#FFFFE8"))))
   `(linum ((t (:foreground "#b2b2b2"))))))

(use-package doom-themes
  :ensure t
  :no-require t
  :defer t
  :init
  (load-theme 'doom-one t)
  :config
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (doom-themes-org-config)))

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

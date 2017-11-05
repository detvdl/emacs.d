;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(use-package writeroom-mode
  :ensure t
  :bind (("C-c w" . writeroom-mode))
  :config
  (progn
    (setq writeroom-fringes-outside-margins nil
          writeroom-restore-window-config t)
    (add-hook 'writeroom-mode-hook
              (lambda () (if (bound-and-true-p writeroom-mode)
                             (display-line-numbers-mode -1)
                           (display-line-numbers-mode 1))))))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          sml/theme 'respectful)
    (sml/setup)))

;;; THEMES
(use-package plan9-theme
  :disabled t
  :ensure t
  :defer t
  :config
  (custom-theme-set-faces
   'plan9
   `(fringe ((t (:background "#FFFFE8"))))
   `(linum ((t (:foreground "#b2b2b2"))))))

(use-package doom-themes
  :ensure t
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

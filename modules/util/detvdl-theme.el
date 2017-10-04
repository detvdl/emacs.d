;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(use-package nlinum
  :ensure t
  :config
  ;; (global-nlinum-mode)
  (setq nlinum-highlight-current-line t
        nlinum-format "%3d ")
  (eval-after-load 'nlinum
    '(set-face-attribute 'linum nil :height 100)))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (sml/setup)))

(use-package plan9-theme
  :ensure t
  :no-require t
  :defer t
  :init
  (load-theme 'plan9 t)
  :config
  (custom-theme-set-faces 'plan9
                          `(fringe ((t (:background "#FFFFE8"))))
                          `(linum ((t (:foreground "#b2b2b2"))))))

  (provide 'detvdl-theme)
;;; detvdl-theme.el ends here

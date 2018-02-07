;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(use-package writeroom-mode
  :ensure t
  :bind (("C-c w" . writeroom-mode))
  :config
  (progn
    (setq writeroom-fringes-outside-margins nil
          writeroom-restore-window-config t)))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          sml/shorten-directory t
          sml/shorten-modes t
          sml/name-width 40
          sml/mode-width 'full
          sml/theme 'respectful)
    (sml/setup)))

;;; THEMES
(use-package plan9-theme
  :ensure t
  :disabled t
  :config
  (progn
    (load-theme 'plan9 t)
    (custom-theme-set-faces
     'plan9
     `(fringe ((t (:background "#FFFFE8")))))))

(use-package gruvbox-theme
  :ensure t
  :defer t)

(defun set-fringe-and-linum ()
  "Force the fringe to have the same color as the background."
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-face-attribute 'line-number nil
                      :background (face-background 'default)
                      :foreground (face-foreground 'font-lock-comment-face))
  (set-face-attribute 'line-number-current-line nil
                      :background (face-background 'default)
                      :foreground (face-foreground 'default)
                      :weight 'bold))

(defun load-gruvbox-setup ()
  "Set all the faces that are prepared for both light and dark themes."
  (set-face-foreground 'git-gutter:modified "#fabd2f")
  (set-face-foreground 'git-gutter:added    "#b8bb26")
  (set-face-foreground 'git-gutter:deleted  "#fb4933")
  (set-face-attribute 'font-lock-doc-face nil :inherit 'shadow)
  (set-fringe-equal-bg))

(defun load-dark-theme (&optional frame)
  (interactive)
  (when frame
    (select-frame frame))
  (load-theme 'gruvbox-dark-medium t)
  (load-gruvbox-setup)
  (gruvbox-dark-modeline))

(defun load-light-theme (&optional frame)
  (interactive)
  (when frame
    (select-frame frame))
  (load-theme 'gruvbox-light-hard t)
  (load-gruvbox-setup)
  (gruvbox-light-modeline))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-dark-theme)
  (load-dark-theme))

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

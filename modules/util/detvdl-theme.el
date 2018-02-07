;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(use-package writeroom-mode
  :ensure t
  :bind (("C-c w" . writeroom-mode))
  :config
  (setq writeroom-fringes-outside-margins nil
        writeroom-restore-window-config t
        writeroom-global-effects
        (delq 'writeroom-set-fullscreen writeroom-global-effects)))

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
    (load-theme 'plan9 t)))

(use-package challenger-deep-theme
  :ensure t
  :defer t)

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

(defun modeline-extras ()
  "Make a thicker modeline and make sure which-func is readable."
  (let ((active-bg (face-attribute 'mode-line :background nil 'default))
        (inactive-bg (face-attribute 'mode-line-inactive :background nil 'default)))
    (set-face-attribute 'mode-line nil
                        :box `(:line-width 5 :color ,active-bg))
    (set-face-attribute 'mode-line-inactive nil
                        :box `(:line-width 5 :color ,inactive-bg))
    (set-face-attribute 'which-func nil
                        :foreground (face-foreground 'default))))

(defun load-dark-theme (&optional frame)
  (interactive)
  (when frame
    (select-frame frame))
  (load-theme 'challenger-deep t)
  (set-fringe-and-linum)
  (modeline-extras))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-dark-theme)
  (load-dark-theme))

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

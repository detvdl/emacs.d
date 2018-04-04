;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(use-package writeroom-mode
  :ensure t
  :bind (("C-c w" . writeroom-mode))
  :config
  (progn
    (setq writeroom-fringes-outside-margins nil
          writeroom-restore-window-config t
          writeroom-width 120
          writeroom-global-effects
          (delq 'writeroom-set-fullscreen writeroom-global-effects))
    (add-hook 'writeroom-mode-hook (lambda () (display-line-numbers-mode)))))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          sml/shorten-directory t
          sml/shorten-modes t
          sml/name-width 40
          sml/mode-width 'full
          sml/theme nil)
    (sml/setup)))

;;; THEMES
(use-package challenger-deep-theme
  :ensure t
  :disabled t)

(use-package tango-plus-theme
  :ensure t)

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

(defvar personal-light-theme 'tango-plus)
(defvar personal-dark-theme 'challenger-deep)

(defun disable-active-themes ()
  "Disable all currently active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun load-personal-theme (&optional frame type)
  "Load the chose theme of type TYPE into the current FRAME."
  (let ((type (or type 'light)))
    (when frame (select-frame frame))
    (disable-active-themes)
    (pcase type
      ('light (progn (load-theme personal-light-theme t)
                     (setq sml/theme 'light)))
      ('dark (progn (load-theme personal-dark-theme t)
                    (setq sml/theme nil))))
    (sml/setup)
    (set-fringe-and-linum)
    (modeline-extras)))

(defun load-dark-theme (&optional frame)
  "Load the chosen dark theme into the current FRAME."
  (interactive)
  (load-personal-theme frame 'dark))

(defun load-light-theme (&optional frame)
  "Load the chosen light theme into the current FRAME."
  (interactive)
  (load-personal-theme frame 'light)
  ;; (custom-theme-set-faces
  ;;  'tango-plus
  ;;  '(font-lock-comment-face ((t (:foreground "#75507b"))))
  ;;  '(font-lock-builtin-face ((t (:foreground "#5c3566"))))
  ;;  '(markdown-comment-face ((t (:foreground "dim gray"))))
  ;;  '(sml/filename ((t :foreground "#204a87"))))
  )

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-dark-theme)
  (load-dark-theme))

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

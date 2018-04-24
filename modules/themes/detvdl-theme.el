;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

;; (require 'detvdl-zenburn)
(require 'detvdl-modeline)
(require 'ligatures-font)

(use-package writeroom-mode
  :ensure t
  :bind (("C-c w" . writeroom-mode))
  :config
  (progn
    (setq writeroom-fringes-outside-margins nil
          writeroom-restore-window-config t
          writeroom-width 80
          writeroom-global-effects
          (delq 'writeroom-set-fullscreen writeroom-global-effects))))

;;; THEMES
;; Other nice themes to keep in mind: challenger-deep
(use-package farmhouse-theme
  :ensure t
  :defer t
  :init
  (defun after-load-farmhouse ()
    (custom-theme-set-faces
     'farmhouse-light
     '(font-lock-warning-face ((t (:weight bold))))
     '(font-lock-constant-face ((t (:foreground "#84BA11"))))
     '(magit-section-highlight ((t (:background "#f6f2f3"))))
     `(indent-guide-face ((t (:foreground
                              ,(face-attribute 'font-lock-comment-face :foreground)))))
     '(iedit-occurrence ((t (:background "purple" :foreground "white")))))))

(use-package arjen-grey-theme
  :ensure t
  :defer t)

(use-package tango-plus-theme
  :ensure t
  :defer t
  :init
  (defun after-load-tango ()
    (custom-theme-set-faces
     'tango-plus
     '(font-lock-comment-face ((t (:foreground "#75507b"))))
     '(font-lock-builtin-face ((t (:foreground "#5c3566"))))
     '(markdown-comment-face ((t (:foreground "dim gray"))))
     '(show-paren-match ((t (:background "#e9b96e"))))
     '(sml/filename ((t :foreground "#204a87")))
     )
    (set-cursor-color "#000000")))


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

(use-package all-the-icons
  :ensure t
  :demand)

(defvar detvdl:themes (list :light 'farmhouse-light
                            :dark 'arjen-grey))

(defun disable-active-themes ()
  "Disable all currently active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun detvdl:load-theme (&optional frame type &rest after-fns)
  "Load the chosen theme of type TYPE into the current FRAME."
  (let ((type (or type :light))
        (frame (or frame (selected-frame))))
    (select-frame frame)
    (disable-active-themes)
    (load-theme (plist-get detvdl:themes type) t)
    (with-eval-after-load 'all-the-icons (my:set-modeline)))
  (when after-fns
    (mapc (lambda (fn) (when fn (funcall fn))) after-fns)))

(defun load-personal-theme (&optional frame type &rest after-fns)
  "Load the chose theme of type TYPE into the current FRAME."
  (let ((type (or type :light)))
    (when frame (select-frame frame))
    (disable-active-themes)
    (pcase type
      (:light (progn (load-theme personal-light-theme t)))
      (:dark (progn (load-theme personal-dark-theme t))))
    (with-eval-after-load 'all-the-icons (my:set-modeline))
    ))

(defun load-dark-theme (&optional frame)
  "Load the chosen dark theme into the current FRAME."
  (interactive)
  (let ((after-fn (pcase (plist-get detvdl:themes :dark)
                    ('zenburn 'after-load-zenburn)
                    ('sanityinc-tomorrow-bright 'after-load-bright))))
    (detvdl:load-theme frame :dark after-fn 'set-fringe-and-linum)))

(defun load-light-theme (&optional frame)
  "Load the chosen light theme into the current FRAME."
  (interactive)
  (let ((after-fn (pcase (plist-get detvdl:themes :light)
                    ('tango-plus 'after-load-tango)
                    ('farmhouse-light 'after-load-farmhouse))))
    (detvdl:load-theme frame :light after-fn 'set-fringe-and-linum)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-light-theme)
  (load-light-theme))

(add-hook 'prog-mode-hook (lambda () (progn
                                  (my:ligatures-fira-code-setup)
                                  (prettify-symbols-mode 1))))

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

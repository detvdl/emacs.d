;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(require 'detvdl-modeline)
(require 'detvdl-ui)

(defvar detvdl:after-theme-fns '())

(use-package base16-theme
  :ensure t
  :defer t
  :init
  (defun after-load-base16-tomorrow-black ()
    (custom-theme-set-faces
     'base16-tomorrow-black
     '(default ((nil (:background "#0b0c0c" :foreground "#d0d0d0"))))
     '(line-number ((t (:background "#0b0b0c" :foreground "#3b3b3b"))))
     '(line-number-current-line ((t (:background "#2b2b2b" :foreground "#d0d0d0"))))
     ;; '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
     '(region ((nil (:background "#464740"))))
     ;; '(hl-line ((nil (:background "#202020"))))
     '(yas-field-highlight-face ((nil (:background "#333399"))))
     '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
     '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
     '(show-paren-mismatch ((((class color)) (:background "red"))))

     '(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#cccccc" :background "gray20"))))
     '(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#cccccc" :background "gray20"))))
     '(org-block ((t (:background "gray10" :foreground "#de935f"))))
     '(org-code ((t (:foreground "#8abeb7" :weight bold))))
     ))
  (add-to-list 'detvdl:after-theme-fns '(base16-tomorrow-black . after-load-base16-tomorrow-black)))

;; (use-package farmhouse-theme
;;   :load-path "modules/themes/custom-themes/emacs-farmhouse-theme"
;;   :defer t
;;   :init
;;   (defun after-load-farmhouse ()
;;     (custom-theme-set-faces
;;      'farmhouse-light
;;      '(font-lock-warning-face ((t (:foreground "#ec3691" :weight bold))))
;;      '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "purple" :weight bold))))
;;      '(font-latex-math-face ((t (:foreground "#A6825B"))))
;;      '(font-latex-warning-face ((t (:foreground "firebrick1"))))
;;      '(font-latex-sedate-face ((t (:foreground "gray30"))))
;;      '(cider-result-overlay-face ((t (:background "gray90"))))
;;      '(cider-deprecated-face ((t (:background "#FFAF5E"))))
;;      '(indent-guide-face ((t (:foreground "#ccc"))))
;;      '(window-divider ((t (:foreground "grey50"))))
;;      `(line-number ((t (:background ,(face-attribute 'default :background)
;;                         :foreground "#b2b2b2"))))
;;      `(line-number-current-line ((t (:background ,(face-attribute 'hl-line :background)
;;                                      :foreground ,(face-attribute 'default :foreground)))))
;;      '(fringe ((t (:background "grey85"))))
;;      ))
;;   (add-to-list 'detvdl:after-theme-fns '(farmhouse-light . after-load-farmhouse)))

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t
        solarized-emphasize-indicators t
        solarized-scale-org-headlines nil
        solarized-distinct-doc-face t
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)
  (defun after-solarized-light ()
    (set-face-attribute 'default nil :foreground "#666666")
    (set-face-attribute 'line-number-current-line nil :weight 'bold)
    (set-face-attribute 'line-number-current-line nil :foreground "#999999")
    (set-face-attribute 'line-number nil :weight 'regular)
    (set-face-attribute 'line-number nil :foreground "#bbbbbb"))
  (add-to-list 'detvdl:after-theme-fns '(solarized-light . after-solarized-light))
  :defer t)

(use-package gruvbox-theme
  :ensure t
  :defer t)

(use-package tao-theme
  :ensure t
  :defer t)

(use-package nofrils-acme-theme
  :ensure t
  :defer t)

;; Window-dividers
(setq window-divider-default-right-width 4
      window-divider-default-bottom-width 2)
(window-divider-mode t)

(defvar detvdl:themes (list :light 'solarized-light
                            :dark 'base16-tomorrow-black))
(defvar detvdl:default-theme 'solarized-light)
(defvar detvdl:current-theme nil)
(defvar detvdl:default-after-theme-fns '(detvdl:extra-typeface-settings
                                         detvdl:set-modeline))

(defun disable-active-themes ()
  "Disable all currently active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun detvdl:load-theme (&optional theme frame &rest after-fns)
  "Load the chosen THEME into the current FRAME.
AFTER-FNS functions can be provided to be run after loading the theme.
These are merged with `detvdl:default-after-theme-fns'"
  (interactive)
  (let* ((theme (if (called-interactively-p 'any)
                    (intern
                     (completing-read
                      "Choose:"
                      (custom-available-themes)
                      nil t))
                  (or theme (detvdl:default-theme))))
         (type (if (eq (plist-get detvdl:themes :light) theme)
                   :light
                 :dark))
         (frame (or frame (selected-frame)))
         (after-theme-fns (alist-get theme detvdl:after-theme-fns))
         (after-fns (append after-fns detvdl:default-after-theme-fns
                            (if (seqp after-theme-fns)
                                after-theme-fns
                              `(,after-theme-fns)))))
    (setq detvdl:current-theme type)
    (select-frame frame)
    (disable-active-themes)
    (load-theme theme t)
    (when after-fns
      (mapc (lambda (fn) (when fn (funcall fn))) after-fns))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda () (detvdl:load-theme (plist-get detvdl:themes :light))))
  (if (display-graphic-p)
      (detvdl:load-theme (plist-get detvdl:themes :light))
    (detvdl:load-theme (plist-get detvdl:themes :light))
    ))

;; (add-hook 'prog-mode-hook (lambda () (progn (my:ligatures-fira-code-setup)
;;                                             (prettify-symbols-mode 1))))

(defun detvdl:toggle-theme (&optional frame)
  "Toggle between light and dark theme in the current FRAME."
  (interactive)
  (if (eq detvdl:current-theme :light)
      (detvdl:load-theme (plist-get detvdl:themes :dark) frame)
    (detvdl:load-theme (plist-get detvdl:themes :light) frame)))

(global-set-key (kbd "C-, t") 'detvdl:toggle-theme)

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

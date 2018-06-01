;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(require 'detvdl-modeline)
(require 'ligatures-font)
(require 'detvdl-ui)

(defvar detvdl:after-theme-fns '())

(use-package base16-theme
  :ensure t
  :defer t
  :init
  (defun after-load-base16-default-dark ()
    (custom-theme-set-faces
     'base16-default-dark
     '(fringe ((t (:background "#202024"))))
     '(window-divider ((t (:foreground "grey50"))))))
  (defun after-load-base16-tomorrow-night ()
    (custom-theme-set-faces
     'base16-tomorrow-night
     '(fringe ((t (:background "#2D2E32"))))
     '(window-divider ((t (:foreground "grey50"))))
     ))
  (defun after-load-base16-tomorrow-black ()
    (custom-theme-set-faces
     'base16-tomorrow-black
     '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
     '(region ((nil (:background "#464740"))))
     '(hl-line ((nil (:background "#222222"))))
     '(yas-field-highlight-face ((nil (:background "#333399"))))
     '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
     '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
     '(show-paren-mismatch ((((class color)) (:background "red"))))

     '(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#cccccc" :background "gray20"))))
     '(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#cccccc" :background "gray20"))))
     '(org-block ((t (:background "gray10" :foreground "#de935f"))))
     '(org-code ((t (:foreground "#8abeb7" :weight bold))))
     )
    )
  (add-to-list 'detvdl:after-theme-fns '(base16-default-dark . after-load-base16-default-dark))
  (add-to-list 'detvdl:after-theme-fns '(base16-tomorrow-night . after-load-base16-tomorrow-night))
  (add-to-list 'detvdl:after-theme-fns '(base16-tomorrow-black . after-load-base16-tomorrow-black))
  )

(use-package doom-themes
  :ensure t
  :preface (defvar region-fg nil)
  :defer t
  :init
  (defun after-load-doom-tomorrow-night ()
    (custom-theme-set-faces
     'doom-tomorrow-night
     '(fringe ((t (:background "#2D2E32"))))
     '(window-divider ((t (:foreground "grey50"))))
     ;; Ivy minibuffer match faces
     '(ivy-modified-buffer ((t (:foreground "grey75" :inherit bold))))
     ))
  (add-to-list 'detvdl:after-theme-fns '(doom-tomorrow-night . after-load-doom-tomorrow-night)))

(use-package farmhouse-theme
  :load-path "modules/themes/custom-themes/emacs-farmhouse-theme"
  ;; :ensure t
  :defer t
  :init
  (defun after-load-farmhouse ()
    (custom-theme-set-faces
     'farmhouse-light
     '(cursor ((t (:background "grey40"))))
     '(font-lock-warning-face ((t (:foreground "#ec3691" :weight bold))))
     '(font-lock-constant-face ((t (:foreground "#7750BA"))))
     ;; Magit faces (badly supported by theme at the moment)
     '(magit-section-highlight ((t (:background "gray80"))))
     '(magit-diff-file-heading-highlight ((t (:background "gray90"))))
     '(magit-section-heading ((t (:foreground "darkgoldenrod"))))
     '(magit-diff-hunk-heading-highlight ((t (:background "gray80" :foreground "black"))))
     '(magit-diff-hunk-heading ((t (:background "gray80" :foreground "black"))))
     '(magit-diff-context-highlight ((t (:background "gray90"))))
     '(magit-diff-removed-highlight ((t (:background "#eecccc" :foreground "firebrick"))))
     '(magit-diff-added-highlight ((t (:background "#cceecc" :foreground "green4"))))
     '(magit-diff-removed ((t (:background "#eecccc" :foreground "firebrick"))))
     '(magit-diff-added ((t (:background "#cceecc" :foreground "green4"))))
     '(magit-branch-local ((t (:foreground "deepskyblue3" :background nil))))
     '(magit-branch-remote ((t (:foreground "seagreen"))))

     `(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#555555" :background "#E2E1D5"))))
     `(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#555555" :background "#E2E1D5"))))
     '(org-block ((t (:background "gray90"))))
     '(org-code ((t (:foreground "purple" :weight bold))))

     '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "purple" :weight bold))))
     '(font-latex-math-face ((t (:foreground "#A6825B"))))
     '(font-latex-warning-face ((t (:foreground "firebrick1"))))
     '(font-latex-sedate-face ((t (:foreground "gray30"))))

     '(cider-result-overlay-face ((t (:background "gray90"))))
     '(cider-deprecated-face ((t (:background "#FFAF5E"))))

     '(web-mode-current-element-highlight-face ((t (:background "gray85"))))
     '(web-mode-doctype-face ((t (:foreground "gray40"))))
     '(web-mode-html-attr-name-face ((t (:foreground "darkolivegreen"))))
     '(web-mode-html-attr-value-face ((t (:foreground "seagreen"))))

     `(indent-guide-face ((t (:foreground "#ccc"))))
     '(hl-line ((t (:foreground nil :background "#F5F0ED"))))
     '(iedit-occurrence ((t (:background "purple" :foreground "white"))))
     '(anzu-replace-to ((t (:foreground "purple"))))

     '(window-divider ((t (:foreground "grey50"))))
     '(line-number ((t (:background "grey85"))))
     '(line-number-current-line ((t (:background nil :weight bold))))
     '(fringe ((t (:background "grey85"))))))
  ;; (add-to-list 'detvdl:after-theme-fns '(farmhouse-light . after-load-farmhouse))
  )

;; Window-dividers
(setq window-divider-default-right-width 4
      window-divider-default-bottom-width 0)
(window-divider-mode t)

(use-package all-the-icons
  :ensure t
  :demand)

(defvar detvdl:themes (list :light 'farmhouse-light
                            :dark 'base16-tomorrow-black))
(defvar detvdl:default-theme 'base16-tomorrow-black)
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
                  (or theme (detvd:default-theme))))
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
    (add-hook 'after-make-frame-functions (lambda () (detvdl:load-theme (plist-get detvdl:themes :dark))))
  (if (display-graphic-p)
      (detvdl:load-theme (plist-get detvdl:themes :dark))
    (detvdl:load-theme (plist-get detvdl:themes :light))
    ))

(add-hook 'prog-mode-hook (lambda () (progn (my:ligatures-fira-code-setup)
                                            (prettify-symbols-mode 1))))

(defun detvdl:toggle-theme (&optional frame)
  "Toggle between light and dark theme in the current FRAME."
  (interactive)
  (if (eq detvdl:current-theme :light)
      (detvdl:load-theme (plist-get detvdl:themes :dark) frame)
    (detvdl:load-theme (plist-get detvdl:themes :light) frame)))

(global-set-key (kbd "C-, t") 'detvdl:toggle-theme)

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(require 'detvdl-modeline)
(require 'ligatures-font)

(use-package olivetti
  :ensure t
  :bind (("C-c w" . olivetti-mode))
  :config
  (progn
    (setq-default olivetti-body-width 0.75
                  olivetti-body-minimum-width 60
                  olivetti-hide-mode-line t)
    (add-hook 'olivetti-mode-hook
              (lambda () (if (bound-and-true-p olivetti-mode)
                             (progn (display-line-numbers-mode -1)
                                    (git-gutter-mode -1))
                           (progn (display-line-numbers-mode t)
                                  (git-gutter-mode t)))))))

;;; THEMES
;; Other nice themes to keep in mind: challenger-deep

(defvar detvdl:after-theme-fns '())

;; (require 'detvdl-zenburn)

(use-package farmhouse-theme
  :ensure t
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

     '(cider-result-overlay-face ((t (:background "gray90"))))
     '(cider-deprecated-face ((t (:background "#FFAF5E"))))

     '(web-mode-current-element-highlight-face ((t (:background "gray85"))))
     '(web-mode-doctype-face ((t (:foreground "gray40"))))
     '(web-mode-html-attr-name-face ((t (:foreground "darkolivegreen"))))
     '(web-mode-html-attr-value-face ((t (:foreground "seagreen"))))

     `(indent-guide-face ((t (:foreground
                              ,(face-attribute 'font-lock-comment-face :foreground)))))
     '(hl-line ((t (:foreground nil :background "#F5F0ED"))))
     '(iedit-occurrence ((t (:background "purple" :foreground "white"))))
     '(anzu-replace-to ((t (:foreground "purple"))))))
  (add-to-list 'detvdl:after-theme-fns '(farmhouse-light . after-load-farmhouse)))

(use-package arjen-grey-theme
  :ensure t
  :defer t
  :init
  (defun after-load-arjen ()
    (custom-theme-set-faces
     'arjen-grey
     `(which-func ((t (:foreground ,(face-attribute font-lock-function-name-face :foreground)))))
     '(hl-line ((t (:background "gray13"))))))
  (add-to-list 'detvdl:after-theme-fns '(arjen-grey . after-load-arjen)))

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
    (set-cursor-color "#000000"))
  (add-to-list 'detvdl:after-theme-fns '(tango-plus . after-load-tango)))


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

;; Window-dividers
(window-divider-mode t)
(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 2)
(defun detvdl:set-window-dividers ()
  (let ((wd--fg (face-attribute 'mode-line-inactive :background)))
    (set-face-attribute 'window-divider nil :foreground wd--fg)
    (set-face-attribute 'window-divider-first-pixel nil :foreground wd--fg)
    (set-face-attribute 'window-divider-last-pixel nil :foreground wd--fg)))

(use-package all-the-icons
  :ensure t
  :demand)

(defvar detvdl:themes (list :light 'farmhouse-light
                            :dark 'arjen-grey))
(defvar detvdl:current-theme nil)
(defvar detvdl:default-after-theme-fns '(set-fringe-and-linum
                                         detvdl:set-modeline
                                         detvdl:set-window-dividers))

(defun disable-active-themes ()
  "Disable all currently active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun detvdl:load-theme (&optional frame type &rest after-fns)
  "Load the chosen theme of type TYPE into the current FRAME."
  (let* ((type (or type :light))
         (frame (or frame (selected-frame)))
         (theme (plist-get detvdl:themes type))
         (after-theme-fns (alist-get theme detvdl:after-theme-fns))
         (after-fns (append after-fns detvdl:default-after-theme-fns
                            (if (seqp after-theme-fns)
                                after-theme-fns
                              `(,after-theme-fns)))))
    (select-frame frame)
    (disable-active-themes)
    (load-theme theme t)
    (when after-fns
      (mapc (lambda (fn) (when fn (funcall fn))) after-fns))))

(defun load-dark-theme (&optional frame)
  "Load the chosen dark theme into the current FRAME."
  (interactive)
  (setq detvdl:current-theme 'dark)
  (detvdl:load-theme frame :dark))

(defun load-light-theme (&optional frame)
  "Load the chosen light theme into the current FRAME."
  (interactive)
  (setq detvdl:current-theme 'light)
  (detvdl:load-theme frame :light))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-light-theme)
  (if (display-graphic-p)
      (load-light-theme)
    (load-dark-theme)))

(add-hook 'prog-mode-hook (lambda () (progn
                                  (my:ligatures-fira-code-setup)
                                  (prettify-symbols-mode 1))))

(defun detvdl:toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (if (eq detvdl:current-theme 'light)
      (load-dark-theme)
    (load-light-theme)))

(global-set-key (kbd "C-, t") 'detvdl:toggle-theme)

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

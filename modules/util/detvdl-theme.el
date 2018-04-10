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
          writeroom-width 80
          writeroom-global-effects
          (delq 'writeroom-set-fullscreen writeroom-global-effects))))

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
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t
  :init
  (defun after-load-zenburn ()
    (custom-theme-set-faces
     'zenburn
     ;; make zenburn background theme darker
     ;; '(default ((t (:foreground "#DCDCCC" :background "#383838"))))
     ;; '(default ((t (:foreground "#DBDBDB" :background "#383838"))))
     '(default ((t (:foreground "#DEDED1" :background "#383838"))))

     ;; ivy minibuffer config
     '(ivy-minibuffer-match-face-4 ((t (:background "pink4" :underline t))))
     '(ivy-minibuffer-match-face-3 ((t (:background "SteelBlue3" :underline t))))
     '(ivy-minibuffer-match-face-2 ((t (:background "DarkSeaGreen4" :underline t))))
     '(ivy-current-match ((t (:foreground "#F0DFAF" :underline nil :weight bold))))
     '(golden-ratio-scroll-highlight-line-face ((t (:background "gray27"
                                                                :weight normal))))
     ;; magit faces
     '(magit-popup-disabled-argument ((t (:foreground "gray55"))))
     '(magit-popup-key ((t (:foreground "#BFEBBF"))))
     '(magit-section-highlight ((t (:background "gray27"))))
     '(magit-diff-file-heading-highlight ((t (:background "gray27"))))
     '(magit-diff-hunk-heading-highlight ((t (:background "gray27"))))
     '(magit-diff-context-highlight ((t (:background "gray27"))))

     ;; make function face brighter so it's easily distinguishable
     '(font-lock-function-name-face ((t (:foreground "CadetBlue1"))))

     ;; fontify links to make them standout
     '(link ((t (:foreground "#C9B8A2"
                             :underline nil :weight normal))))
     '(link-visited ((t (:foreground "C9AE8C"
                                     :underline nil :weight normal))))

     ;; make everything look gray
     '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
     '(font-lock-comment-face ((t (:foreground "gray55"))))
     '(font-lock-doc-face ((t (:foreground "gray70"))))
     '(shm-current-face ((t (:background "gray27"))))
     '(hl-line ((t (:background "gray27"))))
     '(fringe ((t (:background "gray27"))))
     '(vhl/default-face ((t (:background "gray27"))))
     '(vertical-border ((t (:foreground "gray20"))))

     ;; strike through unmatched parenthesis
     '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :inherit unspecified
                                                          :strike-through t))))

     ;; org-mode face
     '(org-checkbox ((t (:foreground "gray70" :background nil
                                     :weight bold :box nil))))
     '(org-priority ((t (:foreground "gray70" :weight bold
                                     :inherit nil))))
     '(org-date ((((class color)) (:underline nil))))

     ;; term face config
     '(term ((t (:foreground "#E5D9BD"))))
     '(term-color-green ((t (:background "grey30" :foreground "#9F8300"))))

     ;; markdown header face config
     '(markdown-header-face-1 ((t (:foreground "#DFAF8F" :weight bold :height 1.8))))
     '(markdown-header-face-2 ((t (:foreground "#BFEBBF" :weight bold :height 1.6))))
     '(markdown-header-face-3 ((t (:foreground "#7CB8BB" :weight bold :height 1.4))))
     '(markdown-header-face-4 ((t (:foreground "#D0BF8F" :weight bold :height 1.2))))
     '(markdown-header-face-5 ((t (:foreground "#93E0E3" :weight bold :height 1.1))))
     '(markdown-header-face-6 ((t (:foreground "#9FC59F" :weight bold))))))
  )

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

(defvar personal-light-theme)
(defvar personal-dark-theme)
(setq personal-light-theme 'tango-plus)
(setq personal-dark-theme 'zenburn)

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
                    (setq sml/theme 'respectful))))
    (sml/setup)
    (set-fringe-and-linum)))

(defun load-dark-theme (&optional frame)
  "Load the chosen dark theme into the current FRAME."
  (interactive)
  (load-personal-theme frame 'dark)
  (when (eq personal-dark-theme 'zenburn)
    (progn
      (after-load-zenburn)
      (set-fringe-and-linum))))

(defun load-light-theme (&optional frame)
  "Load the chosen light theme into the current FRAME."
  (interactive)
  (load-personal-theme frame 'light))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'load-dark-theme)
  (load-dark-theme))

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

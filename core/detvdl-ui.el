;;; detvdl-ui.el --- Test
;;; Commentary:
;;; Code:

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (or (fboundp 'scroll-bar-mode)
          (window-system))
  (scroll-bar-mode -1))

;; turning menu-bar-mode off messes with OS X' (cocoa)
;; native menu-bar integration
;; (menu-bar-mode -1)

(blink-cursor-mode -1)

;; Wrap long lines
(global-visual-line-mode +1)
(diminish 'visual-line-mode)

;; Highlight current line
(global-hl-line-mode +1)

(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
;; smart-mode-line setup in modules/util/detvdl-theme.el
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(diminish 'size-indication-mode)

;; try out emacs-26 native line numbers
(global-display-line-numbers-mode -1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(defun set-font (font-str)
  "Set the default font to the FONT-STR parameter."
  (add-to-list 'default-frame-alist `((font . ,font-str)))
  (set-face-attribute 'default t :font font-str)
  (when (window-system)
    (set-frame-font `(,font-str))))

;; (set-font "SF Mono-12")
(set-font "Fira Code Retina-12")

(provide 'detvdl-ui)
;;; detvdl-ui.el ends here

;;; detvdl-ui.el --- Test
;;; Commentary:
;;; Code:

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

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
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(defun set-font (font-str)
  "Set the default font to the FONT-STR parameter."
  (add-to-list 'default-frame-alist '(font-str))
  (set-face-attribute 'default t :font font-str))
;; (set-font "Pragmata Pro Mono 12")
(set-font "SF Mono-12")

(provide 'detvdl-ui)
;;; detvdl-ui.el ends here

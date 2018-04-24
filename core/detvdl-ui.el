;;; detvdl-ui.el --- Test
;;; Commentary:
;;; Code:
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (or (fboundp 'scroll-bar-mode)
          (window-system))
  (scroll-bar-mode -1))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'lisp-interaction-mode)

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

;; Window-dividers
(window-divider-mode t)
(setq window-divider-default-right-width 2)
(let ((wd--fg (face-attribute 'mode-line-inactive :background)))
  (set-face-attribute 'window-divider nil :foreground wd--fg)
  (set-face-attribute 'window-divider-first-pixel nil :foreground wd--fg)
  (set-face-attribute 'window-divider-last-pixel nil :foreground wd--fg))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Frame settings (position, size)
(defvar detvdl:default-frame-size '(1720 1060))
(defvar detvdl:default-screen-size '(1900 1194))

(defun detvdl:scale-and-center-frame (&optional frame)
  "Scale FRAME according to `detvdl:default-frame-size' and center it."
  (let ((frame (or frame (selected-frame))))
    (when window-system
      (pcase-let* ((`(,fwidth ,fheight) detvdl:default-frame-size)
                   (`(,swidth ,sheight) detvdl:default-screen-size))
        (progn
          (set-frame-size frame fwidth fheight t)
          (set-frame-position frame
                              (/ (- swidth fwidth) 2)
                              (/ (- sheight fheight) 2)))))))
;; Font settings
(defun detvdl:set-font (font-str)
  "Set the default font to the FONT-STR parameter."
  (add-to-list 'default-frame-alist `(font . ,font-str))
  (set-face-attribute 'default t :font font-str)
  (when (window-system)
    (set-frame-font font-str)))

;; Call utility functions here
(detvdl:scale-and-center-frame)
(detvdl:set-font "Fira Code Retina-12")

(provide 'detvdl-ui)
;;; detvdl-ui.el ends here

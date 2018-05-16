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

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't really use the right fringe currently
(fringe-mode '(8 . 0))

;; Frame settings (position, size)
(defvar detvdl:default-frame-size '(1720 1060))
(defvar detvdl:default-screen-size '(1910 1200))

(defun detvdl:scale-and-center-frame (&optional frame)
  "Scale FRAME according to `detvdl:default-frame-size' and center it."
  (let ((frame (or frame (selected-frame))))
    (when window-system
      (pcase-let ((`(,fwidth ,fheight) detvdl:default-frame-size)
                  (`(,swidth ,sheight) detvdl:default-screen-size))
        (progn
          (set-frame-size frame fwidth fheight t)
          (set-frame-position frame
                              (/ (- swidth fwidth) 2)
                              (/ (- sheight fheight) 2)))))))
(add-hook 'after-make-frame-functions #'detvdl:scale-and-center-frame)

;; Font settings
(defun detvdl:set-font (font-str)
  "Set the default font to the FONT-STR parameter."
  (add-to-list 'default-frame-alist `(font . ,font-str))
  (set-face-attribute 'default t :font font-str)
  (when (window-system)
    (set-frame-font font-str)))

(defconst detvdl:fonts
  '(("Fira Code" . "Fira Code Retina-12")
    ("IBM Plex" . "IBM Plex Mono-12")))

(defun detvdl:change-font ()
  (interactive)
  (let* ((choice (completing-read "Choose: " (mapcar 'car detvdl:fonts) nil t))
         (font (cdr (assoc choice detvdl:fonts))))
    (detvdl:set-font font)))
(global-set-key (kbd "C-. f") 'detvdl:change-font)

;; (detvdl:set-font "Fira Code Retina-12")
(detvdl:set-font "IBM Plex Mono 12")

;; Certain fonts such as IBM Plex Mono mess with frame scaling
;; so we delay this until after setting the font
(detvdl:scale-and-center-frame)

(provide 'detvdl-ui)
;;; detvdl-ui.el ends here

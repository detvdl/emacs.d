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

(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Don't really use the right fringe currently
(fringe-mode '(8 . 0))

;; Frame settings (position, size)
(defvar detvdl:default-frame-size '(1600 980))
(defvar detvdl:default-screen-size '(1670 1050))

(defun detvdl:scale-and-center-frame (&optional frame)
  "Scale FRAME according to `detvdl:default-frame-size' and center it."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (when window-system
      (pcase-let ((`(,fwidth ,fheight) detvdl:default-frame-size)
                  (`(,swidth ,sheight) detvdl:default-screen-size))
        (progn
          (set-frame-size frame fwidth fheight t)
          (set-frame-position frame
                              (/ (- swidth fwidth) 2)
                              (/ (- sheight fheight) 2)))))))
(add-hook 'after-init-hook #'detvdl:scale-and-center-frame)

(defconst detvdl:default-line-spacing 0.3)
(setq-default line-spacing detvdl:default-line-spacing)

;; Font settings
(defun detvdl:set-font (font-family &optional height weight spacing &rest after-hooks)
  "Set the default font to be FONT-FAMILY.
Optionally provide HEIGHT and WEIGHT parameters to be set.
Additional AFTER-HOOKS can be run after setting the font"
  (let ((height (or height 12))
        (weight (or weight 'regular))
        (lspace (or spacing detvdl:default-line-spacing))
        (font-string (concat font-family
                             "-"
                             (number-to-string height)
                             ":"
                             (symbol-name weight))))
    (add-to-list 'default-frame-alist `(font . ,font-string))
    (set-face-attribute 'default t :family font-family :height (* 10 height) :weight weight)
    (setq line-spacing lspace)
    (when (window-system)
      (set-frame-font font-string))
    (when after-hooks
      (mapc (lambda (fn) (when fn (funcall fn))) after-hooks))))

(defconst detvdl:fonts
  '(("Iosevka" . ("Iosevka" 15 light))
    ("Go Mono" . ("Go Mono" 14 regular 0.4))
    ("Fira Code" . ("Fira Code Retina" 14 regular))
    ("Input" . ("Input Mono" 14 extralight))
    ("IBM Plex" . ("IBM Plex Mono" 14 regular))
    ("SF Mono" . ("SF Mono" 14))
    ("Operator" . ("Operator Mono" 15 extralight))
    ("Pragmata" . ("Pragmata Pro Mono" 15 regular))
    ("Hack" . ("Hack" 14))
    ("Hasklig" . ("Hasklig" 14))
    ("Proggy Clean" . ("ProggyCleanTT CE" 20))
    ("VGA SquarePx" . ("PxPlus VGA SquarePx" 20))
    ("IBM 3270" . ("IBM 3270 Semi-Narrow" 16 medium))))

(defun detvdl:extra-typeface-settings ()
  (set-face-attribute 'font-lock-comment-face nil :slant 'oblique)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'medium)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'medium)
  (set-face-attribute 'font-lock-variable-name-face nil :weight 'medium))

(defun detvdl:change-font (&optional name)
  (interactive)
  (let* ((choice (or name (completing-read "Choose: " (mapcar 'car detvdl:fonts) nil t)))
         (font (cdr (assoc choice detvdl:fonts)))
         (opts (make-list (- 5 (length font)) nil)))
    (apply 'detvdl:set-font (append font opts '(detvdl:extra-typeface-settings)))))

(global-set-key (kbd "C-. f") 'detvdl:change-font)

(funcall-interactively 'detvdl:change-font "Fira Code")

;; Font character height can influence frame scaling
;; So we delay this until after setting the font
(detvdl:scale-and-center-frame)

(provide 'detvdl-ui)
;;; detvdl-ui.el ends here

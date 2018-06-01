;;; detvdl-editor.el --- Editor settings and bindings
;;; Commentary:
;;; Code:

(setq-default indent-tabs-mode nil)  ;; tabs are bad, hmmkay
(setq-default tab-width 4)

;; Wrap long lines
(global-visual-line-mode +1)
(diminish 'visual-line-mode)

;; Highlight current line
(global-hl-line-mode +1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; delete the current selection on a keypress
(delete-selection-mode t)

;; delete all trailing whitespace every time file is saved
(add-hook 'before-save-hook
          (lambda () (delete-trailing-whitespace)))

;; hippie expand is dabbrev expand on steroids
(setq abbrev-file-name (expand-file-name "abbrev_defs" emacs-savefile-dir))
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; add 1/5th of current line height to line-spacing
(setq-default line-spacing 0.20)

;; native line-numbers since emacs-26
(setq-default display-line-numbers nil
              display-line-numbers-current-absolute t
              display-line-numbers-widen t
              display-line-numbers-width 3)
(global-display-line-numbers-mode -1)

;; always highlight the matching paren
(show-paren-mode 1)

;; mac specific key-rebindings
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'meta)
  (setq mac-option-modifier 'alt)
  (global-set-key [kp-delete] 'delete-char))

(defun start-emacs ()
  (interactive)
  (call-process (executable-find "emacs") nil 0 nil))

(provide 'detvdl-editor)
;;; detvdl-editor.el ends here

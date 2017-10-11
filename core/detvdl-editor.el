;;; detvdl-editor.el --- Editor settings and bindings
;;; Commentary:
;;; Code:

(setq-default indent-tabs-mode nil)  ;; tabs are bad, hmmkay
(setq-default tab-width 4)

;; delete the current selection on a keypress
(delete-selection-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; delete all trailing whitespace every time file is saved
(add-hook 'before-save-hook
          (lambda () (delete-trailing-whitespace)))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
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

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char))
(setq insert-directory-program (executable-find "gls"))

(provide 'detvdl-editor)
;;; detvdl-editor.el ends here

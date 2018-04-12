;;; detvdl-history.el --- Recentf, sessions, autosaving, the works
;;; Commentary:
;;; Code:

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t
          auto-revert-verbose nil))
  (global-auto-revert-mode 1))

;; Mainly taken from https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el
(use-package savehist
  :ensure t
  :config
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" emacs-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :ensure t
  :commands (recentf-mode)
  :functions (recentf-load-list recentf-cleanup recentf-save-list)
  :init
  (setq recentf-save-file (expand-file-name "recentf" emacs-savefile-dir))
  :config
  (progn
    (setq recentf-max-saved-items 100
          recentf-max-menu-items 15
          ;; disable recentf-cleanup on Emacs start, because it can cause
          ;; problems with remote files
          recentf-auto-cleanup 'never)
    (add-to-list 'recentf-exclude 'recentf-exclude-p)
    (recentf-mode +1)))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun auto-save-command ()
  "Save the current buffer if `auto-save' is not nil."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS of CLASS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

;; advise all window switching functions
(advise-commands "auto-save"
                 (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
                 before
                 (auto-save-command))

(add-hook 'mouse-leave-buffer-hook 'auto-save-command)

(provide 'detvdl-history)
;;; detvdl-history.el ends here

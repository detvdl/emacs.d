;;; detvdl-shell.el --- Packages and setting for shell scripting
;;; Commentary:
;;; Code:

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(provide 'detvdl-shell)
;;; detvdl-shell.el ends here

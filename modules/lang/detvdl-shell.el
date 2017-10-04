;;; detvdl-shell.el --- Packages and setting for shell scripting
;;; Commentary:
;;; Code:

(use-package sh-script
  :ensure t)

;; recognize prezto files as zsh scripts
(defvar prelude-prezto-files '("zlogin" "zlogin" "zlogout" "zpreztorc" "zprofile" "zshenv" "zshrc"))

(mapc (lambda (file)
        (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
      prelude-prezto-files)

(add-hook 'sh-mode-hook
          (lambda ()
            (if (and buffer-file-name
                     (member (file-name-nondirectory buffer-file-name) prelude-prezto-files))
                (sh-set-shell "zsh"))))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(provide 'detvdl-shell)
;;; detvdl-shell.el ends here

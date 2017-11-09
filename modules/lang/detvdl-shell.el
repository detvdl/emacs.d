;;; detvdl-shell.el --- Packages and setting for shell scripting
;;; Commentary:
;;; Code:

(use-package sh-script
  :ensure t
  :defer t
  :mode (("\\.sh\\'" . shell-script-mode)
         ("\\.zsh\\'" . shell-script-mode)
         ("\\zlogin\\'" . shell-script-mode)
         ("\\zlogin\\'" . shell-script-mode)
         ("\\zlogout\\'" . shell-script-mode)
         ("\\zpreztorc\\'" . shell-script-mode)
         ("\\zprofile\\'" . shell-script-mode)
         ("\\zshenv\\'" . shell-script-mode)
         ("\\zshrc\\'" . shell-script-mode)))

;; (add-hook 'sh-mode-hook
;;           (lambda ()
;;             (if (and buffer-file-name
;;                      (member (file-name-nondirectory buffer-file-name) prelude-prezto-files))
;;                 (sh-set-shell "zsh"))))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'detvdl-shell)
;;; detvdl-shell.el ends here

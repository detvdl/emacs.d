;;; detvdl-shell.el --- Packages and setting for shell scripting
;;; Commentary:
;;; Code:

(defun set-mode (files mode)
  "Set use-package style MODE for a list of FILES."
  (mapcar
   (lambda (file)
     (cons (concat file "\\'") mode)) files))

(defvar prezto-files '("zlogin" "zlogout" "zpreztorc" "zprofile" "zshenv" "zshrc"))

(defvar sh-script-modes
  (append '(("\\.sh\\'" . shell-script-mode)
            ("\\.zsh\\'" . shell-script-mode))
          (set-mode prezto-files 'shell-script-mode)))

(add-hook 'sh-mode-hook
          (lambda ()
            (if (and buffer-file-name
                     (member (file-name-nondirectory buffer-file-name) prezto-files))
                (sh-set-shell "zsh"))))

;; Need to eval for the :mode keyword to accept dynamically created values
(eval
 `(use-package sh-script
    :ensure t
    :defer t
    :mode ,sh-script-modes
    ))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'detvdl-shell)
;;; detvdl-shell.el ends here

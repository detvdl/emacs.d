;;; detvdl-elisp.el --- Emacs Lisp packages and configuration
;;; Commentary:
;;; Code:

(use-package elisp-slime-nav
  :ensure t
  :defer t)

(add-hook 'elisp-mode-hook #'rainbow-delimiters-mode)

(provide 'detvdl-elisp)
;;; detvdl-elisp.el ends here

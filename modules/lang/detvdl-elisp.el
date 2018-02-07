;;; detvdl-elisp.el --- Emacs Lisp packages and configuration
;;; Commentary:
;;; Code:

(require 'detvdl-lisp)

(use-package elisp-slime-nav
  :ensure t
  :defer t)

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(provide 'detvdl-elisp)
;;; detvdl-elisp.el ends here

;;; detvdl-clojure.el --- Clojure programming packages and configuration
;;; Commentary:
;;; Code:

(require 'detvdl-lisp)

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :defer t
  :config
  (progn
    (setq nrepl-log-messages t)
    (add-hook 'cider-mode-hook #'subword-mode)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)))

(provide 'detvdl-clojure)
;;; detvdl-clojure.el ends here

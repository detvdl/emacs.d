;;; detvdl-clojure.el --- Clojure programming packages and configuration
;;; Commentary:
;;; Code:

(require 'detvdl-lisp)

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj[xc]?\\'"
         "build\\.boot\\'")
  :config
  (add-hook 'clojure-mode-hook #'subword-mode))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(use-package cider
  :ensure t
  :defer t
  :commands cider-jack-in
  :config
  (progn
    (setq nrepl-log-messages t
          ;; cider-boot-parameters "dev"
          cider-eldoc-display-context-dependent-info t
          cider-eldoc-display-for-symbol-at-point t
          cider-dynamic-indentation nil)
    (add-hook 'cider-mode-hook #'subword-mode)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-repl-mode-hook #'eldoc-mode)))

(use-package clojure-snippets
  :ensure t
  :after clojure-mode
  :config
  (with-eval-after-load 'yasnippet
    (clojure-snippets-initialize)))

(provide 'detvdl-clojure)
;;; detvdl-clojure.el ends here

;;; detvdl-js.el --- Javascript packages and configuration
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'"
         "\\.pac\\'")
  :interpreter "node"
  :config
  (progn
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2"
          js-indent-level 2)
    (js2-imenu-extras-mode +1)))

(use-package tern
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :after tern
  :config
  (add-to-list 'company-backends 'company-tern))

;; browser integration environment
;; (use-package indium)

;; typescript development environment
;; (use-package tide)

(use-package json-mode
  :ensure t)

(provide 'detvdl-js)
;;; detvdl-js.el ends here

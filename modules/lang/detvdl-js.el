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
  :ensure t
  :commands tern-mode
  :diminish tern-mode
  :config
  (use-package company-tern
    :ensure t
    :init
    (with-eval-after-load "company"
      (add-to-list 'company-backends 'company-tern))))

(add-hook 'js2-mode-hook #'tern-mode)


;; browser integration environment
;; (use-package indium)

;; typescript development environment
;; (use-package tide)

(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'"
         "\\.js\\'"))

(provide 'detvdl-js)
;;; detvdl-js.el ends here

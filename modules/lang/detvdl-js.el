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

(use-package json-mode
  :ensure t)

(provide 'detvdl-js)
;;; detvdl-js.el ends here

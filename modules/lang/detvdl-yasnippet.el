;;; detvdl-yasnippet.el --- Yasnippet settings and configuration
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (progn
    (with-eval-after-load "company"
      (setq company-backends '(company-yasnippet)))
    (yas-global-mode 1)))

(provide 'detvdl-yasnippet)
;;; detvdl-yasnippet.el ends here

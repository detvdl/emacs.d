;;; detvdl-company.el --- Company bindings
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :bind (("M-\\" . company-select-next))
  :init (global-company-mode 1)
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t))

(use-package pos-tip
  :ensure t)

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.5))

(provide 'detvdl-company)
;;; detvdl-company.el ends here

;;; detvdl-latex.el -- Latex packages and configuration
;;; Commentary:
;;; Code:

(use-package company-auctex
  :ensure t
  :defer t)

(use-package company-math
  :ensure t
  :after company-auctex)


;; local configuration for TeX modes
(defun my-latex-mode-setup ()
  (company-auctex-init)
  (setq-local company-backends
              (append '((company-math-symbols-latex company-latex-commands))
                      company-backends)))
(add-hook 'TeX-mode-hook 'my-latex-mode-setup)

(setq TeX-PDF-mode t
      TeX-auto-save t
      TeX-parse-self t)
(setq-default TeX-master nil)

(provide 'detvdl-latex)
;;; detvdl-latex.el ends here

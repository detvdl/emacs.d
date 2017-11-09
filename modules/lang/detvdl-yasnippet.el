;;; detvdl-yasnippet.el --- Yasnippet settings and configuration
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :config
  (with-eval-after-load "company"
    (defvar company-mode/enable-yas t)
    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas)
              (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
  (yas-global-mode 1))

(provide 'detvdl-yasnippet)
;;; detvdl-yasnippet.el ends here

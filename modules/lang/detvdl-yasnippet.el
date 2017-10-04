;;; detvdl-yasnippet.el --- Yasnippet settings and configuration
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (eval-after-load 'company
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(provide 'detvdl-yasnippet)
;;; detvdl-yasnippet.el ends here

;;; detvdl-yasnippet.el --- Yasnippet settings and configuration
;;; Commentary:
;;; Code:

(use-package yasnippet
  :if (not noninteractive)
  :ensure t
  :commands (yas-reload-all yas-minor-mode)
  :hook (prog-mode . (lambda () (progn (yas-minor-mode))))
  :diminish yas-minor-mode
  :config
  (with-eval-after-load "company"
    (defvar company-mode/enable-yas t)
    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas)
              (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(provide 'detvdl-yasnippet)
;;; detvdl-yasnippet.el ends here

;;; detvdl-rest.el -- REST client setup and configuration
;;; Commentary:
;;; Code:

(use-package restclient
  :ensure t
  :mode (("\\.rest\\'" . restclient-mode)))

(provide 'detvdl-rest)
;;; detvdl-rest.el ends here

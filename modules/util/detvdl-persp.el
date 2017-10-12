;;; detvdl-persp.el --- Perspective.el configuration
;;; Commentary:
;;; Code:

(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab nil)
  :config
  (perspeen-mode))

(provide 'detvdl-persp)
;;; detvdl-persp.el ends here

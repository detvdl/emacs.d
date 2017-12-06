;;; detvdl-ess.el --- Configuration and package related to statistics
;;; Commentary:
;;; Code:

(use-package ess
  :ensure t
  :mode ("\\.R\\'" . R-mode)
  :config
  (setq ess-indent-with-fancy-comments nil))

(provide 'detvdl-ess)
;;; detvdl-ess.el ends here

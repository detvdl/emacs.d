;;; detvdl-persp.el --- Perspective.el configuration
;;; Commentary:
;;; Code:

(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab nil)
  :config
  (progn
    (perspeen-mode)
    (defun detvdl/initial-persp ()
      (perspeen-rename-ws "home")
      (perspeen-create-ws)
      (perspeen-rename-ws "mail")
      (notmuch)
      (perspeen-goto-last-ws))
    (detvdl/initial-persp)))

(provide 'detvdl-persp)
;;; detvdl-persp.el ends here

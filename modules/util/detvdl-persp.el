;;; detvdl-persp.el --- Perspective.el configuration
;;; Commentary:
;;; Code:

(use-package perspeen
  :ensure t
  :config
  (progn
    (perspeen-mode)
    (setq perspeen-use-tab nil)
    (set-face-attribute 'perspeen-selected-face nil
                        :background "#51afef")
    (defun detvdl/initial-persp ()
      (perspeen-rename-ws "home")
      (perspeen-create-ws)
      (perspeen-rename-ws "mail")
      (notmuch)
      (perspeen-goto-last-ws))
    (detvdl/initial-persp)))

(provide 'detvdl-persp)
;;; detvdl-persp.el ends here

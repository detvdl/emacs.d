;;; detvdl-dired.el --- Dired extra packages and configuration
;;; Commentary:
;;; Code:

(put 'dired-find-alternate-file 'disabled nil)

(use-package dired-rainbow
  :after dired)

(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle))
  :config
  (custom-theme-set-faces
   'farmhouse-light
   `(dired-subtree-depth-1-face ((t (:background ,(face-background 'default)))))
   `(dired-subtree-depth-2-face ((t (:background ,(face-background 'default)))))
   `(dired-subtree-depth-3-face ((t (:background ,(face-background 'default)))))
   `(dired-subtree-depth-4-face ((t (:background ,(face-background 'default)))))
   `(dired-subtree-depth-5-face ((t (:background ,(face-background 'default)))))
   `(dired-subtree-depth-6-face ((t (:background ,(face-background 'default)))))
   )
  )

(provide 'detvdl-dired)
;;; detvdl-dired.el ends here

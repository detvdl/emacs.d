;;; detvdl-org.el --- Org mode configuration
;;; Commentary:
;;; Code:

;; (use-package org-plus-contrib
;; :pin org)

;; (package-refresh-contents)

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (setq org-log-done t
        org-startup-indented nil)
  ;; I *don't* like distinctive header sizes
  ;; I *do* like different font colors for each header level
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.10))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.00))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.00))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.00))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.00))))
   ))

(use-package adaptive-wrap
  :ensure t
  :hook (org-mode . adaptive-wrap-prefix-mode)
  :config
  (progn
    (setq-default adaptive-wrap-extra-indent 2)))

(use-package ox-gfm
  :ensure t
  :after org)

(defun gtd ()
  "Instantly switch to my GTD buffer."
  (interactive)
  (find-file (expand-file-name "org/gtd.org" user-emacs-directory)))

(provide 'detvdl-org)
;;; detvdl-org.el ends here

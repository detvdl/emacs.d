;;; detvdl-org.el --- Org mode configuration
;;; Commentary:
;;; Code:

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (setq org-log-done t
        org-startup-indented t
        org-hidden-keywords '()
        ;; LaTeX preview size is a bit too small for comfort
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
        org-highlight-latex-and-related '(latex))
  ;; I *kinda* like distinctive header sizes
  (custom-set-faces
   '(org-document-title ((t (:inherit outline-1 :height 1.40 :underline t))))
   '(org-document-info ((t (:inherit outline-1 :height 1.20))))
   '(org-document-info-keyword ((t (:inherit outline-1 :height 1.20))))
   '(org-warning ((t (:weight bold :foreground "#CC9393" :height 1.20))))

   '(org-level-1 ((t (:inherit outline-1 :height 1.10))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.00))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.00))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.00))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.00))))
   ))

(use-package alert
  :ensure t
  :defer t
  :config
  (setq alert-default-style 'osx-notifier))

(use-package org-alert
  :ensure t
  :after org)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉"
                                  "○")))

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

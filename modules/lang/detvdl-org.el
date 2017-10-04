;;; detvdl-org.el --- Org mode configuration
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (setq org-log-done t))

(use-package ox-gfm
  :ensure t
  :defer t)

(provide 'detvdl-org)
;;; detvdl-org.el ends here

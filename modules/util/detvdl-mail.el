;;; detvdl-mail.el --- Email utilities and configuration
;;; Commentary:
;;; Code:

(use-package notmuch
  :ensure t
  :defer t
  :config
  (setq notmuch-search-oldest-first nil))

(provide 'detvdl-mail)
;;; detvdl-mail.el ends here

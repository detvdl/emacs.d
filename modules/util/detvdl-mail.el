;;; detvdl-mail.el --- Email utilities and configuration
;;; Commentary:
;;; Code:

(use-package notmuch
  :ensure t
  :defer t
  :init
  (setq-default notmuch-search-oldest-first nil
                notmuch-show-all-tags-list t))

(provide 'detvdl-mail)
;;; detvdl-mail.el ends here

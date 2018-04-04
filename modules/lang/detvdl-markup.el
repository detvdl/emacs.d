;;; detvdl-markup.el --- Markup languages configuration
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package pandoc-mode
  :ensure t
  :hook markdown-mode
  ;; :magic ("README" . (lambda () (pandoc-mode -1)))
  :config
  (progn
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
    ;; We don't need pandoc-mode in github-flavored .md files
    (add-hook 'gfm-mode-hook (lambda () (pandoc-mode -1)))))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(provide 'detvdl-markup)
;;; detvdl-markup.el ends here

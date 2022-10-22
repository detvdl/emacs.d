(use-package yari
  :straight t
  :defer t)

(use-package inf-ruby
  :straight t
  :bind (:map inf-ruby-minor-mode-map
         ("C-x C-e" . ruby-send-last-sexp))
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-tools
  :straight t
  :hook (ruby-mode . ruby-tools-mode))

(use-package robe
  :straight t
  :hook (ruby-mode . robe-mode)
  :config
  ;;  (add-hook 'robe-mode (lambda () (company:add-local-backend 'company-robe)))
  )

(use-package rubocop
  :straight t
  :hook (ruby-mode . rubocop-mode))

(provide 'ruby.el)
;;; ruby.el ends here

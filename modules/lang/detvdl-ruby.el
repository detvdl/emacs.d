;;; detvdl-ruby.el --- Ruby packages and settings
;;; Commentary:
;;; Code:

(use-package ruby-mode
  :ensure t
  :mode ("\\.rake\\'"
         "Rakefile\\'"
         "\\.gemspec\\'"
         "\\.ru\\'"
         "Gemfile\\'"
         "Guardfile\\'"
         "Capfile\\'"
         "\\.cap\\'"
         "\\.thor\\'"
         "\\.rabl\\'"
         "Thorfile\\'"
         "Vagrantfile\\'"
         "\\.jbuilder\\'"
         "Podfile\\'"
         "\\.podspec\\'"
         "Puppetfile\\'"
         "Berksfile\\'"
         "Appraisals\\'")
  :interpreter "ruby"
  :config
  (add-hook 'ruby-mode-hook #'subword-mode))

(use-package yari
  :ensure t
  :defer t)

(use-package inf-ruby
  :ensure t
  :bind (:map inf-ruby-minor-mode-map
              ("C-x C-e" . ruby-send-last-sexp))
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-tools
  :ensure t
  :hook (ruby-mode . ruby-tools-mode)
  :bind (:map ruby-tools-mode-map
              ("C-;" . iedit-mode)))

(use-package rbenv
  :ensure t
  :defer t
  :config
  (global-rbenv-mode)
  (rbenv-use-corresponding))

(use-package robe
  :ensure t
  ;; :hook (ruby-mode . robe-mode)
  :config
  (with-eval-after-load "company"
    (add-to-list 'company-backends 'company-robe)))

(use-package rubocop
  :ensure t
  :hook (ruby-mode . rubocop-mode))

(use-package feature-mode
  :ensure t
  :mode (("\.feature$" . feature-mode)))

(provide 'detvdl-ruby)
;;; detvdl-ruby.el ends here

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
  (add-hook 'ruby-mode-hook #'subword-mode)
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  (add-hook 'ruby-mode-hook #'ruby-tools-mode)
  (add-hook 'ruby-mode-hook #'robe-mode)
  (add-hook 'ruby-mode-hook #'rubocop-mode))

(use-package yari
  :ensure t
  :defer t)

(use-package inf-ruby
  :ensure t
  :commands inf-ruby-minor-mode)

(use-package ruby-tools
  :ensure t
  :commands ruby-tools-mode
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
  :commands robe-mode
  :config
  (with-eval-after-load "company"
    (add-to-list 'company-backends 'company-robe)))

(use-package rubocop
  :ensure t
  :commands rubocop-mode)

(provide 'detvdl-ruby)
;;; detvdl-ruby.el ends here

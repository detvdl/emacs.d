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
  :defer t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-tools
  :ensure t
  :defer t
  :bind (:map ruby-tools-mode-map
              ("C-;" . iedit-mode)))

(use-package rvm
  :ensure t
  :defer t
  :config
  (rvm-use-default))

(use-package robe
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends))
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))

(use-package rubocop
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook #'rubocop-mode))

(provide 'detvdl-ruby)
;;; detvdl-ruby.el ends here

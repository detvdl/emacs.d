;;; detvdl-python.el --- Python programming packages and configuration
;;; Commentary:
;;; Code:

(use-package anaconda-mode
  :ensure t
  :mode "\\.py\\'"
  :config
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends '(company-anaconda :with company-capf))))

(use-package pyenv-mode
  :ensure t
  :mode "\\.py\\'"
  :config
  (progn
    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name."
      (let ((project (projectile-project-name)))
        (if (member project (pyenv-mode-versions))
            (pyenv-mode-set project)
          (pyenv-mode-unset))))
    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
    (add-hook 'python-mode-hook #'pyenv-mode)))

(provide 'detvdl-python)
;;; detvdl-python.el ends here

;;; detvdl-python.el --- Python programming packages and configuration
;;; Commentary:
;;; Code:

(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (setq anaconda-mode-installation-directory (expand-file-name "anaconda-mode" emacs-savefile-dir))
  (use-package company-anaconda
    :ensure t
    :config
    (with-eval-after-load "company"
      (add-to-list 'company-backends '(company-anaconda :with company-capf)))))

(remove-hook 'anaconda-mode-response-read-fail-hook
             'anaconda-mode-show-unreadable-response)

(use-package pyenv-mode
  :ensure t
  :hook python-mode
  :config
  (progn
    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name."
      (let ((project (projectile-project-name)))
        (if (member project (pyenv-mode-versions))
            (pyenv-mode-set project)
          (pyenv-mode-unset))))
    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)))

(provide 'detvdl-python)
;;; detvdl-python.el ends here

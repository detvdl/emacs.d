;;; devtdl-c.el --- C programming packages and configuration
;;; Commentary:
;;; Code:

(use-package ggtags
  :ensure t
  :commands ggtags-mode)

(use-package cc-mode
  :ensure t
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c++-mode)
         ("\\.java\\'" . java-mode))
  :init
  (defun c-mode-common-defaults ()
    (setq c-default-style "linux"
          c-basic-offset 8
          c-tab-always-indent t)
    (c-set-offset 'substatement-open 0)
    ;; make the underscore part of a word in C and C++ modes
    (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
    (modify-syntax-entry ?_ "w" c-mode-syntax-table))
  (defun c-mode-alt-defaults ()
    (interactive)
    (setq c-default-style "gnu"
          c-basic-offset 4))
  (defun makefile-mode-defaults ()
    (whitespace-toggle-options '(tabs))
    (setq indent-tabs-mode t))
  (lambda ()
    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
      (ggtags-mode 1))))

(add-hook 'c-mode-common-hook #'c-mode-common-defaults)
(add-hook 'makefile-mode-hook #'makefile-mode-defaults)

(provide 'detvdl-c)
;;; detvdl-c.el ends here

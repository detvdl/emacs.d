;;; devtdl-c.el --- C programming packages and configuration
;;; Commentary:
;;; Code:

(defun c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-mode 0))

(add-hook 'c-mode-common-hook #'c-mode-common-defaults)

(defun makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'makefile-mode-defaults)

(provide 'detvdl-c)
;;; detvdl-c.el ends here

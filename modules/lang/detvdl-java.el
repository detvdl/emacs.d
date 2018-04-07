;;; detvdl-java.el --- Java Development environment
;;; Commentary:
;;; Code:

(use-package java-snippets
  :ensure t
  :defer t)

(use-package meghanada
  :ensure t
  :commands meghanada-mode)

(add-hook 'java-mode-hook (lambda ()
                            ;; (meghanada-mode t)
                            (semantic-mode t)
                            (with-eval-after-load 'yasnippet
                              (java-snippets-initialize))
                            (setq c-basic-offset 4)))

(provide 'detvdl-java)
;;; detvdl-java.el ends here

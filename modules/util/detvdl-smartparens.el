;;; detvdl-smartparens.el --- Smartparens settings and configuration
;;; Commentary:
;;; Code:

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :functions sp-pair
  :config
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'paredit
          sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)
    ;; define new pairs
    (sp-pair "{" nil :post-handlers
             '(((lambda (&rest _ignored)
                  (crux-smart-open-line-above)) "RET")))
    (sp-pair "{" nil :post-handlers
             '(("||\n[i]" "RET")
               ("| " "SPC")))
    (add-hook 'prog-mode-hook (lambda ()
                                (smartparens-mode +1)))))

(provide 'detvdl-smartparens)
;;; detvdl-smartparens.el ends here

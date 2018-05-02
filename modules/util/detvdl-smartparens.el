;;; detvdl-smartparens.el --- Smartparens settings and configuration
;;; Commentary:
;;; Code:

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :functions sp-pair
  :hook ((prog-mode ess-mode slime-mode slime-repl-mode) . smartparens-mode)
  :config
  (progn
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'paredit
          sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)
    ;; TODO: add and fix pairs for Clojure-specific constructs
    (sp-pair "{" nil :post-handlers
             '(((lambda (&rest _ignored)
                  (crux-smart-open-line-above)) "RET")))
    (sp-pair "{" nil :post-handlers
             '(("||\n[i]" "RET")
               ("| " "SPC")))))

  (provide 'detvdl-smartparens)
;;; detvdl-smartparens.el ends here

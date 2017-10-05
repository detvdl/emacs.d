;;; detvdl-elixir.el --- Elixir packages and configuration
;;; Commentary:
;;; Code:

(use-package elixir-mode
  :ensure t
  :config
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "fn" "end"
                   :when '(("SPC" "RET"))
                   :actions '(insert navigate))
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(sp-ruby-def-post-handler)
                   :actions '(insert navigate))))

(use-package alchemist
  :ensure t)

(provide 'detvdl-elixir)
;;; detvdl-elixir.el ends here

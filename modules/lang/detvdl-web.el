;;; detvdl-web.el --- Packages and configuration for all web-related modes
;;; Commentary:
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.tpl\\'"
         "\\.blade\\.php\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.html?\\'"
         "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'")
  :config
  (setq web-mode-enable-auto-pairing nil)
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>")))

(use-package css-mode
  :ensure t
  :config
  (progn
    ;; Pretty colours for css-mode
    (use-package rainbow-mode
      :ensure t
      :defer t)
    (setq css-indent-offset 2)
    (add-hook 'css-mode-hook #'rainbow-mode)))

(provide 'detvdl-web)
;;; detvdl-web.el ends here

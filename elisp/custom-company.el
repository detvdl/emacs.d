;;;; Company
;; (use-package company
;;   :straight t
;;   :blackout company-mode
;;   :bind (("M-\\" . company-select-next))
;;   :hook ((org-mode prog-mode) . company-mode)
;;   :config
;;   (setq company-idle-delay 0.5
;;         company-tooltip-limit 10
;;         company-minimum-prefix-length 2
;;         company-tooltip-flip-when-above t
;;         company-tooltip-align-annotations t)
;;   (add-hook 'org-mode-hook (lambda () (cl-pushnew 'company-capf company-backends))))

;; (when (and (version<= "26" emacs-version)
;;            (display-graphic-p))
;;   (use-package company-box
;;     :straight t
;;     :after company
;;     :blackout
;;     :custom
;;     (company-box-scrollbar nil)
;;     :hook (company-mode . company-box-mode)
;;     :config
;;     ;; disable tab-bar on the company-box frames
;;     (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0)))
;;   )

;; (use-package company-quickhelp
;;   :straight t
;;   :after company
;;   :config
;;   (use-package pos-tip :straight t)
;;   (company-quickhelp-mode 1)
;;   (setq company-quickhelp-delay 0.5
;;         company-quickhelp-use-propertized-text t))

;; (bind-key "M-\\" #'company-complete-common-or-cycle global-map)

;; (defun company:add-local-backend (backend)
;;   "Add the BACKEND to the local `company-backends' variable."
;;   (if (local-variable-if-set-p 'company-backends)
;;       (add-to-list 'company-backends `(:separate ,backend company-yasnippet))
;;     (add-to-list (make-local-variable 'company-backends)
;;                  `(:separate ,backend company-yasnippet))))

;; (use-package company-prescient
;;   :straight t
;;   :after company prescient
;;   :hook (company-mode . company-prescient-mode)
;;   :config
;;   (defadvice lsp (after advice-lsp activate)
;;     (setq-local company-prescient-sort-length-enable
;;                 (cl-dolist (w lsp--buffer-workspaces)
;;                   (when (thread-first w
;;                                       (lsp--workspace-client)
;;                                       (lsp--client-server-id)
;;                                       (memq '(jsts-ls
;;                                               mspyls
;;                                               bash-ls
;;                                               texlab
;;                                               ts-ls
;;                                               svelte-ls))
;;                                       (not))
;;                     (cl-return t)))))
;;   )

;; (use-package company-restclient
;;   :straight t
;;   :after restclient
;;   :config
;;   (add-hook 'restclient-mode-hook (lambda () (company:add-local-backend 'company-restclient))))

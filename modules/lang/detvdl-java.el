;;; detvdl-java.el --- Java Development environment
;;; Commentary:
;;; Code:

(use-package eclim
  :ensure t
  :defer t
  :init
  (setq-default eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse")
                eclimd-executable "/Applications/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.7.0/bin/eclimd"
                eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.7.0/bin/eclim")
  :config
  (progn
    (setq eclimd-autostart nil
          eclim-print-debug-messages t
          ;; Show compilation errors in minibuffer
          help-at-pt-display-when-idle t
          help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer)
    (add-hook 'java-mode-hook (lambda ()
                                (message "Just stop trying to make emacs work with Java.")
                                (eclim-mode t)))))

(use-package company-emacs-eclim
  :ensure t
  :defer t
  :config
  (with-eval-after-load "company"
    (company-emacs-eclim-setup)
    (setq company-emacs-eclim-ignore-case t)))

(provide 'detvdl-java)
;;; detvdl-java.el ends here

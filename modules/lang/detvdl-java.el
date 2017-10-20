;;; detvdl-java.el --- Java Development environment
;;; Commentary:
;;; Code:

(use-package eclim
  :ensure t
  :commands eclim-mode
  :init
  (setq-default eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse")
                eclimd-executable "/Applications/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.7.0/bin/eclimd"
                eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/plugins/org.eclim_2.7.0/bin/eclim")
  :config
  (setq eclimd-autostart nil
        eclimd-wait-for-process nil
        eclim-print-debug-messages nil
        ;; Show compilation errors in minibuffer
        help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (use-package company-emacs-eclim
    :ensure t
    :config
    (with-eval-after-load "company"
      (company-emacs-eclim-setup)
      (setq company-emacs-eclim-ignore-case t))))

(add-hook 'java-mode-hook 'eclim-mode)
(add-hook 'java-mode-hook 'semantic-mode)



(provide 'detvdl-java)
;;; detvdl-java.el ends here

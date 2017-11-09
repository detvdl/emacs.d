;;; detvdl-java.el --- Java Development environment
;;; Commentary:
;;; Code:

;; (use-package jdee
;;   :ensure t
;;   :mode ("\\.java\\'" . jdee-mode)
;;   :init
;;   (setq jdee-server-dir (expand-file-name "jdee-server" emacs-dir)
;;         jdee-jdk-registry '(("1.9" . "/Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home"))
;;         jdee-jdk '("1.9"))
;;   :config
;;   (use-package memoize
;;     :ensure t))

(use-package eclim
  :ensure t
  :mode ("\\.java\\'" . eclim-mode)
  :defer t
  :config
  (progn
    (setq eclim-eclipse-dirs '("/Users/detlev/.eclipse/java-oxygen/Eclipse.app/Contents/Eclipse")
          eclim-executable "/Users/detlev/.eclipse/java-oxygen/Eclipse.app/Contents/Eclipse/eclim"
          eclimd-autostart t)))

(use-package company-emacs-eclim
  :ensure t
  :defer t
  :config
  (progn
    (company-emacs-eclim-setup)
    (setq company-emacs-eclim-ignore-case t)))

(provide 'detvdl-java)
;;; detvdl-java.el ends here

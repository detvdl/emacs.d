;;; detvdl-java.el --- Java Development environment
;;; Commentary:
;;; Code:

(use-package jdee
  :ensure t
  :init
  (use-package memoize :ensure t)
  :config
  (progn
    (setq jdee-server-dir (expand-file-name "jdee-server" emacs-dir))
    (add-hook 'java-mode-hook #'jdee-mode)
    (setq jdee-jdk-registry '(("1.9" . "/Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/bin")))))

(provide 'detvdl-java)
;;; detvdl-java.el ends here

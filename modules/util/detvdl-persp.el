;;; detvdl-persp.el --- Perspective.el configuration
;;; Commentary:
;;; Code:

(use-package persp-mode
  :ensure t
  :init
  (setq-default persp-keymap-prefix (kbd "C-c n"))
  :config
  (progn
    (setq persp-auto-resume-time -1
          persp-auto-save-num-of-backups 1
          persp-autokill-buffer-on-remove 'kill-weak
          persp-save-dir emacs-persp-dir)
    (persp-mode 1)))

(provide 'detvdl-persp)
;;; detvdl-persp.el ends here

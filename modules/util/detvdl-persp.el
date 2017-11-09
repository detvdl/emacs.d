;;; detvdl-persp.el --- Perspective.el configuration
;;; Commentary:
;;; Code:

(use-package persp-mode
  :ensure t
  :init
  (setq-default persp-keymap-prefix (kbd "C-c n"))
  :config
  (progn
    (setq persp-nil-name "main"
          persp-auto-resume-time -1
          persp-auto-save-num-of-backups 1
          persp-autokill-buffer-on-remove 'kill-weak
          persp-save-dir emacs-persp-dir)

    (with-eval-after-load "ivy"
      (add-hook 'ivy-ignore-buffers
                #'(lambda (b)
                    (when persp-mode
                      (let ((persp (get-current-persp)))
                        (if persp
                            (not (persp-contain-buffer-p b persp))
                          nil)))))
      (setq ivy-sort-functions-alist
            (append ivy-sort-functions-alist
                    '((persp-kill-buffer   . nil)
                      (persp-remove-buffer . nil)
                      (persp-add-buffer    . nil)
                      (persp-switch        . nil)
                      (persp-window-switch . nil)
                      (persp-frame-switch  . nil)))))
    ;; automatically create `notmuch' perspective when mode is activated
    (with-eval-after-load "notmuch"
      (persp-def-auto-persp "notmuch"
                            :parameters '((dont-save-to-file . t))
                            :mode-name ".*notmuch.*"
                            :dyn-env '(after-change-major-mode-functions ;; prevent recursion
                                       (persp-add-buffer-on-find-file nil)
                                       persp-add-buffer-on-after-change-major-mode)
                            :hooks '(change-major-mode-hook)
                            :switch 'frame))
    (persp-mode 1)))

(provide 'detvdl-persp)
;;; detvdl-persp.el ends here

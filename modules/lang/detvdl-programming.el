;;; detvdl-programming.el --- Common settings and packages useful for all programming modes
;;; Commentary:
;;; Code:

(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :config
  (indent-guide-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t))))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package which-func
  :ensure t
  :config
  (which-function-mode 1))

;; enable on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :diminish fly-check-mode
  :config
  (if (fboundp 'global-flycheck-mode)
      (global-flycheck-mode +1)
    (add-hook 'prog-mode-hook #'flycheck-mode)))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook (lambda ()
                            (local-comment-auto-fill)
                            (font-lock-comment-annotations)))

(provide 'detvdl-programming)
;;; detvdl-programming.el ends here

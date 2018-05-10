;;; detvdl-programming.el --- Common settings and packages useful for all programming modes
;;; Commentary:
;;; Code:

(use-package ggtags
  :ensure t
  :hook ((prog-mode slime-repl-mode) . ggtags-mode)
  :bind (:map ggtags-mode-map
              ("C-, f" . ggtags-find-definition)
              ("C-, ." . ggtags-find-tag-dwim)
              ("C-, ," . ggtags-find-reference)
              :map ggtags-mode-prefix-map
              ("." . ggtags-find-tag-dwim)
              ("," . ggtags-find-reference)
              ("d" . ggtags-find-definition))
  :config
  (progn
    (customize-set-variable 'ggtags-mode-prefix-key (kbd "C-c t"))
    (setq ggtags-auto-jump-to-match nil
          ggtags-sort-by-nearness t
          ggtags-global-ignore-case t
          ggtags-enable-navigation-keys nil
          ggtags-completing-read-function 'ivy-completing-read
          ggtags-global-window-height nil)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode slime-mode) . rainbow-delimiters-mode))

(use-package idle-highlight-mode
  :ensure t
  :diminish idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook ((lisp-mode lisp-interaction-mode emacs-lisp-mode clojure-mode) . aggressive-indent-mode)
  :config
  (defvar aggressive-indent/excluded '())
  (setq aggressive-indent/excluded '(html-mode ruby-mode python-mode yaml-mode))
  (dolist (i aggressive-indent/excluded)
    (add-to-list 'aggressive-indent-excluded-modes i))
  ;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode 'ruby-mode 'python-mode 'yaml-mode)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (eq major-mode 'rust-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

(use-package which-func
  :ensure t
  :config
  (which-function-mode 1))

;; enable on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :diminish fly-check-mode)

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\|NOTE\\|WARNING\\):\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook (lambda ()
                            (local-comment-auto-fill)
                            (font-lock-comment-annotations)))

(provide 'detvdl-programming)
;;; detvdl-programming.el ends here

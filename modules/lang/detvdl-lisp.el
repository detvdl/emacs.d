;;; detvdl-lisp.el --- Lisp configuration (to be shared among more specific modes)
;;; Commentary:
;;; Code:

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(defun wrap-with (s)
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(define-key read-expression-map (kbd "TAB") (completion-at-point))
(define-key lisp-mode-shared-map (kbd "M-(") (wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-\"") (wrap-with "\""))

(provide 'detvdl-lisp)
;;; detvdl-lisp.el ends here

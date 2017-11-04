;;; detvdl-lisp.el --- Lisp configuration (to be shared among more specific modes)
;;; Commentary:
;;; Code:

(defun wrap-with (s)
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(define-key read-expression-map (kbd "TAB") (completion-at-point))
(define-key lisp-mode-shared-map (kbd "M-(") (wrap-with "("))
(define-key lisp-mode-shared-map (kbd "M-\"") (wrap-with "\""))

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)

(provide 'detvdl-lisp)
;;; detvdl-lisp.el ends here

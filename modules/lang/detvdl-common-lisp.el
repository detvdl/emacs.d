;;; detvdl-common-lisp.el -- Common-Lisp specific Lisp settings
;;; Commentary:
;;; Code:

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(use-package picolisp
  :load-path "site-lisp/picolisp-mode/"
  :commands picolisp-mode tsm-mode
  :mode ("\\.l$" . picolisp-mode)
  :config
  (setq picolisp-pil-executable "/usr/local/bin/pil"
        picolisp-picolisp-executable "/usr/local/bin/picolisp"
        picolisp-pilindent-executable "/usr/local/bin/pilindent"
        picolisp-documentation-directory "/usr/local/share/picolisp/doc/")
  ;; (add-hook 'picolisp-mode-hook (lambda () (tsm-mode +1)))
  )

(use-package inferior-picolisp
  :after picolisp-mode
  :load-path "site-lisp/picolisp-mode/"
  :commands run-picolisp)

(use-package slime
  :load-path "site-lisp/slime/"
  :commands slime
  :bind (:map slime-mode-map
              ("C-c C-s" . slime-selector))
  :init (require 'slime-autoloads)
  :config
  (progn
    (setq slime-lisp-implementations '((ccl ("/usr/local/bin/ccl"))
                                       (sbcl ("/usr/local/bin/sbcl"))
                                       (pico ("/usr/local/bin/pil")))
          slime-contribs '(slime-fancy slime-company slime-indentation)
          slime-autodoc-use-multiline-p t
          slime-enable-evaluate-in-emacs t
          common-lisp-style-default "sbcl")
    (use-package slime-company
      :load-path "site-lisp/slime-company/"
      :config
      (add-to-list 'company-backends 'company-slime)
      (setq slime-company-completion 'fuzzy))
    (defun slime-enable-concurrent-hints ()
      (interactive)
      (setf slime-inhibit-pipelining nil))))

(provide 'detvdl-common-lisp)
;;; detvdl-common-lisp.el ends here

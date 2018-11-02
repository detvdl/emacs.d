;;; detvdl-prolog.el -- Stuff
;;; Commentary:
;;; Code:

(use-package prolog
  :ensure nil
  ;; :load-path "/site-lisp/prolog.el"
  :mode (("\\.pl$" . prolog-mode)
         ("\\.m$" . mercury-mode))
  :commands prolog-mode
  :init
  (progn
    (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
    (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
    (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t))
  :config
  (progn
    (setq prolog-system 'swi)))

(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

(use-package ediprolog
  :ensure t
  :after prolog
  :bind (:map prolog-mode-map
         ("C-x C-e" . 'ediprolog-dwim)))

(provide 'detvdl-prolog)

;;; detvdl-prolog.el ends here

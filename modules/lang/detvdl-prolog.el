;;; detvdl-prolog.el -- Stuff
;;; Commentary:
;;; Code:

(require 'prolog)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(;;("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

(use-package ediprolog
  :ensure t
  :bind (:map prolog-mode-map
              ("C-c C-c C-d" . 'ediprolog-dwim)))

;;; detvdl-prolog.el ends here

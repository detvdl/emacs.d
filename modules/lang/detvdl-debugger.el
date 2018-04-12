;;; detvdl-debugger.el -- Settings and configuration for the realGUD package
;;; Commentary:
;;; Code:

(use-package realgud
  :ensure t
  :defer t
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

(provide 'detvdl-debugger)
;;; detvdl-debugger.el ends here

;;; detvdl-common.el --- Common packages and utility functions based on them
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    ;; OS X doesn't set envvars globally
    (exec-path-from-shell-copy-envs '("LC_ALL"
                                      "LANG"
                                      "LANGUAGE"))
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

;; avoid duplicate buffer names
(use-package uniquify
  ;; package is built-in
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ;; rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; leave special buffers alone
        uniquify-ignore-buffers-re "^\\*"))

(use-package crux
  :ensure t
  :bind (([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards))
  :config
  (crux-reopen-as-root-mode))

(use-package tramp
  :ensure t
  :defer t
  :config
  (setq tramp-default-method "ssh"))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package anzu
  :ensure t
  :defer t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    ;; autosave the undo-tree history
    ;; (setq undo-tree-history-directory-alist
    ;; `((".*" . ,temporary-file-directory))
    ;; undo-tree-auto-save-history t)
    (global-undo-tree-mode)))

(use-package wgrep
  :ensure t
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package easy-kill
  :ensure t
  :defer t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package iedit
  :ensure t
  :commands (iedit-mode)
  :defer t
  :bind (("C-;" . iedit-mode)))

(use-package multiple-cursors
  :ensure t
  :defer t)

(use-package expand-region
  :ensure t
  :defer t
  :functions (er/expand-region)
  :bind (("C-=" . er/expand-region)))

(use-package drag-stuff
  :ensure t
  :defer t
  :diminish drag-stuff-mode
  :bind (("C-S-<up>" . drag-stuff-up)
         ("C-S-<down>" . drag-stuff-down))
  :config
  (drag-stuff-global-mode 1))

(provide 'detvdl-common)
;;; detvdl-common.el ends here

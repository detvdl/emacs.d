;;; detvdl-common.el --- Common packages and utility functions based on them
;;; Commentary:
;;; Code:

;; MacOS does not set its PATH and environment variables globally (outside of shell)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (progn
    (setq exec-path-from-shell-arguments ()
          exec-path-from-shell-variables (append
                                          exec-path-from-shell-variables
                                          '("LC_ALL"
                                            "LANG"
                                            "LANGUAGE"
                                            "PAGER"
                                            "TERM")))
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

;; TODO: add some more rules
(use-package shackle
  :ensure t
  :init (shackle-mode t)
  :config
  (setq shackle-rules '((compilation-mode :noselect t :align 'below :size 0.35 :other t)
                        (cargo-process-mode :noselect t :align 'below :size 0.25 :other t)
                        ("\\`\\*HTTP Response.*?\\*\\'" :noselect t :regexp t :other t )
                        ("\\`\\*Cargo.*?\\*\\'" :regexp t :align 'below :size 0.25 :other t))
        shackle-default-rule '(:select t)))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?r ?s ?d ?h ?n ?e ?i ?o)
        aw-dispatch-always nil))

(use-package avy
  :ensure t
  :bind (("M-g f" . avy-goto-line)
         ("M-g g" . avy-goto-line)
         ("M-g c" . avy-goto-char-2)))

(use-package imenu-anywhere
  :ensure t
  :bind (("M-I" . ivy-imenu-anywhere)))

;; avoid duplicate buffer names
(use-package uniquify
  :ensure nil  ;; package is built-in
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
  (setq tramp-default-method "ssh"
        tramp-auto-save-directory emacs-savefile-dir
        ))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package undo-tree
  :ensure t
  :defer t
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
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package iedit
  :ensure t
  :commands (iedit-mode)
  :bind (("C-;" . iedit-mode)))

(use-package multiple-cursors
  :ensure t
  :defer t
  ;; https://github.com/magnars/multiple-cursors.el/issues/105
  ;; :init (require 'multiple-cursors)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)
         ("C-M->" . mc/unmark-next-like-this)
         ("C-c C->" . mc/skip-to-next-like-this)
         ("C-c C-<" . mc/skip-to-previous-like-this)
         ("C-c >" . mc/mark-all-like-this))
  :config
  (setq mc/list-file (expand-file-name ".mc-lists.el" emacs-savefile-dir)))

(provide 'detvdl-common)
;;; detvdl-common.el ends here

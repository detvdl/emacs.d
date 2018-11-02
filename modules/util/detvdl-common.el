;;; detvdl-common.el --- Common packages and utility functions based on them
;;; Commentary:
;;; Code:

(defvar detvdl:cheatsheet-directory "~/Documents/Cheatsheets/")
(defun detvdl:cheatsheet (arg)
  "Open my cheatsheet directory with `counsel-find-file'.
If the universal ARG is supplied, open in dired."
  (interactive "P")
  (cond
   ((not arg) (counsel-find-file detvdl:cheatsheet-directory))
   ((= 4 (car arg)) (dired detvdl:cheatsheet-directory))))
(bind-key "C-, c" #'detvdl:cheatsheet global-map)

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
                                            "TERM"
                                            "SSH_AUTH_SOCK"
                                            "GPG_TTY")))
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))))

(use-package imenu-list
  :ensure t
  :bind (("C-'" . detvdl:imenu-list-smart-toggle))
  :init
  (defun detvdl:imenu-list-smart-toggle ()
    (interactive)
    (imenu-list-smart-toggle)
    (when-let* ((buf (get-buffer imenu-list-buffer-name))
                (win (get-buffer-window buf)))
      (with-current-buffer buf
        (setq-local display-line-numbers nil)
        (setq-local left-fringe-width 0)
        (set-window-buffer win buf))))
  :config
  (setq imenu-list-size 0.2
        imenu-list-position 'right))

(use-package sr-speedbar
  :load-path "site-lisp/sr-speedbar/"
  ;; :ensure t
  :commands (sr-speedbar-toggle)
  :bind (("M-i" . detvdl:sr-speedbar-toggle)
         ("M-n" . sr-speedbar-select-window)
         :map speedbar-mode-map
         ("q" . sr-speedbar-close)
         ("b" . sr-speedbar-buffers))
  :init
  (defun sr-speedbar-buffers ()
    (interactive)
    (speedbar-change-initial-expansion-list "buffers"))
  (defun detvdl:sr-speedbar-toggle ()
    (interactive)
    (sr-speedbar-toggle)
    (when-let* ((buf (get-buffer sr-speedbar-buffer-name))
                (win (get-buffer-window buf)))
      (with-current-buffer buf
        (setq-local display-line-numbers nil)
        (setq-local left-fringe-width 0)
        (setq-local window-min-width 30)
        (set-window-buffer win buf))
      ))
  :config
  (setq sr-speedbar-right-side nil
        speedbar-show-unknown-files t
        speedbar-indentation-width 2
        speedbar-use-images nil
        speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'" ;; show hidden files
        sr-speedbar-auto-refresh nil
        sr-speedbar-max-width 40
        sr-speedbar-default-width 30
        sr-speedbar-width 30))

;; (use-package shackle
;;   :ensure t
;;   :init (shackle-mode t)
;;   :config
;;   (setq shackle-default-size 0.33
;;         shackle-default-rule '(:noselect t :other t)
;;         shackle-rules '((compilation-mode           :select t             :align below           :other t)
;;                         (cargo-process-mode                               :align below           :other t)
;;                         ("\\*HTTP Response.*\\*\\'"             :regexp t :align right :size 0.5 :other t)
;;                         ("\\*Cargo.*\\*\\'"         :select t   :regexp t :align below           :other t)
;;                         ("*ggtags-global*"          :select t             :align below           :other t)
;;                         ("*eshell*"                 :select t             :align below           :other t))))

(use-package hydra
  :ensure t
  :defer t)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-. e" . er/expand-region)))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?r ?s ?d ?h ?n ?e ?i ?o)
        aw-dispatch-always nil))

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
        tramp-auto-save-directory emacs-savefile-dir))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :bind (("C-, r" . anzu-query-replace)
         ("C-, C-r" . anzu-query-replace-regexp))
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
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package multiple-cursors
  :ensure t
  :defer t
  ;; https://github.com/magnars/multiple-cursors.el/issues/105
  ;; :init (require 'multiple-cursors)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)
         ("C-M->" . mc/unmark-next-like-this)
         ("C-. C->" . mc/skip-to-next-like-this)
         ("C-. C-<" . mc/skip-to-previous-like-this)
         ("C-. >" . mc/mark-all-like-this)
         ("C-; w" . mc/mark-all-words-like-this)
         :map global-map
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (setq mc/list-file (expand-file-name ".mc-lists.el" emacs-savefile-dir)))

(provide 'detvdl-common)
;;; detvdl-common.el ends here

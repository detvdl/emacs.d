;; -*- lexical-binding: t -*-

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			             ("melpa" . "https://melpa.org/packages/")
			             ("org" . "http://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu")))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package--init-file-ensured t
      package-check-signature nil
      package-enable-at-startup nil)

;; Manually rebuild packages when needed/required with `straight-rebuild-all'
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-version 'straight
      straight-use-package-by-default t
      straight-vc-git-default-protocol 'ssh
      straight-host-usernames '((github . "detvdl")))

(use-package blackout :straight t)
(use-package bind-key :straight t)
(use-package s :straight t)
(use-package dash :straight t)

;;; Emacs 28 introduced an extra parameter to `define-obsolete-function-alias'
(when (version<= "28" emacs-version)
  (define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
    (let ((obsolete-name (pop ll))
          (current-name (pop ll))
          (when (if ll (pop ll) "1"))
          (docstring (if ll (pop ll) nil)))
      (list obsolete-name current-name when docstring))))

;; TODO: fix
(setq x-select-request-type 'STRING)

(use-package no-littering
  :straight t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (with-eval-after-load "recentf"
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defconst emacs-misc-dir (expand-file-name "misc" user-emacs-directory))
(defconst emacs-org-dir (expand-file-name "org" user-emacs-directory))

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(defun list-init-files (directory)
  "List all .init.el files inside DIRECTORY."
  (if (not (file-exists-p directory))
      '()
    (let (init-files-list
          (current-dir-list (directory-files-and-attributes directory t)))
      (dolist (dir-item current-dir-list init-files-list)
        (if (and (not (string-prefix-p ".#" (file-name-base (car dir-item))))
                 (equal ".init.el" (substring (car dir-item) -8)))
            (let ((dir-item-base (substring (car dir-item) 0 -3)))
              (setq init-files-list
                    (cons dir-item-base
                          init-files-list))))))))

(defun platform-init-path ()
  "Return path to directory containing platform-specific init files."
  (let* ((platform-dir (symbol-name system-type))
         (sanitized-platform-dir
          (if (s-contains? "/" platform-dir)
              (car (last (s-split "/" platform-dir)))
            platform-dir)))
    (concat user-emacs-directory
            sanitized-platform-dir)))

;; If there are any customizations per-machine, per-user, load them as well
(mapc 'load
      (sort (list-init-files (platform-init-path))
            'string-lessp))

;; ORG-MODE
(push emacs-org-dir load-path)
(require 'detvdl-org)

;; Source environment variables from init shell on non-shell based init systems
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :straight t
  :custom
  (exec-path-from-shell-variables '("HOME" "PATH" "MANPATH"
                                    "PAGER" "TERM"
                                    "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                                    "LANGUAGE" "LANG" "LC_CTYPE" "LC_ALL"
                                    "LOMBOK_JAR"
                                    "GOPATH" "GOROOT"))
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) initial-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(blink-cursor-mode -1)
(show-paren-mode 1)

(column-number-mode)

(setq-default indicate-empty-lines t)

(setq frame-resize-pixelwise t
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Zoom in and out using text-scale commands
(bind-key "C--" #'text-scale-decrease global-map)
(bind-key "C-+" #'text-scale-increase global-map)

(defvar font-height (face-attribute 'default :height))
(setq inhibit-compacting-font-caches t)

;; PATCHing stuff
(use-package el-patch
  :straight t
  :config
  (setq el-patch-enable-use-package-integration t))

;; (defconst git--state-small-dot
;;   "/* XPM */
;; static char * data[] = {
;; \"14 7 3 1\",
;; \" 	c None\",
;; \"+	c #202020\",
;; \".	c %s\",
;; \"      +++     \",
;; \"     +...+    \",
;; \"    +.....+   \",
;; \"    +.....+   \",
;; \"    +.....+   \",
;; \"     +...+    \",
;; \"      +++     \"};")

;; (defconst git--state-large-dot
;;   "/* XPM */
;; static char * data[] = {
;; \"18 13 3 1\",
;; \" 	c None\",
;; \"+	c #000000\",
;; \".	c %s\",
;; \"                  \",
;; \"       +++++      \",
;; \"      +.....+     \",
;; \"     +.......+    \",
;; \"    +.........+   \",
;; \"    +.........+   \",
;; \"    +.........+   \",
;; \"    +.........+   \",
;; \"    +.........+   \",
;; \"     +.......+    \",
;; \"      +.....+     \",
;; \"       +++++      \",
;; \"                  \"};")

;; (defun git--state-color (state)
;;   "Return an appropriate color string for the given Git STATE."
;;   (cond ((eq state 'edited) "green")
;;         ((eq state 'added) "blue")
;;         ((memq state '(removed conflict unregistered)) "red")
;;         ((memq state '(needs-update needs-merge)) "purple")
;;         ((eq state 'up-to-date) "yellow")
;;         ((eq state 'staged) "yellow")
;;         ((memq state '(ignored unknown)) "gray50")
;;         (t "gray50")))

;; (defun git--state-dot (&optional state)
;;   "Return the appropriate bitmap dot for the given Git STATE."
;;   (let* ((backend (vc-backend buffer-file-name))
;;          (state (or state (if (and backend buffer-file-name)
;;                               (vc-state buffer-file-name backend)
;;                             'unknown)))
;;          (color (git--state-color state)))
;;     (propertize "   "
;;                 'help-echo (format "VC state: %s" state)
;;                 'display
;;                 `(image :type xpm
;;                         :data ,(format git--state-large-dot color)
;;                         :ascent center))))

;; (setq-default mode-line-format
;; 	          '("%e"
;; 		        mode-line-front-space
;; 		        (:eval (git--state-dot))
;; 		        mode-line-mule-info
;; 		        mode-line-client
;; 		        mode-line-modified
;; 		        mode-line-remote
;; 		        mode-line-frame-identification
;; 		        mode-line-buffer-identification
;; 		        "   "
;; 		        mode-line-position
;; 		        (vc-mode vc-mode)
;; 		        "  "
;; 		        mode-line-modes
;; 		        mode-line-misc-info
;; 		        mode-line-end-spaces))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Never type "yes" or "no" again.
(fset 'yes-or-no-p 'y-or-n-p)

;; Always delete selection when typing over or pasting
(delete-selection-mode +1)

;; Remove trailing whitespace when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Automatically create missing parent directories when visiting a new file.
(defun create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory '%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'create-non-existent-directory)

;; Some generic variables.
(setq-default tab-width 4
	          make-backup-files nil
	          indent-tabs-mode nil
              ring-bell-function 'ignore
	          visible-bell nil)

(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

;; Show me which line I'm on.
(global-hl-line-mode +1)

;; Proper line wrapping.
(global-visual-line-mode +1)
(blackout 'visual-line-mode)

;; Uniquify buffers with the same name instead of appending a number.
(setq uniquify-buffer-name-style 'forward
      uniquify-separator " . "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;;; Buffers
(bind-key "C-x C-b" #'ibuffer global-map)

;;;; Dired
(use-package dired-subtree
  :straight t
  :bind (:map dired-mode-map
         ("i" . dired-subtree-insert)
         (";" . dired-subtree-remove)
         ("<tab>" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle))
  :custom
  (dired-subtree-line-prefix "\t"))

;;;; Undo/Redo
(if (version< emacs-version "28")
    (use-package undo-tree
      :straight t
      :defer 1
      :blackout
      :config
      (global-undo-tree-mode +1))
  (progn
    (bind-key "C-/" #'undo-only global-map)
    (bind-key "C-?" #'undo-redo global-map)))

;;;; Auto-revert
;; Automatically revert buffers that have changed on disk
(auto-revert-mode +1)
(blackout 'auto-revert-mode)

;;;; Clipboard
(setq select-enable-clipboard t)
(setq select-active-regions t)
;; (setq save-interprogram-paste-before-kill 1)
(setq yank-pop-change-selection t)

(use-package ace-window
  :straight t
  :blackout ace-window-mode
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?r ?s ?d ?h ?n ?e ?i ?o)
        aw-scope 'global
        aw-ignore-current t
        aw-dispatch-always nil))

;; Edit multiple occurences of a code fragment in one buffer.
;; Used in combination with ivy/swiper and rg for easy refactoring.
;; To refactor for example, search with swiper =C-s=, then activate ivy-occur =C-c C-o=.
;; This opens an occur buffer with all results. Now you can activate wgrep and edit to your heart's content
(use-package wgrep
  :straight t
  :defer t
  :bind (("C-x C-q" . wgrep-change-to-wgrep-mode)
         ("C-c C-c" . wgrep-finish-edit))
  :config
  (setq wgrep-auto-save-buffer t))

;; Helper functions to improve some emacs basics.
(use-package crux
  :straight t
  :bind (([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards))
  :config
  (crux-reopen-as-root-mode))

;; Handy-dandy menu in case you ever forget a keybind.
(use-package which-key
  :straight t
  :blackout which-key-mode
  :config
  (which-key-mode))

;; Quickly select expanding regions and put them in the kill-ring.
(use-package easy-kill
  :straight t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package hydra
  :straight t)

(use-package iedit
  :straight t
  :bind (("C-;" . iedit-mode)
         ("C-:" . iedit-dwim))
  :config
  (defun iedit-dwim (begin end)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "r")
    (let ((occurrence (if (use-region-p)
                          (buffer-substring begin end)
                        (current-word))))
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (narrow-to-defun)
            (iedit-start occurrence (point-min) (point-max)))))))
  )

(use-package expand-region
  :straight t
  :custom
  (expand-region-subword-enabled t)
  :bind ("C-=" . er/expand-region))

;; Does what it says: multiple cursors!
(use-package multiple-cursors
  :straight t
  :functions (mc/num-cursors)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)
         ("C-M->" . mc/unmark-next-like-this)
         ("C-. C->" . mc/skip-to-next-like-this)
         ("C-. C-<" . mc/skip-to-previous-like-this)
         ("C-. >" . hydra-multiple-cursors/body)
         :map global-map
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("M-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (defhydra hydra-multiple-cursors (:hint nil)
    "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search
 [Click] Cursor at point       [_q_] Quit"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("s" mc/mark-all-in-region-regexp :exit t)
    ("0" mc/insert-numbers :exit t)
    ("A" mc/insert-letters :exit t)
    ("<mouse-1>" mc/add-cursor-on-click)
    ;; Help with click recognition in this hydra
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore)
    ("q" nil)))

(use-package deadgrep
  :straight t
  :bind (("C-c k" . deadgrep))
  :config
  (require 'dash)
  (defun deadgrep--arguments-patch (rg-arguments)
    "Add --no-ignore-vcs to rg-command."
    (-insert-at (- (length rg-arguments) 3) "--no-ignore-vcs" rg-arguments))
  (advice-add 'deadgrep--arguments :filter-return #'deadgrep--arguments-patch))

;;;; Ivy
(use-package ivy
  :straight t
  :blackout ivy-mode
  :hook (after-init . ivy-mode)
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-x C-f" . counsel-find-file)
         ("C-c y" . counsel-yank-pop)
         ;; TODO: investigate https://github.com/Wilfred/deadgrep as a possible substitute/enrichment
         ("C-c g" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x 8" . counsel-unicode-char)
         ("C-x b" . ivy-switch-buffer)
	     ("C-c C-r" . ivy-resume)
         ("C-c C-u" . swiper-all)
         ("M-x" . counsel-M-x)
         :map ivy-occur-mode-map
         ("w" . ivy-wgrep-change-to-wgrep-mode)
         ("C-x C-q" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-m" . ivy-alt-done)
         ("C-j" . ivy-done)
         ("<escape>" . minibuffer-keyboard-quit))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  (enable-recursive-minibuffers t)
  (ivy-display-style nil)
  (ivy-height 8) ;; used when posframe is not available
  (ivy-virtual-abbreviate 'full)
  (ivy-extra-directories nil)
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (counsel-ag-function . ivy--regex-plus)
                           (counsel-grep-function . ivy--regex-plus)
                           (swiper-all . ivy--regex-plus)
                           (swiper-isearch . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-rg-base-command '("rg" "--max-columns" "240" "--with-filename" "--no-heading"
                             "--line-number" "--color" "never" "--no-ignore-vcs" "%s"))
  :config
  ;; Fuzzy matching
  (use-package flx :straight t))

(use-package swiper
  :straight t
  :after ivy
  :bind ("M-n" . swiper-thing-at-point)
  :config
  (setq swiper-use-visual-line-p #'ignore))

(use-package counsel
  :straight t
  :after swiper
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\'"))

(use-package aweshell
  :straight (aweshell
             :host github :type git
             :repo "manateelazycat/aweshell"
             :fork t)
  :commands (aweshell-new aweshell-toggle aweshell-dedicated)
  :bind (("C-c s t" . aweshell-toggle)
         ("C-c s s" . aweshell-new)
         ("C-c s n" . aweshell-next)
         ("C-c s p" . aweshell-prev)
         ("C-c s d" . aweshell-dedicated-toggle))
  :custom
  (aweshell-search-history-key "C-r")
  (aweshell-auto-suggestion-p nil))

(use-package ggtags
  :straight t
  :hook ((c-mode c++-mode java-mode) . ggtags-mode)
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  ;; ensure ace-window keybind doesn't get overridden in ggtags-mode buffers
  (unbind-key "M-o" ggtags-navigation-map))

;; ensure no stale imenu tags in treemacs or otherwise
(setq imenu-auto-rescan t)

(use-package imenu-list
  :straight t
  :bind ("C-'" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil
        imenu-list-size 0.25
        imenu-list-position 'right))

;;;; Symbol Highlighting
(use-package symbol-overlay
  :straight t
  :blackout
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (set-face-attribute 'symbol-overlay-default-face nil :background "DarkOrchid" :foreground "white"))

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :blackout
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|) ; left-align vertical bar
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "darkgray"))

;;;; Dired
(use-package dired-subtree
  :straight t
  :bind (:map dired-mode-map
         ("i" . dired-subtree-insert)
         (";" . dired-subtree-remove)))

;;;; Comment Keywords
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

;;;; Smartparens
(use-package smartparens
  :straight (smartparens
             :host github :type git
             :repo "Fuco1/smartparens"
             :fork (:host github
                    :repo "sirikid/smartparens"
                    :branch "hotfix/when-let"))
  :blackout smartparens-mode
  :hook ((prolog-mode prog-mode ess-mode sly-mode slime-mode slime-repl-mode org-mode) . smartparens-mode)
  :functions (sp-wrap-with-pair)
  :bind (("C-. )" . sp-rewrap-sexp)
         ("C-. (" . sp-rewrap-sexp))
  :config
  (require 'smartparens-config)
  ;; reserve this keybing for `xref-find-references'
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair t
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (unbind-key "M-?" smartparens-mode-map)
  ;; TODO: add and fix pairs for Clojure-specific constructs
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "{" nil :post-handlers
           '(("||\n[i]" "RET")
             ("| " "SPC")))
  (sp-pair "[" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "[" nil :post-handlers
           '(("||\n[i]" "RET")
             ("| " "SPC"))))

(defun wrap-with (s)
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(bind-key "M-(" (lambda () (wrap-with "(")) lisp-mode-shared-map)
(bind-key "M-\"" (lambda () (wrap-with "\"")) lisp-mode-shared-map)

;;;; Projectile
(use-package projectile
  :straight t
  :blackout projectile-mode
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recentf
        projectile-indexing-method 'alien)
  (with-eval-after-load "ivy"
    (ivy-set-actions 'projectile-find-file
                     '(("j" find-file-other-window "other window")))
    (ivy-set-actions 'projectile-switch-project
                     '(("g" magit-status "magit status")))))

;;;; Rainbows
(use-package rainbow-delimiters
  :straight t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode slime-mode sly-mode) . rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :straight t
  :blackout t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode slime-mode sly-mode) . color-identifiers-mode)
  :custom
  (color-identifiers:min-color-saturation 0.3)
  :config
  (color-identifiers:set-declaration-scan-fn
   'lisp-mode 'color-identifiers:elisp-get-declarations)

  (add-to-list
   'color-identifiers:modes-alist
   `(lisp-mode . (""
                  "\\_<\\(\\(?:\\s_\\|\\sw\\)+\\)"
                  (nil)))))

;;;; Movement
(use-package avy
  :straight t
  ;; :bind (("M-n l" . avy-goto-line)
  ;;        ("M-n c" . avy-goto-char)
  ;;        ("M-n f" . avy-goto-char-2)
  ;;        ("M-n w" . avy-goto-word-1))
  :config
  (avy-setup-default))

(use-package all-the-icons
  :straight t
  :config
  (setq all-the-icons-scale-factor 1.0))

;;; Treemacs
(use-package treemacs
  :straight t
  :bind (("M-0" . treemacs-select-window)
         ("M-'" . treemacs)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :custom
  (treemacs-indentation-string (propertize " \| " 'font-lock-face '(:foreground "lightgray")))
  (treemacs-indentation 1)
  (treemacs-no-png-images nil)
  (treemacs-display-in-side-window t)
  (treemacs-width 30)
  (treemacs-silent-refresh t)
  (treemacs-silent-filewatch t)
  (treemacs-show-hidden-files t)
  (treemacs-sorting 'alphabetic-case-insensitive-desc)
  (treemacs-follow-after-init t)
  (treemacs-project-follow-cleanup t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.0)
  (treemacs-recenter-distance 0.1)
  (treemacs-recenter-after-tag-follow 'on-distance)
  (treemacs-recenter-after-file-follow 'on-distance)
  (treemacs-file-event-delay 1000)
  (treemacs-file-follow-delay 0.1)
  :config
  (treemacs-follow-mode t)
  (treemacs-tag-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (add-hook 'treemacs-mode-hook (lambda ()
                                  (display-line-numbers-mode -1)
                                  (when (display-graphic-p)
                                    (set-window-fringes nil 00))
                                  (setq tab-width 1)
                                  (setq mode-line-format nil)
                                  (buffer-face-mode 1)))
  ;; Improve treemacs icons
  (with-eval-after-load 'treemacs
    (unless (require 'all-the-icons nil t)
      (error "all-the-icons isn't installed"))
    ;; minimalistic atom-inspired icon theme
    (treemacs-create-theme "doom"
      :config
      (let ((face-spec '(:inherit font-lock-doc-face :slant normal)))
        (treemacs-create-icon
         :icon (format " %s\t" (all-the-icons-octicon "repo" :v-adjust -0.1 :face face-spec))
         :extensions (root))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face face-spec)
                       (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
         :extensions (dir-open))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                       (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
         :extensions (dir-closed))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face face-spec)
                       (all-the-icons-octicon "package" :v-adjust 0 :face face-spec)) :extensions (tag-open))
        (treemacs-create-icon
         :icon (format "%s\t%s\t"
                       (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                       (all-the-icons-octicon "package" :v-adjust 0 :face face-spec))
         :extensions (tag-closed))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face face-spec))
         :extensions (tag-leaf))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "flame" :v-adjust 0 :face face-spec))
         :extensions (error))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "stop" :v-adjust 0 :face face-spec))
         :extensions (warning))
        (treemacs-create-icon
         :icon (format "%s\t" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face face-spec))
         :extensions (info))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-media" :v-adjust 0 :face face-spec))
         :extensions ("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff" "svg" "bmp"
                      "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
                      "wav" "mp3" "ogg" "midi"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-code" :v-adjust 0 :face face-spec))
         :extensions ("yml" "yaml" "sh" "zsh" "fish" "c" "h" "cpp" "cxx" "hpp"
                      "tpp" "cc" "hh" "hs" "lhs" "cabal" "py" "pyc" "rs" "el"
                      "elc" "clj" "cljs" "cljc" "ts" "tsx" "vue" "css" "html"
                      "htm" "dart" "java" "kt" "scala" "sbt" "go" "js" "jsx"
                      "hy" "json" "jl" "ex" "exs" "eex" "ml" "mli" "pp" "dockerfile"
                      "vagrantfile" "j2" "jinja2" "tex" "racket" "rkt" "rktl" "rktd"
                      "scrbl" "scribble" "plt" "makefile" "elm" "xml" "xsl" "rb"
                      "scss" "lua" "lisp" "scm" "sql" "toml" "nim" "pl" "pm" "perl"
                      "vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"
                      "cask" "r" "re" "rei" "bashrc" "zshrc" "inputrc" "editorconfig"
                      "gitconfig"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "book" :v-adjust 0 :face face-spec))
         :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                      "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                      "azw3" "kf8" "kfx" "lit" "prc" "mobi" "exe" "or" "html"
                      "pkg" "opf" "txt" "pdb" "ps" "rtf" "pdg" "xml" "tr2"
                      "tr3" "oxps" "xps"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face face-spec))
         :extensions ("md" "markdown" "rst" "log" "org" "txt"
                      "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face face-spec))
         :extensions ("exe" "dll" "obj" "so" "o" "out"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-pdf" :v-adjust 0 :face face-spec))
         :extensions ("pdf"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-zip" :v-adjust 0 :face face-spec))
         :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))
        (treemacs-create-icon
         :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face face-spec))
         :extensions (fallback)))))
  (treemacs-load-theme "doom"))

(use-package treemacs-projectile
  :after treemacs projectile
  :straight t)

;; Always enable eldoc
(global-eldoc-mode +1)
(blackout 'eldoc-mode)

;;;; Error checking
(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode))

;;;; Indentation
;; Aggressively indent everything (except for basically all non-lisp modes)!
(use-package aggressive-indent
  :straight t
  :blackout
  :hook ((lisp-mode lisp-interaction-mode emacs-lisp-mode) . aggressive-indent-mode)
  :config
  (defvar aggressive-indent/excluded '())
  (setq aggressive-indent/excluded '(html-mode ruby-mode python-mode yaml-mode))
  (dolist (i aggressive-indent/excluded)
    (add-to-list 'aggressive-indent-excluded-modes i))
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (eq major-mode 'rust-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Utility function to re-indent entire file
(defun indent-whole-file ()
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key "C-c i f" #'indent-whole-file global-map)

;; Emacs-lisp does not indent keyword-plists correctly. This function fixes that
;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

;;;; Describe thing at point
;; handy function from https://www.emacswiki.org/emacs/DescribeThingAtPoint
(defun describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call"
  (interactive)
  (let (sym)
	;; sigh, function-at-point is too clever.  we want only the first half.
	(cond ((setq sym (ignore-errors
                       (with-syntax-table emacs-lisp-mode-syntax-table
                         (save-excursion
                           (or (not (zerop (skip-syntax-backward "_w")))
                               (eq (char-syntax (char-after (point))) ?w)
                               (eq (char-syntax (char-after (point))) ?_)
                               (forward-sexp -1))
                           (skip-chars-forward "`'")
        	               (let ((obj (read (current-buffer))))
                             (and (symbolp obj) (fboundp obj) obj))))))
           (describe-function sym))
          ((setq sym (variable-at-point)) (describe-variable sym))
          ;; now let it operate fully -- i.e. also check the
          ;; surrounding sexp for a function call.
          ((setq sym (function-at-point)) (describe-function sym)))))

;;;; Xref
(bind-key "M-." 'xref-find-definitions prog-mode-map)
(bind-key "M-," 'xref-pop-marker-stack prog-mode-map)
(bind-key "M-?" 'xref-find-references prog-mode-map)
(bind-key "M-[" 'describe-thing-at-point prog-mode-map)

(use-package yasnippet
  :if (not noninteractive)
  :straight t
  :blackout yas-minor-mode
  :commands (yas-reload-all yas-minor-mode))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;;;; Company
(use-package company
  :straight t
  :blackout company-mode
  :bind (("M-\\" . company-select-next))
  :hook ((org-mode prog-mode) . company-mode)
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t)
  (add-hook 'org-mode-hook (lambda () (cl-pushnew 'company-capf company-backends))))

(when (and (version<= "26" emacs-version)
           (display-graphic-p))
  (use-package company-box
    :straight t
    :after company
    :blackout
    :custom
    (company-box-scrollbar nil)
    :hook (company-mode . company-box-mode))
  )

(use-package company-quickhelp
  :straight t
  :after company
  :config
  (use-package pos-tip :straight t)
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.5
        company-quickhelp-use-propertized-text t))

(bind-key "M-\\" #'company-complete-common-or-cycle global-map)

(defun company:add-local-backend (backend)
  "Add the BACKEND to the local `company-backends' variable."
  (if (local-variable-if-set-p 'company-backends)
      (add-to-list 'company-backends `(:separate ,backend company-yasnippet))
    (add-to-list (make-local-variable 'company-backends)
                 `(:separate ,backend company-yasnippet))))

;;;; Language Server Protocol (LSP)
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-eldoc-enable-hover t
        lsp-eldoc-render-all nil
        lsp-prefer-flymake nil))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap describe-thing-at-point] . lsp-describe-thing-at-point)
         ("C-. p" . lsp-ui-doc-glance))
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-update-mode 'line))

;;;; Magit
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        vc-follow-symlinks t)
  (use-package other-frame-window
    :straight t
    :config
    (defun magit-display-buffer-popup-frame (buffer)
      (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
          (display-buffer buffer '((display-buffer-reuse-window
                                    ofw-display-buffer-other-frame)
                                   (reusable-frames . t)))
        (magit-display-buffer-traditional buffer)))
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
  (advice-add #'magit-key-mode-popup-committing :after
              (lambda ()
                (magit-key-mode-toggle-option (quote committing) "--verbose"))))

;; Git Diff
;; Visual diff feedback in the margin/gutter
(use-package diff-hl
  :straight (diff-hl
             :type git :host github
             :repo "dgutov/diff-hl")
  :commands (diff-hl-update)
  :config
  (set-face-attribute 'diff-hl-change nil :height font-height)
  (set-face-attribute 'diff-hl-delete nil :height font-height)
  (set-face-attribute 'diff-hl-insert nil :height font-height)
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
;; Only load the diff-hl package once we actually visit a file
;; This hook gets added by global-diff-hl mode anyway
(add-hook 'find-file-hook #'diff-hl-update)

;; Don't let ediff create any fancy layouts, just use a proper, separate buffer.
(use-package ediff
  :straight t
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package editorconfig
  :straight t
  :blackout
  :config
  (editorconfig-mode 1))

(use-package direnv
  :straight t
  :config
  (direnv-mode))

;; Prescient: sorting by frecency
(use-package prescient
  :straight t
  :config
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :straight t
  :after (ivy counsel prescient)
  :custom
  (ivy-prescient-enable-filtering nil)
  (ivy-prescient-sort-commands
   '(:not swiper swiper-isearch ivy-switch-buffer vrt-news))
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :straight t
  :after company prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (defadvice lsp (after advice-lsp activate)
    (setq-local company-prescient-sort-length-enable
                (cl-dolist (w lsp--buffer-workspaces)
                  (when (thread-first w
                          (lsp--workspace-client)
                          (lsp--client-server-id)
                          (memq '(jsts-ls
                                  mspyls
                                  bash-ls
                                  texlab
                                  ts-ls
                                  svelte-ls))
                          (not))
                    (cl-return t)))))
  )


;;;; PlantUML
(use-package plantuml-mode
  :straight t
  :mode ("\\.puml\\'" . plantuml-mode)
  :custom
  (plantuml-default-exec-mode 'jar))

;;;; Emacs Lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (company:add-local-backend 'company-elisp)))

(use-package emr
  :straight (emr
             :host github :type git
             :repo "Wilfred/emacs-refactor"
             :fork t)
  :bind (:map prog-mode-map
         ("M-RET" . emr-refactor)))

;;;; Common Lisp
;; the SBCL configuration file is written in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(use-package sly
  :straight t
  :commands (sly)
  :init
  (setq inferior-lisp-program "sbcl -Q -l ~/.sbclrc run")
  :config
  (use-package sly-asdf
    :straight t
    :config (add-to-list 'sly-contribs 'sly-asdf 'append))
  (use-package sly-macrostep :straight t)
  (use-package sly-named-readtables :straight t)
  ;; (use-package sly-quicklisp :straight t)
  (sly-setup))

;; (use-package sly-stepper
;;   :straight (sly-stepper
;;              :host github :type git
;;              :repo "joaotavora/sly-stepper"
;;              :files (:defaults "*.lisp" "*.asd" (:exclude "sly-stepper-autoloads.el")))
;;   :after sly sly-stickers)

;;;; Clojure
(use-package clojure-mode
  :straight t
  :mode ("\\.clj[xc]?\\'"
         "build\\.boot\\'"
         "project\\.clj\\'")
  :config
  (add-hook 'clojure-mode-hook #'subword-mode))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(use-package cider
  :straight t
  :defer t
  :commands (cider-jack-in-clj&cljs
             cider-jack-in
             cider-jack-in-cljs
             cider-connect
             cider-connect-cljs)
  :config
  (defun cider-jack-in-with-profile (profile)
    (interactive "sEnter profile name: ")
    (letrec ((lein-params (concat "with-profile +" profile " repl :headless")))
      (message "lein-params set to: %s" lein-params)
      (set-variable 'cider-lein-parameters lein-params)
      (cider-jack-in '())))
  (progn
    (setq nrepl-log-messages t
          cider-eldoc-display-context-dependent-info t
          cider-eldoc-display-for-symbol-at-point t
          cider-dynamic-indentation t)
    (add-hook 'cider-mode-hook #'subword-mode)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-repl-mode-hook #'eldoc-mode)))

(use-package clojure-snippets
  :straight t
  :after clojure-mode
  :config
  (with-eval-after-load 'yasnippet
    (clojure-snippets-initialize)))

(use-package clj-refactor
  :straight t
  :after clojure-mode
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
  )

;;;; Scheme
(use-package geiser
  :straight t
  :commands run-geiser
  :defines scheme-mode-map
  :bind (:map scheme-mode-map
         ("C-x C-e" . geiser-eval-last-sexp)))

;;;; Prolog
(use-package prolog
  :ensure nil
  :mode (("\\.pro$" . prolog-mode)
         ("\\.m$" . mercury-mode))
  :init
  (progn
    (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
    (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
    (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t))
  :config
  (setq prolog-system 'swi))

(use-package ediprolog
  :straight t
  :after prolog
  :bind (:map prolog-mode-map
         ("C-x C-e" . 'ediprolog-dwim)))

;;;; C-like modes
(use-package cc-mode
  :ensure nil
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c++-mode)
         ("\\.java\\'" . java-mode))
  :init
  (defun c-mode-common-defaults ()
    (setq c-default-style "gnu"
          c-basic-offset 4
          c-tab-always-indent t)
    (c-set-offset 'substatement-open 0)
    ;; make the underscore part of a word in C and C++ modes
    (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
    (modify-syntax-entry ?_ "w" c-mode-syntax-table))
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (c-mode-common-defaults)))

(defun makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook #'makefile-mode-defaults)

;;;; Go
(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :bind (:map go-mode-map
         ("C-c c" . compile)
         ("C-c r" . recompile))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun my--go-mode-hook ()
    (setq-local indent-tabs-mode 1)
    (setq-local tab-width 4)
    (subword-mode +1)
    (yas-minor-mode)
    (lsp-deferred)
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))
  (add-hook 'go-mode-hook #'my--go-mode-hook))

(use-package gotest
  :straight t
  :after go-mode
  :bind (:map go-mode-map
         ("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c ." . go-test-current-test)
         ("C-c b" . go-run)))

(use-package flycheck-golangci-lint
  :straight t
  :after flycheck
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package go-impl
  :straight t
  :after go-mode
  :bind (:map go-mode-map
         ("C-c C-l" . go-impl)))

;;;; Rust
(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'")
  :hook (rust-mode . lsp))

(use-package toml-mode
  :straight t
  :mode ("\\.toml\\'"))

(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :straight t
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;; Java
;; NOTE: possibly dap-java needs to be byte-compiled separately, to account for the following snippet:
;; (eval-when-compile
;;   (require 'cl-lib))
;; (setq lsp-java-vmargs
;;       (list "-noverify"
;;             "-Xmx2G"
;;             "-XX:+UseG1GC"
;;             "-XX:+UseStringDeduplication"
;;             (concat "-javaagent:" jmi/lombok-jar)
;;             (concat "-Xbootclasspath/a:" jmi/lombok-jar))
;;       lsp-file-watch-ignored
;;       '(".idea" ".ensime_cache" ".eunit" "node_modules"
;;         ".git" ".hg" ".fslckout" "_FOSSIL_"
;;         ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
;;         "build")

;;       lsp-java-import-order '["" "java" "javax" "#"]
;;       ;; Don't organize imports on save
;;       lsp-java-save-action-organize-imports nil

;;       ;; Formatter profile
;;       lsp-java-format-settings-url
;;       (concat "file://" jmi/java-format-settings-file))
(use-package dap-mode
  :straight t
  :functions (dap-breakpoint-toggle
              dap-debug
              dap-eval
              dap-eval-region
              dap-eval-thing-atp-point
              dap-step-in
              dap-step-out
              dap-next
              dap-continue)
  :after lsp-mode)

(defun dap:set-local-keybindings ()
  (local-set-key (kbd "C-; b") #'dap-breakpoint-toggle)
  (local-set-key (kbd "C-; e") #'dap-eval-thing-at-point)
  (local-set-key (kbd "C-; n") #'dap-next)
  (local-set-key (kbd "C-; c") #'dap-continue)
  (local-set-key (kbd "C-; i") #'dap-step-in)
  (local-set-key (kbd "C-; o") #'dap-step-out))

(use-package lsp-java
  :straight t
  :after lsp-mode
  :config
  (setq lsp-java-workspace-dir (expand-file-name "workspace" lsp-java-server-install-dir)
        lsp-java-save-action-organize-imports nil))

(use-package dap-java
  :straight nil
  :after (lsp-java dap-mode))

(defun my--java-mode-hook ()
  (setq lsp-prefer-flymake nil)
  (lsp-deferred)
  (company:add-local-backend 'company-lsp)
  (dap-mode t)
  (dap-ui-mode t)
  (local-set-key (kbd "C-; i") #'lsp-java-organize-imports)
  (local-set-key (kbd "C-; m") #'lsp-java-extract-method)
  (local-set-key (kbd "C-; v") #'lsp-java-extract-to-local-variable)
  (local-set-key (kbd "C-; c") #'lsp-java-extract-to-constant)
  (dap:set-local-keybindings))

(add-hook 'java-mode-hook 'my--java-mode-hook)

;;;; Python
;; System dependencies for the following packages: jedi flake8 autopep8 yap
(add-hook 'python-mode-hook #'subword-mode)

(use-package lsp-python-ms
  :straight t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))

;;;; Ruby
(use-package ruby-mode
  :straight t
  :mode ("\\.rake\\'"
         "Rakefile\\'"
         "\\.gemspec\\'"
         "\\.ru\\'"
         "Gemfile\\'"
         "Guardfile\\'"
         "Capfile\\'"
         "\\.cap\\'"
         "\\.thor\\'"
         "\\.rabl\\'"
         "Thorfile\\'"
         "Vagrantfile\\'"
         "\\.jbuilder\\'"
         "Podfile\\'"
         "\\.podspec\\'"
         "Puppetfile\\'"
         "Berksfile\\'"
         "Appraisals\\'")
  :interpreter "ruby"
  :config
  (add-hook 'ruby-mode-hook #'subword-mode))

(use-package yari
  :straight t
  :defer t)

(use-package inf-ruby
  :straight t
  :bind (:map inf-ruby-minor-mode-map
         ("C-x C-e" . ruby-send-last-sexp))
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-tools
  :straight t
  :hook (ruby-mode . ruby-tools-mode))

(use-package robe
  :straight t
  :hook (ruby-mode . robe-mode)
  :config
  (add-hook 'robe-mode (lambda () (company:add-local-backend 'company-robe))))

(use-package rubocop
  :straight t
  :hook (ruby-mode . rubocop-mode))

(use-package feature-mode
  :straight t
  :mode (("\\.feature$" . feature-mode)))

;;;; (X)HTML & CSS
(use-package web-mode
  :straight t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.tpl\\'"
         "\\.blade\\.php\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.html?\\'"
         "\\.eex\\'"
         "\\.vue\\'"
         "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'")
  :init
  (defun my/web-mode-hook ()
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2))
  :config
  (setq web-mode-enable-auto-pairing nil
        web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook #'my/web-mode-hook)
  (eval-after-load 'smartparens
    (sp-with-modes '(web-mode)
      (sp-local-pair "%" "%"
                     :unless '(sp-in-string-p)
                     :post-handlers '(((lambda (&rest _ignored)
                                         (just-one-space)
                                         (save-excursion (insert " ")))
                                       "SPC" "=" "#")))
      (sp-local-tag "%" "<% "  " %>")
      (sp-local-tag "=" "<%= " " %>")
      (sp-local-tag "#" "<%# " " %>"))))

(use-package prettier-js
  :straight t
  ;; :ensure-system-package (prettier . "npm i -g prettier")
  :commands prettier-js
  :init
  (defun my/prettier-js-hook ()
    (setq-local indent-region-function #'prettier-js)
    (prettier-js-mode))
  :hook ((typescript-mode js2-mode) . my/prettier-js-hook))

(use-package emmet-mode
  :straight t
  :bind (:map emmet-mode-keymap
         ("TAB" . emmet-expand-line))
  :hook ((web-mode sgml-mode css-mode) . emmet-mode))

(use-package css-mode
  :straight t
  :mode ("\\.[s]?css\\'")
  :config
  (setq css-indent-offset 2))

;; Pretty colours for css-mode
(use-package rainbow-mode
  :straight t
  :after css-mode
  :hook css-mode)

;;;; JavaScript
(use-package js2-mode
  :straight t
  :mode ("\\.js\\'"
         "\\.pac\\'")
  :interpreter "node"
  :config
  (defun my--js2-mode-hook ()
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2"
          js-indent-level 2))
  (add-hook 'js2-mode-hook 'my--js2-mode-hook)
  (js2-imenu-extras-mode +1))

(use-package tern
  :straight t
  :blackout
  :hook (js2-mode . tern-mode))

(use-package json-mode
  :straight t
  :mode ("\\.json\\'")
  :config
  (add-hook 'json-mode-hook (lambda ()
                              (make-local-variable 'js-indent-level)
                              (setq js-indent-level 2)))
  (defun json-to-single-line (beg end)
    "Collapse prettified json in region between BEG and END to a single line"
    (interactive "r")
    (if (use-region-p)
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-min))
            (while (re-search-forward "\\s-+\\|\n" nil t)
              (replace-match " "))))
      (print "This function operates on a region"))))

;;;; Typescript
(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'")

(use-package tide
  :straight t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :bind (:map tide-mode-map
         ([remap xref-find-definition] . tide-goto-definition)
         ([remap xref-find-references] . tide-references)
         ([remap describe-thing-at-point] . tide-documentation-at-point)
         ("C-; i" . tide-organize-imports)
         ("C-; f" . tide-fix))
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (defun my/setup-tsx-mode ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))
  :config
  (setq typescript-indent-level 2))

;;;; Markdown
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc")
  '(mapc
    (lambda (face)
      (set-face-attribute
       face nil
       :inherit
       (my-adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'markdown-pre-face 'markdown-inline-code-face)))

(use-package edit-indirect
  :after markdown-mode
  :straight t)

(use-package pandoc-mode
  :straight t
  :hook markdown-mode
  :config
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  ;; We don't need pandoc-mode in github-flavored .md files
  (add-hook 'gfm-mode-hook (lambda () (pandoc-mode -1))))

;; Yet Another Markup Language
(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'")

;;;; REST
(use-package restclient
  :straight t
  :mode (("\\.rest\\'" . restclient-mode)))

(use-package company-restclient
  :straight t
  :after restclient
  :config
  (add-hook 'restclient-mode-hook (lambda () (company:add-local-backend 'company-restclient))))

(use-package haskell-mode
  :straight t
  :mode (("\\.hs\\'" . haskell-mode))
  :config
  (setq haskell-stylish-on-save t))

(use-package terraform-mode
  :straight t)

(use-package ereader
  :straight t
  :mode ("\\.epub\\'" . ereader-mode))

;; THEMES
;; (use-package modus-themes
;;   :straight (modus-themes
;;              :host github :type git
;;              :repo "protesilaos/modus-themes")
;;   :custom
;;   (modus-operandi-theme-distinct-org-blocks t)
;;   (modus-operandi-theme-slanted-constructs t))

(use-package nano
  :straight (nano-emacs
             :type git :host github
             :repo "rougier/nano-emacs")
  :no-require t
  :custom
  (nano-font-family-proportional (face-attribute 'variable-pitch :family))
  (nano-font-family-monospaced (face-attribute 'default :family))
  :config
  (require 'nano-base-colors)
  (require 'nano-faces)
  (nano-faces)
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil)
  (require 'disp-table)
  ;; Fix bug on OSX in term mode & zsh (spurious % after each command)
  (add-hook 'term-mode-hook
	        (lambda () (setq buffer-display-table (make-display-table))))
  (require 'nano-colors))

;; Make sure new frames use window-divider
;; Make a clean & minimalist frame
(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-right-width 12)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t)
  :config
  (setq-default default-frame-alist
                (append (list
                         '(min-height . 1)
                         '(height     . 45)
	                     '(min-width  . 1)
                         '(width      . 81)
                         '(vertical-scroll-bars . nil)
                         '(internal-border-width . 10)
                         '(left-fringe    . 1)
                         '(right-fringe   . 1)
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t))

(add-hook 'before-make-frame-hook 'window-divider-mode)

(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t
        bespoke-set-variable-pitch t)
  (bespoke/light-theme)
  (set-face-attribute 'region nil :inherit nil :background "yellow"))

(use-package bespoke-modeline
  :straight (:type git :host github :repo "mclear-tools/bespoke-modeline")
  :init
  (setq bespoke-modeline-position 'top)
  (setq bespoke-modeline-size 2)
  (setq bespoke-modeline-git-diff-mode-line t)
  (setq bespoke-modeline-cleaner t)
  (setq bespoke-modeline-visual-bell t)
  :config
  (bespoke-modeline-mode))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

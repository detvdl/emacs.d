;; -*- lexical-binding; t -*-

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			             ("melpa" . "https://melpa.org/packages/")
			             ("org" . "http://orgmode.org/elpa/")))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package--init-file-ensured t
      package-check-signature nil)

(when (version< emacs-version "27")
  (setq package-enable-at-startup nil)
  (unless package--initialized
    (package-initialize)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

(use-package delight :ensure t)
(use-package bind-key :ensure t)

(setq use-package-always-ensure nil)

(defconst emacs-misc-dir (expand-file-name "misc" user-emacs-directory))
(defconst emacs-theme-dir (expand-file-name "themes" user-emacs-directory))

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(when *is-mac*
  (setq mac-command-modifier 'meta
        mac-option-modifier 'alt)
  (global-set-key [kp-delete] 'delete-char))

(push emacs-theme-dir custom-theme-load-path)
(dolist (dir (directory-files emacs-theme-dir))
  (let ((dirpath (expand-file-name dir emacs-theme-dir)))
    (unless (or (member dir '("." ".." ".git"))
		        (not (file-directory-p dirpath)))
      (push dirpath custom-theme-load-path))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Source environment variables from init shell on non-shell based init systems
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (setq exec-path-from-shell-variables
        '("HOME" "PATH" "MANPATH"
          "PAGER" "TERM"
          "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
          "LANGUAGE" "LANG" "LC_CTYPE" "LC_ALL"
          "LOMBOK_JAR"
          "GOPATH" "GOROOT")
        exec-path-from-shell-arguments '("-l" "-i"))
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

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

(when (and (display-graphic-p) *is-linux*)
  (setq x-gtk-use-system-tooltips nil))

;; Zoom in and out using text-scale commands
(bind-key "C--" #'text-scale-decrease global-map)
(bind-key "C-+" #'text-scale-increase global-map)

(defvar font-height (face-attribute 'default :height))
;; (set-face-attribute 'default nil :family "Fira Code" :height 140 :weight 'light)
;; (set-frame-font "Fira Code-14:light")
(setq inhibit-compacting-font-caches t)


(defconst git--state-small-dot
  "/* XPM */
static char * data[] = {
\"14 7 3 1\",
\" 	c None\",
\"+	c #202020\",
\".	c %s\",
\"      +++     \",
\"     +...+    \",
\"    +.....+   \",
\"    +.....+   \",
\"    +.....+   \",
\"     +...+    \",
\"      +++     \"};")

(defconst git--state-large-dot
  "/* XPM */
static char * data[] = {
\"18 13 3 1\",
\" 	c None\",
\"+	c #000000\",
\".	c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};")

(defun git--state-color (state)
  "Return an appropriate color string for the given Git STATE."
  (cond ((eq state 'edited) "green")
        ((eq state 'added) "blue")
        ((memq state '(removed conflict unregistered)) "red")
        ((memq state '(needs-update needs-merge)) "purple")
        ((eq state 'up-to-date) "yellow")
        ((eq state 'staged) "yellow")
        ((memq state '(ignored unknown)) "gray50")
        (t "gray50")))

(defun git--state-dot (&optional state)
  "Return the appropriate bitmap dot for the given Git STATE."
  (let* ((backend (vc-backend buffer-file-name))
         (state (or state (if (and backend buffer-file-name)
                              (vc-state buffer-file-name backend)
                            'unknown)))
         (color (git--state-color state)))
    (propertize "   "
                'help-echo (format "VC state: %s" state)
                'display
                `(image :type xpm
                        :data ,(format git--state-large-dot color)
                        :ascent center))))

(setq-default mode-line-format
	          '("%e"
		        mode-line-front-space
		        (:eval (git--state-dot))
		        mode-line-mule-info
		        mode-line-client
		        mode-line-modified
		        mode-line-remote
		        mode-line-frame-identification
		        mode-line-buffer-identification
		        "   "
		        mode-line-position
		        (vc-mode vc-mode)
		        "  "
		        mode-line-modes
		        mode-line-misc-info
		        mode-line-end-spaces))

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
	          visible-bell nil)

(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

;; Show me which line I'm on.
(global-hl-line-mode +1)

;; Proper line wrapping.
(global-visual-line-mode +1)
(delight 'visual-line-mode nil t)

;; Uniquify buffers with the same name instead of appending a number.
(setq uniquify-buffer-name-style 'forward
      uniquify-separator " . "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;;; Buffers
(bind-key "C-x C-b" #'ibuffer global-map)

;;;; Dired
(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
    ("i" . dired-subtree-insert)
    (";" . dired-subtree-remove)))

;;;; Undo/Redo
(use-package undo-tree
  :ensure t
  :defer 1
  :delight
  :config
  (global-undo-tree-mode +1))

;;;; Auto-revert
;; Automatically revert buffers that have changed on disk
(auto-revert-mode +1)
(delight 'auto-revert-mode nil t)

;;;; Clipboard
(setq select-enable-clipboard t)
(setq select-active-regions t)
(setq save-interprogram-paste-before-kill 1)
(setq yank-pop-change-selection t)

(use-package ace-window
  :ensure t
  :delight ace-window-mode
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
  :ensure t
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

;; Helper functions to improve some emacs basics.
(use-package crux
  :ensure t
  :bind (([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards))
  :config
  (crux-reopen-as-root-mode))

;; Handy-dandy menu in case you ever forget a keybind.
(use-package which-key
  :ensure t
  :delight which-key-mode
  :config
  (which-key-mode))

;; Quickly select expanding regions and put them in the kill-ring.
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))


(use-package hydra
  :ensure t)

(use-package iedit
  :ensure t)

;; Does what it says: multiple cursors!
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)
         ("C-M->" . mc/unmark-next-like-this)
         ("C-. C->" . mc/skip-to-next-like-this)
         ("C-. C-<" . mc/skip-to-previous-like-this)
         ("C-. >" . hydra-multiple-cursors/body)
         :map global-map
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :init
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
    ("q" nil))
  (setq mc/list-file (expand-file-name ".mc-lists.el" emacs-misc-dir)))

;;;; Ivy
(use-package ivy
  :ensure t
  :delight ivy-mode
  :bind (("C-s" . swiper-isearch)
         ("C-x C-f" . counsel-find-file)
	     ("M-x" . counsel-M-x)
	     ("M-X" . smex-major-mode-commands)
         ("C-c y" . counsel-yank-pop)
         ;; TODO: investigate https://github.com/Wilfred/deadgrep as a possible substitute/enrichment
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x 8" . counsel-unicode-char)
         ("C-x b" . ivy-switch-buffer)
	     ("C-c C-r" . ivy-resume)
         ("C-c C-u" . swiper-all)
         :map ivy-occur-mode-map
         ("w" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-m" . ivy-alt-done)
         ("C-j" . ivy-done))
  :config
  ;; Fuzzy matching
  (use-package flx :ensure t)
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        enable-recursive-minibuffers t
        ivy-display-style 'fancy
        ivy-height 8
        ivy-virtual-abbreviate 'full
        ivy-extra-directories nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (counsel-ag-function . ivy--regex-plus)
                                (counsel-grep-function . ivy--regex-plus)
                                (swiper-all . ivy--regex-plus)
                                (swiper-isearch . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (use-package smex
    :ensure t
    :config
    (setq smex-save-file (expand-file-name "smex-items" emacs-misc-dir)))
  ;; use the faster ripgrep for standard counsel-grep
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")
  (ivy-mode 1))

(use-package swiper :ensure t :after ivy)
(use-package counsel :ensure t
  :after swiper
  :config
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\'"))

(use-package ggtags
  :ensure t
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  ;; ensure ace-window keybind doesn't get overridden in ggtags-mode buffers
  (unbind-key "M-o" ggtags-navigation-map))

(use-package imenu-list
  :ensure t
  :bind ("C-'" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil
        imenu-list-size 0.25
        imenu-list-position 'right))

;;;; Symbol Highlighting
(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode))

;;;; Dired
(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
         ("i" . dired-subtree-insert)
         (";" . dired-subtree-remove)))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
  (when (version<= "9.2" (org-version))
    (require 'org-tempo))
  (setq org-log-done t
        org-startup-indented t
        org-hide-leading-stars t
        org-hidden-keywords '()
        ;; LaTeX preview size is a bit too small for comfort
        org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
        org-highlight-latex-and-related '(latex))
  ;; I *kinda* like distinctive header sizes
  (custom-set-faces
   '(org-document-title ((t (:inherit outline-1 :height 1.30 :underline t))))
   '(org-document-info ((t (:inherit outline-1 :height 1.20))))
   '(org-document-info-keyword ((t (:inherit outline-1 :height 1.20))))
   '(org-warning ((t (:weight bold :foreground "#CC9393" :height 1.20))))

   '(org-level-1 ((t (:inherit outline-1 :height 1.05))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.00))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.00))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.00))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.00)))))
  (add-hook 'org-mode-hook (lambda ()
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" .  "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (prettify-symbols-mode)))
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  (delight 'org-indent-mode nil t))

;; Org-mode buffer-local variables
(put 'org-src-preserve-indentation 'safe-local-variable (lambda (val) #'booleanp))

;;;; Look & feel
;; Prettifying org-mode buffers.
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("●"
                                  "○")))

(use-package adaptive-wrap
  :ensure t
  :hook ((prog-mode org-mode) . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 2))

;; Make the whole heading line fontified
(setq org-fontify-whole-heading-line t)

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
  :ensure t
  :delight smartparens-mode
  :hook ((prolog-mode prog-mode ess-mode slime-mode slime-repl-mode) . smartparens-mode)
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
  :ensure t
  :delight projectile-mode
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file))
  :config
  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid)
  (with-eval-after-load 'ivy
    (ivy-set-actions 'projectile-find-file
                     '(("j" find-file-other-window "other window")))
    (ivy-set-actions 'projectile-switch-project
                     '(("g" magit-status "magit status"))))
  (projectile-mode))

;;;; Rainbows
(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode slime-mode) . rainbow-delimiters-mode))

;;;; Movement
(use-package avy
  :ensure t
  :bind (("M-n l" . avy-goto-line)
         ("M-n c" . avy-goto-char)
         ("M-n f" . avy-goto-char-2)
         ("M-n w" . avy-goto-word-1))
  :config
  (avy-setup-default))

(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-scale-factor 1.0))

;;; Treemacs
(use-package treemacs
  :ensure t
  :bind (("M-0" . treemacs-select-window)
         ("M-'" . treemacs)
         :map treemacs-mode-map
         ([mouse-1] . treemacs-single-click-expand-action))
  :config
  (setq treemacs-fringe-indicator-mode nil
        treemacs-no-png-images nil
        treemacs-width 30
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-show-hidden-files t
        treemacs-file-event-delay 1000
        treemacs-file-follow-delay 0.1)
  ;; (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)
  (add-hook 'treemacs-mode-hook (lambda ()
                                  (linum-mode -1)
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
         :extensions (fallback))))
    )
  (treemacs-load-theme "doom"))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; TODO: set correct faces to properly use solaire-mode
(use-package solaire-mode
  :ensure t
  :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (solaire-global-mode)
  (solaire-mode-swap-bg))

;;; Centaur tabs
(use-package centaur-tabs
  :ensure t
  :bind (("C-S-<tab>" . centaur-tabs-backward)
         ("C-<tab>" . centaur-tabs-forward))
  :init
  (setq centaur-tabs-set-bar 'over)
  :config
  (centaur-tabs-mode)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "●"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-height 20
        centaur-tabs-set-icons nil
        centaur-tabs-gray-out-icons 'buffer))

;; Always enable eldoc
(global-eldoc-mode +1)
(delight 'eldoc-mode nil t)

;;;; Error checking
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

;;;; Indentation
;; Aggressively indent everything (except for basically all non-lisp modes)!
(use-package aggressive-indent
  :ensure t
  :delight
  :hook ((lisp-mode lisp-interaction-mode emacs-lisp-mode clojure-mode) . aggressive-indent-mode)
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
(bind-key "C-c i l" #'indent-whole-file global-map)

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
  :ensure t
  :delight yas-minor-mode
  :commands (yas-reload-all yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;;; Company
(use-package company
  :ensure t
  :delight company-mode
  :bind (("M-\\" . company-select-next))
  :hook ((org-mode prog-mode) . company-mode)
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (use-package pos-tip :ensure t)
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
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-eldoc-enable-hover t
        lsp-eldoc-render-all t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap describe-thing-at-point] . lsp-describe-thing-at-point))
  :config
  (setq lsp-ui-doc-include-signature nil
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-sideline-update-mode 'line))

(use-package company-lsp
  :ensure t
  :after (lsp-mode company))

;;;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        vc-follow-symlinks t)
  (use-package other-frame-window
    :ensure t
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

(use-package magit-todos
  :ensure t
  :after magit
  :demand t
  :bind (:map magit-status-mode-map
         ("j l" . magit-todos-list))
  :config
  (magit-todos-mode +1)
  (setq magit-todos-auto-group-items 5))

;;;; Git Diff
;; Visual diff feedback in the margin/gutter
(use-package diff-hl
  :ensure t
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
  :ensure t
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;;; Emacs Lisp
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :commands (elisp-slime-nav-mode)
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook (lambda () (company:add-local-backend 'company-elisp)))

;;;; Common Lisp
;; the SBCL configuration file is written in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(use-package slime
  :ensure t
  :commands slime
  :bind (:map slime-mode-map
         ("C-c C-s" . slime-selector))
  :config
  (setq slime-lisp-implementations '((ccl ("/usr/local/bin/ccl"))
                                     (sbcl ("/usr/local/bin/sbcl"))
                                     (pico ("/usr/local/bin/pil")))
        slime-contribs '(slime-fancy slime-company slime-indentation)
        slime-autodoc-use-multiline-p t
        slime-enable-evaluate-in-emacs t
        common-lisp-style-default "sbcl")
  (defun slime-enable-concurrent-hints ()
    (interactive)
    (setf slime-inhibit-pipelining nil)))

(use-package slime-company
  :ensure t
  :after slime
  :config
  (add-hook 'slime-mode-hook (lambda () (company:add-local-backend 'company-slime)))
  (setq slime-company-completion 'fuzzy))

;;;; Clojure
(use-package clojure-mode
  :ensure t
  :mode ("\\.clj[xc]?\\'"
         "build\\.boot\\'")
  :config
  (add-hook 'clojure-mode-hook #'subword-mode))

(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojurescript-mode))

(use-package cider
  :ensure t
  :defer t
  :commands (cider-jack-in-clj&cljs
             cider-jack-in
             cider-jack-in-cljs
             cider-connect
             cider-connect-cljs)
  :config
  (progn
    (setq nrepl-log-messages t
          ;; cider-boot-parameters "dev"
          cider-eldoc-display-context-dependent-info t
          cider-eldoc-display-for-symbol-at-point t
          cider-dynamic-indentation nil)
    (add-hook 'cider-mode-hook #'subword-mode)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-repl-mode-hook #'eldoc-mode)))

(use-package clojure-snippets
  :ensure t
  :after clojure-mode
  :config
  (with-eval-after-load 'yasnippet
    (clojure-snippets-initialize)))

(use-package clj-refactor
  :ensure t
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
  :ensure t
  :commands run-geiser
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
  :ensure t
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
  :ensure t
  :mode "\\.go\\'"
  :bind (:map go-mode-map
         ("C-c c" . compile)
         ("C-c r" . recompile))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun my--go-mode-hook ()
    (setq-local indent-tabs-mode 1)
    (setq-local tab-width 2)
    (subword-mode +1)
    (yas-minor-mode)
    ;; (lsp-deferred) ;; WARNING: for this to work with `bingo', set `$GOROOT' correctly
    ;; (company:add-local-backend 'company-lsp)
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))
  (add-hook 'go-mode-hook #'my--go-mode-hook))

(use-package gotest
  :ensure t
  :after go-mode
  :bind (:map go-mode-map
         ("C-c a" . go-test-current-project)
         ("C-c m" . go-test-current-file)
         ("C-c ." . go-test-current-test)
         ("C-c b" . go-run)))

(use-package flycheck-golangci-lint
  :ensure t
  :after flycheck
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package go-impl
  :ensure t
  :after go-mode
  :bind (:map go-mode-map
         ("C-c C-l" . go-impl)))

;;;; Rust
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'")
  :config
  (defun my--rust-mode-hook ()
    (lsp)
    (company:add-local-backend 'company-lsp))
  (add-hook 'rust-mode-hook #'my--rust-mode-hook))

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'"))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;; Java
;; NOTE: possibly dap-java needs to be byte-compiled separately, to account for the following snippet:
;; (eval-when-compile
;;   (require 'cl))
(use-package dap-mode
  :ensure t
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
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-java-workspace-dir (expand-file-name "workspace" lsp-java-server-install-dir)
        lsp-java-save-action-organize-imports nil))

(use-package dap-java
  :after (lsp-java dap-mode))

(defun my--java-mode-hook ()
  (setq lsp-prefer-flymake nil)
  (lsp)
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

(use-package anaconda-mode
  :ensure t
  :hook python-mode
  :bind (:map anaconda-mode-map
         ([remap xref-find-definitions] . anaconda-mode-find-definitions)
         ([remap xref-find-references] . anaconda-mode-find-references)
         ([remap describe-thing-at-point] . anaconda-mode-show-doc))
  :config
  (setq python-shell-interpreter "python3")
  (setq anaconda-mode-installation-directory (expand-file-name "anaconda-mode" emacs-misc-dir))
  (when *is-mac*
    (setq anaconda-mode-localhost-address "localhost")))

(use-package company-anaconda
  :ensure t
  :after anaconda-mode
  :config
  (add-hook 'anaconda-mode-hook
            (lambda ()
              (company:add-local-backend 'company-anaconda)
              (anaconda-eldoc-mode))))

(use-package pyenv-mode
  :ensure t
  :hook python-mode
  :config
  (progn
    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name."
      (let ((project (projectile-project-name)))
        (if (member project (pyenv-mode-versions))
            (pyenv-mode-set project)
          (pyenv-mode-unset))))
    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)))

;;;; Ruby
(use-package ruby-mode
  :ensure t
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
  :ensure t
  :defer t)

(use-package inf-ruby
  :ensure t
  :bind (:map inf-ruby-minor-mode-map
         ("C-x C-e" . ruby-send-last-sexp))
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package ruby-tools
  :ensure t
  :hook (ruby-mode . ruby-tools-mode))

(use-package rbenv
  :ensure t
  :defer t
  :config
  (global-rbenv-mode)
  (rbenv-use-corresponding))

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode)
  :config
  (add-hook 'robe-mode (lambda () (company:add-local-backend 'company-robe))))

(use-package rubocop
  :ensure t
  :hook (ruby-mode . rubocop-mode))

(use-package feature-mode
  :ensure t
  :mode (("\\.feature$" . feature-mode)))

;;;; Perl6
(use-package perl6-mode
  :ensure t
  :defer t)

(use-package flycheck-perl6
  :ensure t
  :after flycheck perl6-mode)

;;;; (X)HTML & CSS
(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.tpl\\'"
         "\\.blade\\.php\\'"
         "\\.jsp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.html?\\'"
         "\\.eex\\'"
         "/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'")
  :config
  (setq web-mode-enable-auto-pairing nil
        web-mode-enable-current-element-highlight t)
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

(use-package emmet-mode
  :ensure t
  :bind (:map emmet-mode-keymap
         ("TAB" . emmet-expand-line))
  :hook ((web-mode sgml-mode css-mode) . emmet-mode))

(use-package css-mode
  :ensure t
  :mode ("\\.[s]?css\\'")
  :config
  (setq css-indent-offset 2))

;; Pretty colours for css-mode
(use-package rainbow-mode
  :ensure t
  :after css-mode
  :hook css-mode)

;;;; JavaScript
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'"
         "\\.pac\\'")
  :interpreter "node"
  :config
  (defun my--js2-mode-hook ()
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2"
          js-indent-level 4))
  (add-hook 'js2-mode-hook 'my--js2-mode-hook)
  (js2-imenu-extras-mode +1))

(use-package tern
  :ensure t
  :delight
  :hook js2-mode)

(use-package company-tern
  :ensure t
  :after tern
  :config
  (add-hook 'js2-mode-hook (lambda () (company:add-local-backend 'company-tern))))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'"))

;;;; Typescript
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(use-package tide
  :ensure t
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
  :config
  (setq typescript-indent-level 4))

;;;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))

(use-package pandoc-mode
  :ensure t
  :hook markdown-mode
  :config
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  ;; We don't need pandoc-mode in github-flavored .md files
  (add-hook 'gfm-mode-hook (lambda () (pandoc-mode -1))))

;; Yet Another Markup Language
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

;;;; REST
(use-package restclient
  :ensure t
  :mode (("\\.rest\\'" . restclient-mode)))

(use-package company-restclient
  :ensure t
  :after restclient
  :config
  (add-hook 'restclient-mode-hook (lambda () (company:add-local-backend 'company-restclient))))

(use-package ob-restclient
  :after org
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-stylish-on-save t))

(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package zenburn-theme
  :ensure t)

(load-theme 'zenburn t)
;; (load-theme 'default-dark t)

;;; Package archives and setup
;;;; Package archives
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			             ("melpa" . "https://melpa.org/packages/")
			             ("org" . "http://orgmode.org/elpa/")))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-enable-at-startup nil
      package--init-file-ensured t
      package-check-signature nil)

;;;; Use-package
;; Install use-package and its sub-packages/necessary packages for some of its extra features
(require 'package)

(unless package--initialized
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure nil)

(use-package delight :ensure t)
(use-package bind-key :ensure t)

;;; Environment
;;;; Custom Variables
;; Constants
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *pretty-mode* t)
(defconst *line-numbers-on* -1)

;; Directories
(defconst emacs-misc-dir (expand-file-name "misc" user-emacs-directory))
(defconst emacs-theme-dir (expand-file-name "themes" user-emacs-directory))

;;; MacOS specifics
;;;; Modifiers
;; Switch up modifier keys to fit the MacOS keyboard layout better
(when *is-mac*
  (setq mac-command-modifier 'meta
        mac-option-modifier 'alt)
  (global-set-key [kp-delete] 'delete-char))

;;;; Looks
;; Prettify Emacs appearance to match colour scheme
(when (and *is-mac* *pretty-mode*)
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t)))


;;; Paths & Custom files
;;;; Load path initialization
(push emacs-theme-dir custom-theme-load-path)
(dolist (dir (directory-files emacs-theme-dir))
  (let ((dirpath (expand-file-name dir emacs-theme-dir)))
    (unless (or (member dir '("." ".." ".git"))
		        (not (file-directory-p dirpath)))
      (push dirpath custom-theme-load-path))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Shell
;;;; Environment variables
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH"
          "PAGER" "TERM"
          "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
          "LANGUAGE" "LANG" "LC_CTYPE" "LC_ALL"
          "JDT_SERVER" "JDT_SERVER_CONFIG" "JDT_SERVER_DATA" "LOMBOK_JAR"
          "GOPATH"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; User Interface
;;;; GUI
;; Remove unnecessary cruft from the GUI application (and then add line-number cruft)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode 1)

(global-display-line-numbers-mode *line-numbers-on*)
(setq-default display-line-numbers-width 4
              display-line-numbers-current-absolute t
              display-line-numbers-widen t)

(column-number-mode)

(setq inhibit-splash-screen nil)
(setq ring-bell-function 'ignore)

(setq-default indicate-empty-lines t)

(setq frame-resize-pixelwise t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq x-gtk-use-system-tooltips nil)

;; Zoom in and out using text-scale commands
(bind-key "C--" #'text-scale-decrease global-map)
(bind-key "C-+" #'text-scale-increase global-map)

;;;; Fonts
(defconst font-height 140)
(defconst font-size (/ font-height 10))
(defconst font-weight 'regular)
(defconst font-family "Go Mono")
(defconst font-string (format "%s-%s:%s" font-family font-size font-weight))

(defconst Go-font `(:family ,font-family :height ,font-height :weight ,font-weight))
(defconst Baskerville-font `(:family "Baskerville" :height ,font-height))

(set-frame-font font-string nil t)
(apply 'set-face-attribute `(default nil ,@Go-font))
(apply 'set-face-attribute `(fixed-pitch nil ,@Go-font))
(apply 'set-face-attribute `(line-number nil ,@Go-font))
(apply 'set-face-attribute `(variable-pitch nil ,@Baskerville-font))

;; (add-hook 'text-mode-hook #'variable-pitch-mode)

(setq line-spacing 0.1)

;;;; Theme
(use-package poet-theme
  :ensure t
  :defer t
  :config
  (custom-theme-set-faces
   'poet
   '(org-level-1 ((nil ((:weight bold :overline "#A7A7A7")))))
   '(outline-1 ((nil (:overline "#A7A7A7"))))))

(use-package zenburn-theme
  :ensure t
  :defer t
  :custom
  (zenburn-override-colors-alist '(("zenburn-bg" . "#222222")
                                   ("zenburn-bg+1" . "#323232"))))

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'zenburn t)

(setq-default left-margin-width 2)
(setq-default fringes-outside-margins t)
(defun refresh-new-frame-buffer (frame)
  "Refresh all windows in the given FRAME.
Doing this allows the `fringes-outside-margins' setting to take effect."
  (mapcar (lambda (w)
            (set-window-buffer w (window-buffer w)))
          (cons (minibuffer-window frame) (window-list frame))))
(add-to-list 'after-make-frame-functions #'refresh-new-frame-buffer)

;;;; Modeline
(which-function-mode)

;;; Editor
;;;; General
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Never type "yes" or "no" again.
(fset 'yes-or-no-p 'y-or-n-p)

;; Always delete selection when typing over or pasting
(delete-selection-mode +1)

;; Remove trailing whitespace when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Automatically create missing parent directories when visiting a new file.
(defun detvdl/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory '%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'detvdl/create-non-existent-directory)

;; Some generic variables.
(setq-default tab-width 4
	          make-backup-files nil
	          indent-tabs-mode nil
	          show-trailing-whitespace t
	          visible-bell nil)

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

;;;; Undo/Redo
(use-package undo-tree
  :ensure t
  :delight
  :bind (("C-/" . undo)
         ("C-?" . undo-tree-redo))
  :config
  (global-undo-tree-mode +1))

;;;; Auto-revert
;; Automatically revert buffers that have changed on disk
(auto-revert-mode +1)
(delight 'auto-revert-mode nil t)

;;;; Clipboard
(when *is-linux*
  (setq x-select-enable-clipboard t))
(setq select-active-regions t)
(setq save-interprogram-paste-before-kill 1)
(setq yank-pop-change-selection t)

;;;; General Utilities
;; Easily select regions.
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-. e" . er/expand-region)))

;; Quickly switch windows, with visual help
(use-package ace-window
  :ensure t
  :delight ace-window-mode
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?r ?s ?d ?h ?n ?e ?i ?o)
        aw-scope 'frame
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

;; Great regex-based find-and-replace.
(use-package anzu
  :ensure t
  :bind (("C-, r" . anzu-query-replace)
         ("C-, R" . anzu-query-replace-regexp)))

;; Quickly select expanding regions and put them in the kill-ring.
(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

;; Does what it says: multiple cursors!
(use-package multiple-cursors
  :ensure t
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
  (setq mc/list-file (expand-file-name ".mc-lists.el" emacs-misc-dir)))

;;;; Writeroom (zen-mode)
(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode
  :bind ("C-c w" . writeroom-mode)
  :config
  (defun writeroom-disable-line-numbers (arg)
    (cond
     ((= arg 1) (display-line-numbers-mode -1))
     ((= arg -1) (when (= *line-numbers-on* +1)
                   (display-line-numbers-mode +1)))))
  (setq writeroom-fringes-outside-margins t
        writeroom-fullscreen-effect 'maximized
        writeroom-width 120)
  (add-to-list 'writeroom-global-effects 'writeroom-disable-line-numbers))

;;;; Ivy
(use-package ivy
  :ensure t
  :delight ivy-mode
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-x C-f" . counsel-find-file)
	     ("M-x" . counsel-M-x)
	     ("M-X" . smex-major-mode-commands)
         ("C-c y" . counsel-yank-pop)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x 8" . counsel-unicode-char)
         ("C-x b" . ivy-switch-buffer)
	     ("C-c C-r" . ivy-resume)
         ("C-c C-u" . swiper-all)
         ("C-c C-w" . ivy-wgrep-change-to-wgrep-mode)
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
                                (swiper-all . ivy--regex-plus)
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

(use-package ivy-rich
  :ensure t
  :after ivy
  :init (setq ivy-rich-parse-remote-file-path t)
  :config (ivy-rich-mode 1))

;;;; Shackle
(use-package shackle
  :ensure t)
;;; Outlining
;;;; Outshine
(use-package outshine
  :ensure t
  :delight
  :commands outshine-mode
  :hook (emacs-lisp-mode . outshine-mode)
  :bind (:map outshine-mode-map
         ("S-<tab>" . outshine-cycle-buffer)
         ("<backtab>" . outshine-cycle-buffer))
  :config
  (setq outshine-startup-folded-p nil))

;;; Org-mode
;;;; General
;; Install org from org-plus-contrib!
(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :config
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
   '(org-level-5 ((t (:inherit outline-5 :height 1.00))))
   )
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

(use-package leuven-theme
  :ensure t
  :defer t
  :config
  ;; Make sure hidden leading stars are actually invisible in my themes
  (custom-theme-set-faces
   'leuven
   '(org-hide ((t (:foreground "#FFFFFF"))))))

;;; Programming tools
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

;;;; Compilation mode
;; colorize the output of the compilation mode.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))

  ;; mocha seems to output some non-standard control characters that
  ;; aren't recognized by ansi-color-apply-on-region, so we'll
  ;; manually convert these into the newlines they should be.
  (goto-char (point-min))
  (while (re-search-forward "\\[2K\\[0G" nil t)
    (progn
      (replace-match "")))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;; Encoding & Transient buffers
(put 'encoding 'safe-local-variable (lambda (val) #'stringp))

;; Don't replace existing buffers with transient ones, keep them persistent.
(use-package dedicated
  :ensure t
  :commands dedicated-mode)

;;;; Smartparens
(use-package smartparens
  :ensure t
  :delight smartparens-mode
  :hook ((prolog-mode prog-mode ess-mode slime-mode slime-repl-mode) . smartparens-mode)
  :bind (("C-. )" . sp-rewrap-sexp)
         ("C-. (" . sp-rewrap-sexp))
  :config
  (require 'smartparens-config)
  ;; reserve this keybing for `xref-find-references'
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
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

;;;; Projectile
(use-package projectile
  :ensure t
  :delight projectile-mode
  :bind (("C-c p p" . projectile-switch-project)
         ("C-c p f" . projectile-find-file))
  :config
  (setq projectile-completion-system 'ivy)
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

;;;; Documentation
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
(defun detvdl/indent-file ()
  (interactive)
  (indent-region (point-min) (point-max)))
(bind-key "C-; l" #'detvdl/indent-file global-map)

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

;;;; Imenu/speedbar
(use-package sr-speedbar
  :ensure t
  :bind (("M-i" . my--sr-speedbar-toggle)
         ("M-n" . sr-speedbar-select-window)
         :map speedbar-mode-map
         ("q" . sr-speedbar-close)
         ("b" . sr-speedbar-buffers))
  :init
  (defun sr-speedbar-buffers ()
    (interactive)
    (speedbar-change-initial-expansion-list "buffers"))
  (defun my--sr-speedbar-toggle ()
    (interactive)
    (sr-speedbar-toggle)
    (when-let* ((buf (get-buffer sr-speedbar-buffer-name))
                (win (get-buffer-window buf)))
      (with-current-buffer buf
        (setq-local display-line-numbers nil)
        (setq-local left-fringe-width 0)
        (setq-local window-min-width 30)
        (setq window-size-fixed 'width)
        (set-window-buffer win buf))))
  :config
  (setq sr-speedbar-right-side nil
        speedbar-show-unknown-files t
        speedbar-indentation-width 2
        speedbar-use-images nil
        speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'" ;; show hidden files
        sr-speedbar-auto-refresh t
        sr-speedbar-max-width 40
        sr-speedbar-default-width 30
        sr-speedbar-width 30)
  (defconst speedbar--face-font-height 100)
  (set-face-attribute 'speedbar-button-face nil :height speedbar--face-font-height)
  (set-face-attribute 'speedbar-file-face nil :height speedbar--face-font-height)
  (set-face-attribute 'speedbar-directory-face nil :height speedbar--face-font-height)
  (set-face-attribute 'speedbar-tag-face nil :height speedbar--face-font-height)
  (set-face-attribute 'speedbar-selected-face nil :height speedbar--face-font-height))

(use-package imenu-list
  :ensure t
  :commands imenu-list
  :bind ("C-'" . imenu-list))

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

;;; Completion
;;;; Snippets
(use-package yasnippet
  :if (not noninteractive)
  :ensure t
  :delight
  :commands (yas-reload-all yas-minor-mode)
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;;; Company
(use-package company
  :ensure t
  :delight company-mode
  :bind (("M-\\" . company-select-next))
  :demand
  :config
  (setq company-idle-delay 0.5
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t)
  (global-company-mode 1))

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
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap describe-thing-at-point] . lsp-describe-thing-at-point))
  :config
  ;; Very Projectile-like way to handle short-file-names (sfn)
  ;; in lsp-ui-peek on a project-basis
  ;; Currently just copied functions from `projectile.el'
  ;; TODO: integrate with Projectile itself
  (defvar lsp-ui--sfn-projects-file
    (expand-file-name "lsp-ui--sfn.eld" user-emacs-directory))
  (defvar lsp-ui--sfn-projects nil)
  (defvar lsp-ui--sfn-projects-on-file nil)
  (defun lsp-ui--sfn-diff (list1 list2)
    (cl-remove-if
     (lambda (x) (member x list2))
     list1))
  (defun lsp-ui--sfn-serialize (data file)
    (when (file-writable-p file)
      (with-temp-file file
        (insert (let (print-length) (prin1-to-string data))))))
  (defun lsp-ui--sfn-deserialize (file)
    (with-demoted-errors
        "Error during file deserialization: %S"
      (when (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (read (buffer-string))))))
  (defun lsp-ui--sfn-load-projects ()
    (setq lsp-ui--sfn-projects (lsp-ui--sfn-deserialize lsp-ui--sfn-projects-file))
    (setq lsp-ui--sfn-projects-on-file
          (and (sequencep lsp-ui--sfn-projects)
               (copy-sequence lsp-ui--sfn-projects))))
  (defun lsp-ui--sfn-add-project (project-root)
    (interactive (list (read-directory-name "Add to sfn-projects: ")))
    (setq lsp-ui--sfn-projects
          (delete-dups
           (cons (file-name-as-directory (abbreviate-file-name project-root))
                 lsp-ui--sfn-projects)))
    (lsp-ui--sfn-merge-projects))
  (defun lsp-ui--sfn-save-projects ()
    (lsp-ui--sfn-serialize lsp-ui--sfn-projects
                           lsp-ui--sfn-projects-file)
    (setq lsp-ui--sfn-projects-on-file
          (and (sequencep lsp-ui--sfn-projects)
               (copy-sequence lsp-ui--sfn-projects))))
  (defun lsp-ui--sfn-merge-projects ()
    (let* ((known-now lsp-ui--sfn-projects)
           (known-on-last-sync lsp-ui--sfn-projects-on-file)
           (known-on-file
            (lsp-ui--sfn-deserialize lsp-ui--sfn-projects-file))
           (removed-after-sync (lsp-ui--sfn-diff known-on-last-sync known-now))
           (removed-in-other-process
            (lsp-ui--sfn-diff known-on-last-sync known-on-file))
           (result (delete-dups
                    (lsp-ui--sfn-diff
                     (append known-now known-on-file)
                     (append removed-after-sync
                             removed-in-other-process)))))
      (setq lsp-ui--sfn-projects result)
      (lsp-ui--sfn-save-projects)))
  (lsp-ui--sfn-load-projects)
  (defun file-in-project? (project-dir)
    (string-prefix-p
     (expand-file-name project-dir)
     (buffer-file-name (current-buffer))))
  (defun lsp-ui-peek--truncate-filepath (orig-fun &rest args)
    "Advisory function to only keep the filename from the path
when using lsp-ui-peek functionality.
Used for a pre-defined list of modes to mitigate large, unreadable filepaths in lsp-ui-peek-find-references.
Applies ORIG-FUN to ARGS first, and then truncates the path."
    (if (seq-some #'file-in-project?
                  lsp-ui--sfn-projects)
        (let ((res (apply orig-fun args)))
          (file-name-nondirectory res))
      (apply orig-fun args)))
  (advice-add 'lsp-ui--workspace-path :around 'lsp-ui-peek--truncate-filepath)
  (setq lsp-ui-doc-include-signature t
        lsp-eldoc-enable-hover nil
        lsp-ui-sideline-update-mode 'point))

(add-hook 'before-revert-hook #'lsp-ui-sideline--delete-ov)

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :commands company-lsp)

(add-hook 'lsp-mode-hook #'lsp-ui-mode)

;;; Git
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


;;;; git diff
;; Visual diff feedback in the margin/gutter
(use-package diff-hl
  :ensure t
  :config
  (set-face-attribute 'diff-hl-change nil :height font-height)
  (set-face-attribute 'diff-hl-delete nil :height font-height)
  (set-face-attribute 'diff-hl-insert nil :height font-height)
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Don't let ediff create any fancy layouts, just use a proper, separate buffer.
(use-package ediff
  :ensure t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;; Languages
;;;; Shell Script
;; Enable shell-script mode for zshell configuration files
(let ((shell-files '("zprofile" "zshenv" "zshrc" "zlogin" "zlogout" "zpreztorc")))
  (mapc (lambda (file)
          (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
        shell-files))

;;;; Lisp
;; Some common functionality for all lisp-like languages (akin to smartparens)
(defun wrap-with (s)
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(bind-key "TAB" #'completion-at-point read-expression-map)
(bind-key "M-(" (lambda () (wrap-with "(")) lisp-mode-shared-map)
(bind-key "M-\"" (lambda () (wrap-with "\"")) lisp-mode-shared-map)

;;;; Emacs Lisp
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :commands (elisp-slime-nav-mode)
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook (lambda () (company:add-local-backend 'company-elisp)))

;;;; Common Lisp
;; the SBCL configuration file is in Common Lisp
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
  :commands cider-jack-in
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
         ([remap xref-find-definitions] . godef-jump)
         ([remap xref-pop-marker-stack] . pop-tag-mark)
         ("C-c c" . compile)
         ("C-c r" . recompile))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun my--go-mode-hook ()
    (setq-local indent-tabs-mode 1)
    (setq-local tab-width 2)
    (subword-mode +1)
    (go-eldoc-setup)
    (company:add-local-backend 'company-go)
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

(use-package go-eldoc
  :ensure t
  :after go-mode)

(use-package company-go
  :ensure t
  :after go-mode
  :init
  (setq company-go-gocode-command "gocode"
        company-go-gocode-args '("-source"
                                 "-ignore-case"
                                 "-builtin"
                                 "-unimported-packages")))

(use-package flycheck-gometalinter
  :ensure t
  :after flycheck
  :config
  (setq flycheck-gometalinter-fast t
        flycheck-gometalinter-disable-linters '("gotype"))
  (add-hook 'go-mode-hook (lambda () (flycheck-gometalinter-setup))))

(use-package go-impl
  :ensure t
  :after go-mode
  :bind (:map go-mode-map
         ("C-c C-l" . go-impl)))

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
              dap-eval-thing-at-point
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
(defun my--install-python-dependencies ()
  (when (executable-find "pip")
    (start-process "Python deps" nil "pip install" "jedi flake8 autopep8 yapf")))

(add-hook 'python-mode-hook #'subword-mode)

(use-package anaconda-mode
  :ensure t
  :hook python-mode
  :bind (:map anaconda-mode-map
         ([remap xref-find-definitions] . anaconda-mode-find-definitions)
         ([remap xref-find-references] . anaconda-mode-find-references)
         ([remap describe-thing-at-point] . anaconda-mode-show-doc))
  :config
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
          js-indent-level 2))
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
  (setq typescript-indent-level 2))

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
  :ensure t)

;;; Miscellaneous
;; Start emacs from within emacs!
(defun start-emacs ()
  (interactive)
  (call-process (executable-find "emacs") nil 0 nil))

;; Handy functions to URL-encode/-decode a region
(defun url-encode-region (beg end)
  "URL encode the region between BEG and END."
  (interactive "r")
  (if (use-region-p)
      (let* ((selected-text (buffer-substring beg end))
             (encoded-text (url-hexify-string selected-text)))
        (kill-region beg end)
        (insert encoded-text))))

(defun url-decode-region (beg end)
  "URL decode the region between BEG and END."
  (interactive "r")
  (if (use-region-p)
      (let* ((selected-text (buffer-substring beg end))
             (decoded-text (url-unhex-string selected-text)))
        (kill-region beg end)
        (insert decoded-text))))

(defun raw-prefix-arg-p (arg)
  (and (listp arg) (car arg)))

(defun insert-agenda-week (&optional arg)
  "Insert a new week at point.  Used in personal time-clocking agenda.
When ARG is specified, prompts for a file to add it to."
  (interactive "P")
  (let* ((raw (raw-prefix-arg-p arg))
         (arg (prefix-numeric-value arg))
         (use-file-dialog nil)
         (file (when raw (read-file-name "Agenda file: " "~")))
         (start (org-read-date t t nil "Starting" (current-time)))
         (end (time-add start (days-to-time 4)))
         (text (with-temp-buffer
                 (org-insert-time-stamp start nil t "* Week " "--")
                 (org-insert-time-stamp end  nil t nil "\n")
                 (dotimes (i 5)
                   (let ((day (time-add start (days-to-time i))))
                     (org-insert-time-stamp
                      day
                      nil
                      t
                      (format-time-string "** %A " day)
                      "\n")))
                 (insert "\n#+BEGIN: clocktable :scope file :maxlevel 2 :link t"
                         " :tstart " (format-time-string "\"<%F %a>\"" start)
                         " :tend "(format-time-string "\"<%F %a>\"" (time-add end (days-to-time 1)))
                         "\n#+END:")
                 (org-clock-report)
                 (buffer-string))))
    (if file
        (write-region text nil file t)
      (save-excursion
        (cond ((= arg 1) (goto-char (point-max)))
              (t (goto-char arg)))
        (insert text)))))

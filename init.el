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

(use-package use-package-ensure-system-package :ensure t)

(defconst emacs-misc-dir (expand-file-name "misc" user-emacs-directory))
(defconst emacs-theme-dir (expand-file-name "themes" user-emacs-directory))

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(defun list-init-files (directory)
  "List all .init.el files inside DIRECTORY."
  (if (not (file-exists-p directory))
      '()
    (let (init-files-list
          (current-dir-list (directory-files-and-attributes directory t)))
      (dolist (dir-item current-dir-list init-files-list)
        (if (equal ".init.el" (substring (car dir-item) -8))
            (let ((dir-item-base (substring (car dir-item) 0 -3)))
              (setq init-files-list
                    (cons dir-item-base
                          init-files-list))))))))
(use-package s :ensure t)
(defun platform-init-path ()
  "Return path to directory containing platform-specific init files."
  (let* ((platform-dir (symbol-name system-type))
         (sanitized-platform-dir
          (if (s-contains? "/" platform-dir)
              (car (last (s-split "/" platform-dir)))
            platform-dir)))
    (concat user-emacs-directory
            sanitized-platform-dir)))

;; If there are any customizations per-machine, per-user, load them as
;; well
(mapc 'load
      (sort (list-init-files (platform-init-path))
            'string-lessp))

(push emacs-theme-dir custom-theme-load-path)
(dolist (dir (directory-files emacs-theme-dir))
  (let ((dirpath (expand-file-name dir emacs-theme-dir)))
    (unless (or (member dir '("." ".." ".git"))
		        (not (file-directory-p dirpath)))
      (push dirpath custom-theme-load-path))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; (setq x-select-request-type 'STRING)

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

;; Zoom in and out using text-scale commands
(bind-key "C--" #'text-scale-decrease global-map)
(bind-key "C-+" #'text-scale-increase global-map)

(defvar font-height (face-attribute 'default :height))
;; (set-face-attribute 'default nil :family "Fira Code" :height 140 :weight 'light)
;; (set-face-attribute 'variable-pitch nil :family "Baskerville")
;; (set-frame-font "Fira Code-11:light")
(setq inhibit-compacting-font-caches t)

;; PATCHing stuff
(use-package el-patch
  :ensure t
  :config
  (setq el-patch-enable-use-package-integration t))

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
              ring-bell-function 'ignore
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
  :ensure t
  :bind ("C-;" . iedit-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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
  :hook (after-init . ivy-mode)
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
         ("C-j" . ivy-done)
         ("<escape>" . minibuffer-keyboard-quit))
  :config
  ;; Fuzzy matching
  (use-package flx :ensure t)
  (setq ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        enable-recursive-minibuffers t
        ivy-display-style nil
        ivy-height 8 ;; used when posframe is not available
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
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s ."))

(use-package swiper
  :ensure t
  :after ivy
  :config
  (setq swiper-use-visual-line-p #'ignore))
(use-package counsel :ensure t
  :after swiper
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\'"))

(use-package ivy-posframe
  :after ivy
  :ensure t
  :delight
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20)))
  (setq ivy-posframe-width 80)
  (ivy-posframe-mode +1))

(use-package ggtags
  :ensure t
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  ;; ensure ace-window keybind doesn't get overridden in ggtags-mode buffers
  (unbind-key "M-o" ggtags-navigation-map))

;; ensure no stale imenu tags in treemacs or otherwise
(setq imenu-auto-rescan t)

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
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (set-face-attribute 'symbol-overlay-default-face nil :background "DarkOrchid" :foreground "white"))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :delight
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|) ; left-align vertical bar
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "darkgray"))

;;;; Dired
(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
         ("i" . dired-subtree-insert)
         (";" . dired-subtree-remove)))

;; TEXT
;;; Apply variable-pitch font to all text-related buffers
(use-package variable-pitch
  :ensure nil
  :after org
  :hook (org-mode . variable-pitch-mode))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c C-o" . org-open-maybe))
  :custom
  (org-log-done t)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-list-indent-offset 4)
  (org-hide-leading-stars t)
  (org-ellipsis " \u25bc" )
  (org-hide-emphasis-markers t)
  (org-image-actual-width nil)
  (org-hidden-keywords '())
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-preserve-indentation t)
  ;; LaTeX preview size is a bit too small for comfort
  ;; (org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (org-highlight-latex-and-related '(latex))
  :custom-face
  (org-document-title ((t (:inherit outline-1 :height 1.20 :underline t))))
  (org-document-info ((t (:inherit outline-1 :height 1.15))))
  (org-document-info-keyword ((t (:inherit outline-1 :height 1.10))))
  (org-warning ((t (:weight bold :foreground "#CC9393" :height 1.05))))
  (org-level-1 ((t (:inherit outline-1 :height 1.05))))
  (org-level-2 ((t (:inherit outline-2 :height 1.00))))
  (org-level-3 ((t (:inherit outline-3 :height 1.00))))
  (org-level-4 ((t (:inherit outline-4 :height 1.00))))
  (org-level-5 ((t (:inherit outline-5 :height 1.00))))
  :config
  ;; (org-link-frame-setup '((file . find-file))) ;; don't split windows from org-mode
  (defun org-force-open-current-window ()
    (interactive)
    (let ((org-link-frame-setup (quote
                                 ((vm . vm-visit-folder)
                                  (vm-imap . vm-visit-imap-folder)
                                  (gnus . gnus)
                                  (file . find-file)
                                  (wl . wl)))))
      (org-open-at-point)))
  ;; Depending on universal argument try opening link
  (defun org-open-maybe (&optional arg)
    (interactive "P")
    (if arg
        (org-open-at-point)
      (org-force-open-current-window)))
  (when (version<= "9.2" (org-version))
    (require 'org-tempo))
  (defun my-adjoin-to-list-or-symbol (element list-or-symbol)
    (let ((list (if (not (listp list-or-symbol))
                    (list list-or-symbol)
                  list-or-symbol)))
      (require 'cl-lib)
      (cl-adjoin element list)))
  ;; Apply fixed-pitch font to all org text that is in code blocks or tables
  (mapc (lambda (face)
          (set-face-attribute face nil :inherit
                              (my-adjoin-to-list-or-symbol 'fixed-pitch
                                                           (face-attribute face :inherit))))
        (list 'org-code 'org-block 'org-table))
  (add-hook 'org-mode-hook (lambda ()
                             (setq line-spacing .2)
                             "Beautify Org Checkbox Symbol"
                             (push '("[ ]" . "☐") prettify-symbols-alist)
                             (push '("[X]" . "☑" ) prettify-symbols-alist)
                             (push '("[-]" . "❍" ) prettify-symbols-alist)
                             (push '("-->" . "⟶") prettify-symbols-alist)
                             (push '("<--" . "⟵") prettify-symbols-alist)
                             (prettify-symbols-mode)))
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")
  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
              1 'org-checkbox-done-text prepend)
             ("^ *\\([-]\\) "
              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "\u2022"))))
             ("^ *\\([+]\\) "
              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "\u2023")))))
           'append))
        (list 'org-mode 'org-journal-mode))
  (delight 'org-indent-mode nil t))

;; Org-mode buffer-local variables
(put 'org-src-preserve-indentation 'safe-local-variable (lambda (val) #'booleanp))

;; Poporg is a useful package to edit comments in code as an org-mode buffer
;; By the way, it works amazingly
(use-package poporg
  :ensure t
  :commands poporg-dwim
  :bind (("C-c \"" . poporg-dwim)))

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :init
  (use-package company-org-roam
    :ensure t
    :after org
    :load-path "elisp/company-org-roam"
    :config
    (add-hook 'org-mode-hook (lambda ()
                               (company:add-local-backend 'company-org-roam))))
  :custom
  (org-roam-directory "~/stack/Documents/roam")
  (org-roam-completion-system 'ivy)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#C991E1"))))
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n x" . org-roam-jump-to-index)
          ("C-c n b" . org-roam-switch-to-buffer)
          ("C-c n g" . org-roam-graph))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))))

(use-package org-journal
  :ensure t
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :custom
  (org-journal-date-prefix "#+TITLE: [DAILY] ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/stack/Documents/roam")
  (org-journal-date-format "%Y-%m-%d")
  :config
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t))
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  (setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                                 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))))

(use-package org-download
  :ensure t
  :after org
  :bind
  (:map org-mode-map
   (("s-Y" . org-download-screenshot)
    ("s-y" . org-download-yank))))

(use-package org-cliplink
  :ensure t
  :after org
  :bind
  (:map org-mode-map
   (("C-c c" . org-cliplink))))


;;;; Look & feel
;; Prettifying org-mode buffers.
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉"
                                  "●"
                                  "○")))

;; DEFT
(use-package deft
  :after org
  :ensure t
  :bind (("C-c n d" . deft)
         ("C-x C-g". deft-find-file))
  :commands (deft)
  :custom
  (deft-auto-save-interval 5)
  (deft-directory "~/stack/Documents/roam")
  (deft-extensions '("txt" "org" "md"))
  (deft-default-extension "org")
  (deft-use-filter-string-for-filename t)
  (deft-recursive t)
  :config/el-patch
  (defun deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (el-patch-swap (if deft-use-filename-as-title
                       (deft-base-filename file)
                     (let ((begin (string-match "^.+$" contents)))
                       (if begin
                           (funcall deft-parse-title-function
                                    (substring contents begin (match-end 0))))))
                   (org-roam--get-title-or-slug file))))

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
  (setq treemacs-indentation-string (propertize " \| " 'face 'font-lock-comment-face)
        treemacs-indentation 1
        treemacs-no-png-images nil
        treemacs-display-in-side-window t
        treemacs-width 30
        treemacs-silent-refresh t
        treemacs-silent-filewatch t
        treemacs-show-hidden-files t
        treemacs-sorting 'alphabetic-case-insensitive-desc
        treemacs-follow-after-init t
        treemacs-project-follow-cleanup t
        treemacs-tag-follow-cleanup t
        treemacs-tag-follow-delay 1.0
        treemacs-recenter-distance 0.1
        treemacs-recenter-after-tag-follow 'on-distance
        treemacs-recenter-after-file-follow 'on-distance
        treemacs-file-event-delay 1000
        treemacs-file-follow-delay 0.1)
  (treemacs-follow-mode t)
  (treemacs-tag-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (add-hook 'treemacs-mode-hook (lambda ()
                                  (linum-mode -1)
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
                              :extensions (fallback))))
    )
  (treemacs-load-theme "doom"))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; (use-package solaire-mode
;;   :ensure t
;;   :defer 2
;;   :hook (((after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;;          (minibuffer-setup . solaire-mode-in-minibuffer))
;;   :config
;;   (add-hook 'change-major-mode-hook #'turn-on-solaire-mode)
;;   (solaire-global-mode +1)
;;   (solaire-mode-swap-bg))

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
        lsp-eldoc-render-all t
        lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap describe-thing-at-point] . lsp-describe-thing-at-point))
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-sideline-update-mode 'line))

(use-package company-lsp
  :ensure t
  :after (lsp-mode company)
  :init (yas-minor-mode 1))

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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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
  :bind (:map slime-mode-map
         ("C-c C-s" . slime-selector))
  :config
  (setq inferior-lisp-program (executable-find "sbcl")
        slime-contribs '(slime-fancy slime-company slime-indentation)
        slime-autodoc-use-multiline-p t
        slime-enable-evaluate-in-emacs t
        common-lisp-style-default "sbcl")
  (add-hook 'slime-load-hook (lambda () (require 'slime-fancy)))
  (defun slime-enable-concurrent-hints ()
    (interactive)
    (setf slime-inhibit-pipelining nil)))

(use-package slime-company
  :ensure t
  :after slime
  :config
  (setq slime-company-completion 'fuzzy))

(add-hook 'slime-mode-hook (lambda ()
                             (setq lisp-indent-function 'common-lisp-indent-function
                                   lisp-loop-indent-subclauses nil
                                   lisp-loop-indent-forms-like-keywords t
                                   lisp-lambda-list-keyword-parameter-alignment t)
                             (company:add-local-backend 'company-slime)))

;;;; Clojure
(use-package clojure-mode
  :ensure t
  :mode ("\\.clj[xc]?\\'"
         "build\\.boot\\'"
         "project\\.clj\\'")
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
    (setq-local tab-width 4)
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
  :ensure t
  :ensure-system-package (prettier . "npm i -g prettier")
  :commands prettier-js
  :init
  (defun my/prettier-js-hook ()
    (setq-local indent-region-function #'prettier-js)
    (prettier-js-mode))
  :hook ((typescript-mode js2-mode) . my/prettier-js-hook))

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
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :ensure t
  :after tern
  :config
  (add-hook 'js2-mode-hook (lambda () (company:add-local-backend 'company-tern))))

(use-package json-mode
  :ensure t
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
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown")
  '(mapc
    (lambda (face)
      (set-face-attribute
       face nil
       :inherit
       (my-adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'markdown-pre-face 'markdown-inline-code-face)))

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
  :hook (haskell-mode . intero-mode)
  :config/el-patch
  (el-patch-defun intero-ghci-output-flags ()
    "Get the appropriate ghci output flags for the current GHC version"
    (with-current-buffer (intero-buffer 'backend)
      (let ((current-version (mapcar #'string-to-number (split-string (el-patch-swap intero-ghc-version (intero-ghc-version)) "\\."))))
        (if (intero-version>= '(8 4 1) current-version)
            '("-fno-code" "-fwrite-interface")
          '("-fobject-code")))))
  (el-patch-defun intero-start-process-in-buffer (buffer &optional targets source-buffer stack-yaml)
    "Start an Intero worker in BUFFER.
Uses the specified TARGETS if supplied.
Automatically performs initial actions in SOURCE-BUFFER, if specified.
Uses the default stack config file, or STACK-YAML file if given."
    (if (buffer-local-value 'intero-give-up buffer)
        buffer
      (let* ((process-info (intero-start-piped-process buffer targets stack-yaml))
             (arguments (plist-get process-info :arguments))
             (options (plist-get process-info :options))
             (process (plist-get process-info :process)))
        (set-process-query-on-exit-flag process nil)
        (mapc
         (lambda (flag)
           (process-send-string process ((el-patch-swap append concat) ":set " flag "\n")))
         (intero-ghci-output-flags))
        (process-send-string process ":set -fdefer-type-errors\n")
        (process-send-string process ":set -fdiagnostics-color=never\n")
        (process-send-string process ":set prompt \"\\4\"\n")
        (with-current-buffer buffer
          (erase-buffer)
          (when stack-yaml
            (setq intero-stack-yaml stack-yaml))
          (setq intero-targets targets)
          (setq intero-start-time (current-time))
          (setq intero-source-buffer source-buffer)
          (setq intero-arguments arguments)
          (setq intero-starting t)
          (setq intero-callbacks
                (list (list (cons source-buffer
                                  buffer)
                            (lambda (buffers msg)
                              (let ((source-buffer (car buffers))
                                    (process-buffer (cdr buffers)))
                                (with-current-buffer process-buffer
                                  (when (string-match "^Intero-Service-Port: \\([0-9]+\\)\n" msg)
                                    (setq intero-service-port (string-to-number (match-string 1 msg))))
                                  (setq-local intero-starting nil))
                                (when source-buffer
                                  (with-current-buffer source-buffer
                                    (when flycheck-mode
                                      (run-with-timer 0 nil
                                                      'intero-call-in-buffer
                                                      (current-buffer)
                                                      'intero-flycheck-buffer)))))
                              (message "Booted up intero!"))))))
        (set-process-filter
         process
         (lambda (process string)
           (when intero-debug
             (message "[Intero] <- %s" string))
           (when (buffer-live-p (process-buffer process))
             (with-current-buffer (process-buffer process)
               (goto-char (point-max))
               (insert string)
               (when (and intero-try-with-build
                          intero-starting)
                 (let ((last-line (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
                   (if (string-match-p "^Progress" last-line)
                       (message "Booting up intero (building dependencies: %s)"
                                (downcase
                                 (or (car (split-string (replace-regexp-in-string
                                                         "\u0008+" "\n"
                                                         last-line)
                                                        "\n" t))
                                     "...")))
                     (message "Booting up intero ..."))))
               (intero-read-buffer)))))
        (set-process-sentinel process 'intero-sentinel)
        buffer))))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; (use-package parchment-theme
;;   :ensure t)

(use-package modus-operandi-theme
  :ensure t
  :custom
  (modus-operandi-theme-distinct-org-blocks t)
  (modus-operandi-theme-slanted-constructs t)
  :config
  (load-theme 'modus-operandi t))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

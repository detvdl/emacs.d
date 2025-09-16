;;;; `ediff'
(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

(use-package prot-ediff
  :ensure nil
  :functions (prot-ediff-visible-buffers-2 prot-ediff-visible-buffers-3)
  :commands (prot-ediff-visible-buffers-2 prot-ediff-visible-buffers-3)
  ;; :bind
  ;; The C-x v prefix is for all "version control" commands that are
  ;; already built into Emacs.  It makes sense to extend it for this
  ;; use-case.
  ;; NOTE: superseded by the transient prefix defined below
  ;; (("C-x v 2" . prot-ediff-visible-buffers-2)
   ;; ("C-x v 3" . prot-ediff-visible-buffers-3))
  :hook
  ((ediff-before-setup . prot-ediff-store-layout)
   (ediff-quit . prot-ediff-restore-layout)))

;;;; `project'
(use-package project
  :ensure nil
  :bind
  (("C-x p ." . project-dired)
   ("C-x p C-g" . keyboard-quit)
   ("C-x p <return>" . project-dired)
   ("C-x p <delete>" . project-forget-project))
  :config
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers '(".project")) ; Emacs 29
  (setq project-key-prompt-style t) ; Emacs 30

  (advice-add #'project-switch-project :after #'prot-common-clear-minibuffer-message))

(use-package prot-project
  :ensure nil
  :commands (prot-project-switch prot-project-in-tab)
  :bind
  ( :map project-prefix-map
    ("p" . prot-project-maybe-in-tab)))

;;;; `diff-mode'
(use-package diff-mode
  :ensure nil
  :defer t
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil) ; I do it on demand, with my `agitate' package (more below)
  (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
  (setq diff-font-lock-syntax nil))

;;; Version control framework (vc.el, vc-git.el, and more)
(use-package vc
  :ensure nil
  :bind
  (;; NOTE: superseded by transient definition
   ;; :map global-map
   ;; ("C-x v B" . vc-annotate) ; Blame mnemonic
   ;; ("C-x v e" . vc-ediff)
   ;; ("C-x v k" . vc-delete-file) ; 'k' for kill==>delete is more common
   ;; ("C-x v G" . vc-log-search)  ; git log --grep
   ;; ("C-x v t" . vc-create-tag)
   ;; ("C-x v c" . vc-clone) ; Emacs 31
   ;; ("C-x v d" . vc-diff)
   ;; ("C-x v ." . vc-dir-root) ; `vc-dir-root' is from Emacs 28
   ;; ("C-x v <return>" . vc-dir-root)
   :map vc-dir-mode-map
   ("t" . vc-create-tag)
   ("O" . vc-log-outgoing)
   ("o" . vc-dir-find-file-other-window)
   ("d" . vc-diff)         ; parallel to D: `vc-root-diff'
   ("k" . vc-dir-delete-file)
   ("G" . vc-revert)
   :map vc-git-stash-shared-map
   ("a" . vc-git-stash-apply-at-point)
   ("c" . vc-git-stash) ; "create" named stash
   ("k" . vc-git-stash-delete-at-point) ; symmetry with `vc-dir-delete-file'
   ("p" . vc-git-stash-pop-at-point)
   ("s" . vc-git-stash-snapshot)
   :map vc-annotate-mode-map
   ("M-q" . vc-annotate-toggle-annotation-visibility)
   ("C-c C-c" . vc-annotate-goto-line)
   ("<return>" . vc-annotate-find-revision-at-line)
   :map log-edit-mode-map
   ("M-s" . nil) ; I use M-s for my search commands
   ("M-r" . nil) ; I use `consult-history'
   :map log-view-mode-map
   ("<tab>" . log-view-toggle-entry-display)
   ("<return>" . log-view-find-revision)
   ("s" . vc-log-search)
   ("o" . vc-log-outgoing)
   ("f" . vc-log-incoming)
   ("F" . vc-update)
   ("P" . vc-push))
  :init
  (setq vc-follow-symlinks t)
  :config
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; I only use Git.  If I ever need another, I will include it here.
  ;; This may have an effect on performance, as Emacs will not try to
  ;; check for a bunch of backends.
  (setq vc-handled-backends '(Git))

  (setq vc-dir-save-some-buffers-on-revert t) ; Emacs 31

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)
  ;; I can see the files from the Diff with C-c C-d
  (remove-hook 'log-edit-hook #'log-edit-show-files)

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  ;; I use a different account for git commits
  (setq add-log-mailing-address "detvdl@pm.me")
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-log-switches '("--stat"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        `("%d %h %ai %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                   "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                   "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                   "\\(?3:.*?\\):")
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  ;; These two are from Emacs 29
  (setq vc-git-log-edit-summary-target-len 50)
  (setq vc-git-log-edit-summary-max-len 70))

;; Advice to ensure vc-do-command follows symlinked
;; default-directories when working with relative paths
;; FIXME: does not work, because filepaths in diff-hl-stage-current-hunk generates diff file contents
;; based on (buffer-file-name), which does not use default-directory but maintains the symlink
;; IDEA: advise diff-hl function(s) by expanding file-names to their truenames before continuing
;; both the filepath of the edited file, as well as the generated temporary file for the diff
;; (defun follow-default-directory-symlink (orig-fun &rest args)
;;   (let ((default-directory (if (file-symlink-p (directory-file-name default-directory))
;;                                (file-truename default-directory)
;;                              default-directory)))
;;     (apply orig-fun args)))
;; (advice-add 'vc-do-command :around #'follow-default-directory-symlink)

;;; Interactive and powerful git front-end (Magit)
(use-package transient
  :defer t
  :config
  (setq transient-show-popup 0.5))

(use-package magit
  :ensure t
  :bind
  ( :map global-map
    ("C-c g" . magit-status)
    :map magit-mode-map
    ("C-w" . nil)
    ("M-w" . nil))
  :init
  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))
  :config
  (setq git-commit-summary-max-length 50)
  ;; NOTE 2023-01-24: I used to also include `overlong-summary-line'
  ;; in this list, but I realised I do not need it.  My summaries are
  ;; always in check.  When I exceed the limit, it is for a good
  ;; reason.
  (setq git-commit-style-convention-checks '(non-empty-second-line))

  (setq magit-diff-refine-hunk t)

  ;; Show icons for files in the Magit status and other buffers.
  (with-eval-after-load 'magit
    (setq magit-format-file-function #'magit-format-file-nerd-icons)))

(use-package magit-repos
  :ensure nil ; part of `magit'
  :commands (magit-list-repositories)
  :init
  (setq magit-repository-directories
        '(("~/Git" . 1)
          ("~/Code" . 1)
          ("~/.emacs.d" . 1))))

(use-package diff-hl
  :ensure t
  :defines (diff-hl-mode-map)
  :functions (diff-hl-update)
  :config
  (let ((font-height (face-attribute 'default :height)))
    (set-face-attribute 'diff-hl-change nil :height font-height)
    (set-face-attribute 'diff-hl-delete nil :height font-height)
    (set-face-attribute 'diff-hl-insert nil :height font-height))
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (let* ((height (frame-char-height))
         (width 2)
         (ones (1- (expt 2 width)))
         (bits (make-vector height ones)))
    (define-fringe-bitmap 'my-diff-hl-bitmap bits height width))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my-diff-hl-bitmap)))

;; Only load the diff-hl package once we actually visit a file
;; This hook gets added by global-diff-hl mode anyway
(add-hook 'find-file-hook #'diff-hl-update)

(require 'transient)

(transient-define-prefix detvdl/vc-transient ()
  [:class transient-columns
          ["VC check"
           ("b" "Blame/annotate"  vc-annotate) ; Blame mnemonic
           ("d"  "Diff" vc-diff)
           ("e" "Ediff"  vc-ediff)
           ("G" "Log search" vc-log-search)  ; git log --grep
           ("."  "Dir root" vc-dir-root) ; `vc-dir-root' is from Emacs 28
           ("+" "Update" vc-update)
           ("h" "Region history" vc-region-history)]
          ["VC act"
           ("<return>" "Next action"  vc-next-action)
           ("c" "Clone" vc-clone) ; Emacs 31
           ("k" "Delete file"  vc-delete-file) ; 'k' for kill==>delete is more common
           ("m" "Merge" vc-merge)
           ("P" "Push" vc-push)
           ("R" "Rename file" vc-rename-file)
           ("t" "Create tag" vc-create-tag)
           ("?" "Create branch" vc-create-branch)
           ]
          ["Diff-hl"
           ("[" "Prev hunk" diff-hl-previous-hunk :transient t)
           ("]" "Next hunk" diff-hl-next-hunk :transient t)
           ("=" "Goto hunk" diff-hl-diff-goto-hunk)
           ("r" "Revert hunk" diff-hl-revert-hunk :transient t)
           ("*" "Show hunk" diff-hl-show-hunk)
           ("s" "Stage hunk" diff-hl-stage-current-hunk :transient t)
           ("u" "Unstage file" diff-hl-unstage-file :transient t)
           ]
          ["Ediff"
           ("2" "Ediff 2 buffers" prot-ediff-visible-buffers-2)
           ("3" "Ediff 3 buffers" prot-ediff-visible-buffers-3)]]
  )

(define-key diff-hl-mode-map (kbd "C-x v") #'detvdl/vc-transient)

(provide 'detvdl-emacs-git)

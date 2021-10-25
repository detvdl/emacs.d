;;; detvdl-org.el -- Personal Org-mode configuration
;;; Commentary:
;;; Code:

(defvar user-roam-dir "~/stack/Documents/roam")
(defvar user-roam-dailies-dir (file-name-as-directory (expand-file-name "daily" user-roam-dir)))

;; TEXT
;;; Apply variable-pitch font to all text-related buffers
(use-package variable-pitch
  :straight nil
  :after org
  :hook (org-mode . variable-pitch-mode))

(use-package org
  :straight org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c C-o" . org-open-maybe))
  :custom
  (org-agenda-files (directory-files-recursively user-roam-dailies-dir "\\.org$"))
  (org-log-done t)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-link-descriptive t) ;; set to nil to avoid immediately formatted links
  (org-list-indent-offset 2)
  (org-hide-leading-stars t)
  (org-ellipsis " ⧩" )
  (org-src-fontify-natively t)
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-fontify-quote-and-verse-blocks t)
  (org-image-actual-width nil)
  (org-hidden-keywords '())
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-export-preserve-breaks t)
  ;; LaTeX preview size is a bit too small for comfort
  ;; (org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (org-highlight-latex-and-related '(latex))
  (org-todo-keywords
   '((sequence "TODO(t)" "IN PROGRESS(i!)" "VERIFY(v@/!)" "|" "DONE(d!)" "CANCELED(c@/!)")))
  (org-log-into-drawer "LOGBOOK")
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
  (add-to-list 'org-export-backends 'md)
  (require 'ob-shell)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (awk . t)))
  ;; (require 'ox-confluence)
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
                             (push '("--" . "⸺") prettify-symbols-alist)
                             (push '("-->" . "⟶") prettify-symbols-alist)
                             (push '("<--" . "⟵") prettify-symbols-alist)
                             (push '("=>" . "⇒") prettify-symbols-alist)
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
             ("^ *\\([X]\\) "
              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "\u2023")))))
           'append))
        (list 'org-mode 'org-journal-mode))
  (blackout 'org-indent-mode))

(use-package ox-jira
  :straight t
  :after org
  :config
  (load-library "ox-jira")
  (add-to-list 'org-export-backends 'jira))

(use-package org-contrib
  :straight t
  :after org
  :config
  (require 'ox-confluence)
  (add-to-list 'org-export-backends 'confluence))

;; Org-mode buffer-local variables
(put 'org-src-preserve-indentation 'safe-local-variable (lambda (val) #'booleanp))

;; Poporg is a useful package to edit comments in code as an org-mode buffer
;; By the way, it works amazingly
(use-package poporg
  :straight t
  :commands poporg-dwim
  :bind (("C-c \"" . poporg-dwim)))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory user-roam-dir)
  (org-roam-completion-system 'ivy)
  (org-roam-link-title-format "[[%s]]")
  ;; enable to have automatic completion anywhere in the buffer
  (org-roam-completion-everywhere nil)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* [%<%R>] %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: [daily] %<%Y-%m-%d>\n"))))
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#C991E1"))))
  (org-roam-link-current ((t (:inherit org-roam-link :slant italic))))
  :bind (("C-c n c" . org-roam-capture)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n n" . org-id-get-create) ;; creates new Node ID for a given headline/file
         ("C-c n w" . org-roam-refile)
         ("C-c n f" . org-roam-node-find)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n u" . org-roam-dailies-goto-today)
         :map org-mode-map
         (("C-c n i" . org-roam-node-insert)))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight (:host github
             :repo "org-roam/org-roam-ui" :branch "main"
             :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-transclusion
  :straight (org-transclusion
             :host github :type git
             :repo "nobiot/org-transclusion")
  :after org
  :bind (:map org-mode-map
         (("<f12>" . org-transclusion-mode))))

(use-package org-journal
  :straight t
  ;; :bind
  ;; ("C-c n j" . org-journal-new-entry)
  ;; ("C-c n t" . org-journal-today)
  :custom
  (org-journal-date-prefix "#+TITLE: [DAILY] ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir (concat user-roam-dir "daily"))
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
  :straight t
  :after org
  :bind
  (:map org-mode-map
   (("s-Y" . org-download-screenshot)
    ("s-y" . org-download-yank)))
  :config
  (when (eq system-type 'darwin)
      (setq org-download-screenshot-method "screencapture -i %s")))

(use-package org-cliplink
  :straight t
  :after org
  :bind
  (:map org-mode-map
   (("C-c c" . org-cliplink))))

;;;; Look & feel
;; Prettifying org-mode buffers.
(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-prettify-item-bullets t)
  (org-superstar-item-bullet-alist '((?- . ?•)
                                     (?+ . ?▸)
                                     (?* . ?▪))))

(use-package org-appear
  :straight (org-appear
             :type git :host github
             :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

(use-package darkroom
  :straight t
  :custom
  (darkroom-margins 0.1))

;; DEFT
(use-package deft
  :after org
  :straight t
  :bind (("C-c n d" . deft)
         ("C-x C-g". deft-find-file))
  :commands (deft)
  :custom
  (deft-auto-save-interval 5)
  (deft-directory user-roam-dir)
  (deft-extensions '("txt" "org" "md"))
  (deft-default-extension "org")
  (deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  ;; (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-recursive t)
  :config
  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
	  (if begin
	      (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	    (deft-base-filename file))))
  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)
  (setq deft-strip-summary-regexp
	    (concat "\\("
		        "[\n\t]" ;; blank
		        "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		        "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		        "\\)")))

(use-package ob-restclient
  :after org
  :straight t)

;; Make the whole heading line fontified
(setq org-fontify-whole-heading-line t)

(use-package adaptive-wrap
  :straight t
  :hook ((prog-mode org-mode) . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 2))

(provide 'detvdl-org)
;;; detvdl-org.el ends here

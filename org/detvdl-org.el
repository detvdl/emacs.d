;;; detvdl-org.el -- Personal Org-mode configuration
;;; Commentary:
;;; Code:

(defvar user-roam-dir "~/stack/Documents/roam")
(defvar user-roam-dailies-dir (file-name-as-directory (expand-file-name "daily" user-roam-dir)))

(add-to-list 'org-capture-templates
             `("i"
               "Inbox"
               entry
               (file "inbox.org")
               ,(concat "* TODO %?\n"
                        "/Entered on/ %U")))

(defun org-capture-inbox ()
  (interactive)
  (ignore-errors
    (call-interactively 'org-store-link))
  (org-capture nil "i"))

(define-key global-map (kbd "C-c i") 'org-capture-inbox)

;; TEXT
;;; Apply variable-pitch font to all text-related buffers
;; (use-package variable-pitch
;;   :straight nil
;;   :after org
;;   :hook (org-mode . variable-pitch-mode))

(use-package mixed-pitch
  :straight t
  :hook (text-mode . mixed-pitch-mode)
  :custom
  (mixed-pitch-variable-pitch-cursor 'box))

(use-package org
  :straight org
  :blackout org-indent-mode
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-'" . nil)
         ("C-c C-o" . org-open-maybe)
         ("M-<up>" . org-move-subtree-up)
         ("M-<down>" . org-move-subtree-down))
  :init
  (defun org-ask-location ()
    (let* ((org-refile-targets '((nil :maxlevel . 9)))
           (hd (condition-case nil
                   (car (org-refile-get-location "Headline" nil t))
                 (error (car org-refile-history)))))
      (goto-char (point-min))
      (outline-next-heading)
      (if (re-search-forward
           (format org-complex-heading-regexp-format (regexp-quote hd))
           nil t)
          (goto-char (point-at-bol))
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert "* " hd "\n")))
    (end-of-line))
  :custom
  (org-agenda-files '("inbox.org"))
  (org-agenda-hide-tags-regexp ".")
  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " %i %-12:c")
     (tags   . " %i %-12:c")
     (search . " %i %-12:c")))
  (org-directory user-roam-dir)
  (org-babel-confirm-evaluate nil)
  (org-log-done t)
  (org-startup-folded nil)
  (org-startup-indented t) ;; org-indent-mode doesn't play well with pretty org-src blocks from `org-modern.el'
  (org-link-descriptive t) ;; set to nil to avoid immediately formatted links
  (org-list-indent-offset 2)
  (org-hide-leading-stars t)
  (org-src-fontify-natively t)
  (org-return-follows-link t)
  (org-hide-emphasis-markers t)
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  (org-image-actual-width nil)
  (org-hidden-keywords '())
  (org-refile-allow-creating-parent-nodes t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-src-strip-leading-and-trailing-blank-lines t)
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-export-preserve-breaks t)
  (org-export-copy-to-kill-ring 'if-interactive)
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
  ;; Apply fixed-pitch font to all org text that is in code blocks or tables
  :config
  (mapc (lambda (face)
          (set-face-attribute face t :family (face-attribute 'default :family)))
        '(org-code org-block org-block-begin-line org-block-end-line org-table))
  (add-to-list 'org-export-backends 'md)
  (require 'ob-shell)
  (require 'ob-clojure)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell      . t)
                                 (js         . t)
                                 (emacs-lisp . t)
                                 (perl       . t)
                                 (clojure    . t)
                                 (python     . t)
                                 (ruby       . t)
                                 (dot        . t)
                                 (css        . t)
                                 (plantuml   . t)
                                 (awk        . t)))
  (with-eval-after-load "cider"
    (setq org-babel-clojure-backend 'cider))
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
  (add-hook 'org-mode-hook (lambda ()
                             (setq line-spacing .2)
                             (push '("--" . "—") prettify-symbols-alist)
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
        (list 'org-mode 'org-journal-mode)))

(use-package org-capture
  :straight nil
  :after org)

(use-package org-modern
  :straight t
  :after org
  :hook (org-mode . org-modern-mode)
  :custom-face
  (org-modern-block-keyword ((t (:inherit default))))
  :custom
  (org-modern-keyword "‣")
  (org-pretty-entities-include-sub-superscripts nil)
  (org-pretty-entities t)
  (org-auto-align-tags nil)
  (org-tags-column 90)
  (org-insert-heading-respect-content t)
  (org-ellipsis "…")
  (org-modern-label-border 'auto)
  (org-modern-variable-pitch 'variable-pitch)
  (org-modern-timestamp t)
  (org-modern-table t)
  (org-modern-table-vertical 2)
  (org-modern-table-horizontal 1)
  (org-modern-list '((?+ . "•")
                     (?- . "–")
                     (?* . "◦")))
  (org-modern-block t)
  )

(use-package ox-gfm
  :straight t
  :after org)

(use-package ox-gist
  :straight t
  :after org)

(use-package ox-hugo
  :straight t
  :after (ox org)
  :config
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")                ;Place the cursor here finally
                 "\n")))
  (add-to-list 'org-capture-templates
               '("h"
                 "Hugo post"
                 entry
                 (file+olp "~/Blog/content-org/all-posts.org" "Posts")
                 (function org-hugo-new-subtree-post-capture-template))))

(use-package ox-jira
  :straight t
  :after org)

(use-package org-contrib
  :straight t
  :after org
  :config
  (require 'ox-confluence)
  (add-to-list 'org-export-backends 'confluence))

(use-package org-num
  :straight nil
  :after org
  :hook (org-mode . org-num-mode))

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
  (setq org-roam-database-connector 'sqlite-builtin)
  :custom
  (org-roam-directory user-roam-dir)
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
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode +1)
  (setq org-capture-templates `(("b" "bookmark" item
                                 (file+function ,(org-roam-node-file (org-roam-node-from-title-or-alias "Browser: Bookmarks")) org-ask-location)
                                 "- %(org-cliplink-capture)%?\n"
                                 :unnarrowed t
                                 :empty-lines-before 0)))
  )

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
   (("C-c v" . org-cliplink))))

;;;; Look & feel
(use-package org-appear
  :straight (org-appear
             :type git :host github
             :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-delay 0.8))

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

(use-package adaptive-wrap
  :straight t
  :hook ((prog-mode org-mode) . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 2))

(use-package olivetti
  :straight t
  :bind ("<f9>" . olivetti-mode)
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t)
  :config
  (add-hook 'olivetti-mode-on-hook (lambda () (progn (display-line-numbers-mode 0)
                                                     (highlight-indent-guides-mode 0))))
  (add-hook 'olivetti-mode-off-hook (lambda () (when (not (eq major-mode 'org-mode))
                                                 (progn (display-line-numbers-mode +1)
                                                        (highlight-indent-guides-mode +1))))))

(provide 'detvdl-org)
;;; detvdl-org.el ends here

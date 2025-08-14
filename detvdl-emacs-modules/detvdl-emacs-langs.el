;;;; Tabs, indentation, and the TAB key
(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
  (setq-default tab-width 4
                indent-tabs-mode nil))

;;;; Disable "electric" behaviour
(use-package electric
  :ensure nil
  :hook
  (prog-mode . electric-indent-local-mode)
  :config
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  (electric-indent-mode -1))

(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (set-face-attribute 'symbol-overlay-default-face nil :background "DarkOrchid" :foreground "white"))

;; (use-package highlight-indent-guides
;;   :ensure t
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :config
;;   (setq highlight-indent-guides-method 'bitmap)
;;   (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
;;   ;; (setq highlight-indent-guides-character ?\|) ; left-align vertical bar
;;   (setq highlight-indent-guides-auto-enabled t))

;; (defun detvdl-modus-themes-update-faces (&rest _)
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      `(highlight-indent-guides-character-face ((,c :inherit default :foreground ,bg-dim)))))    
;;   )
;; (add-hook 'enable-theme-functions 'detvdl-modus-themes-update-faces)

(when (and (treesit-available-p) detvdl-emacs-treesitter-extras)
  (dolist (grammar
           '((css        . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.2"))
             (bash       . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.21.0"))
             (go         . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.4"))
             (html       . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src"))
             (json       . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
             (markdown   . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
             (python     . ("https://github.com/tree-sitter/tree-sitter-python" "v0.23.6"))
             (rust       . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.24.0"))
             (toml       . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
             (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
             (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar))))
  )

;;;; Parentheses (show-paren-mode)
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay)) ; Emacs 29

;;;; Plain text (text-mode)
(use-package text-mode
  :ensure nil
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :hook
  ((text-mode . turn-on-auto-fill)
   (prog-mode . (lambda () (setq-local sentence-end-double-space t))))
  :config
  (setq sentence-end-double-space nil)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t))

;;;; Arch Linux and AUR package scripts (sh-mode)
(use-package sh-script
  :ensure nil
  :mode ("PKGBUILD" . sh-mode))

;;;; SystemD and other configuration files (conf-mode)
(use-package conf-mode
  :ensure nil
  :mode ("\\`dircolors\\'" "\\.\\(service\\|timer\\)\\'" "dunstrc"))

;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :bind (:map prog-mode-map
              ("M-d" . eldoc-doc-buffer))
  :config
  (setq eldoc-message-function #'message ; don't use mode line for M-x eval-expression, etc.
        eldoc-echo-area-use-multiline-p nil) ; don't expand echo area for eldoc
)

;;;; Eglot (built-in client for the language server protocol)
;; (use-package eglot
;;   :ensure nil
;;   :functions (eglot-ensure)
;;   :commands (eglot)
;;   :config
;;   (setq eglot-sync-connect nil)
;;   (setq eglot-autoshutdown t))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;;;; TODO: test out org-src blocks with lsp-mode: https://github.com/emacs-lsp/lsp-mode/issues/2842#issuecomment-870807018
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)
  (lsp-hover-text-function #'lsp--text-document-signature-help)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all t)
  (lsp-signature-auto-activate t)
  (lsp-signature-render-documentation nil)
  (lsp-prefer-flymake t)
  :init
  (defun detvdl/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)) ;; Configure orderless
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . detvdl/lsp-mode-setup-completion)))

;; Source: https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("M-p" . lsp-signature-activate))
  :custom
  (lsp-ui-flycheck-enable t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-lens-enable t)
  (lsp-modeline-diagnostics-enable t)
  )

;;;; Handle performance for very long lines (so-long.el)
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;;; Markdown (markdown-mode)
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc")
  (setq markdown-fontify-code-blocks-natively t)
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
  :ensure t)

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.y[a]?ml\\'")

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'")

(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+\\|\n" nil t)
        (replace-match " ")))))

(with-eval-after-load 'crux
  (crux-with-region-or-buffer json-to-single-line))

(use-package pet
  :ensure t)

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (pet-mode)
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))

;;; csv-mode
(use-package csv-mode
  :ensure t
  :commands (csv-align-mode))

;;; Flyspell and prot-spell.el (spell check)
(use-package flyspell
  :ensure nil
  :bind
  ( :map flyspell-mode-map
    ("C-;" . nil)
    :map flyspell-mouse-map
    ("<mouse-3>" . flyspell-correct-word)
    :map ctl-x-x-map
    ("s" . flyspell-mode)) ; C-x x s
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_GB"))

(use-package prot-spell
  :ensure nil
  :bind
  (("M-$" . prot-spell-spell-dwim)
   ("C-M-$" . prot-spell-change-dictionary)
   ("M-i" . prot-spell-spell-dwim) ; override `tab-to-tab-stop'
   ("C-M-i" . prot-spell-change-dictionary)) ; override `complete-symbol'
  :config
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("EL Ελληνικά" . "el")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))

  ;; Also check prot-spell.el for what I am doing with
  ;; `prot-spell-ispell-display-buffer'.  Then refer to the
  ;; `display-buffer-alist' for the relevant entry.
  (setq ispell-choices-buffer "*ispell-top-choices*"))

;;; Flymake
(use-package flymake
  :ensure nil
  :preface
  (defvar prot/flymake-mode-projects-path
    (file-name-as-directory (expand-file-name "Projects" "~/Git/"))
    "Path to my Git projects.")

  (defun prot/flymake-mode-lexical-binding ()
    (when lexical-binding
      (flymake-mode 1)))

  (defun prot/flymake-mode-in-my-projects ()
    (when-let* ((file (buffer-file-name))
                ((string-prefix-p prot/flymake-mode-projects-path (expand-file-name file)))
                ((not (file-directory-p file)))
                ((file-regular-p file)))
      (add-hook 'find-file-hook #'prot/flymake-mode-lexical-binding nil t)))

  (add-hook 'emacs-lisp-mode-hook #'prot/flymake-mode-in-my-projects)
  :bind
  ( :map ctl-x-x-map
    ("m" . flymake-mode) ; C-x x m
    :map flymake-mode-map
    ("C-c ! s" . flymake-start)
    ("C-c ! d" . flymake-show-buffer-diagnostics) ; Emacs28
    ("C-c ! D" . flymake-show-project-diagnostics) ; Emacs28
    ("C-c ! n" . flymake-goto-next-error)
    ("C-c ! p" . flymake-goto-prev-error))
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  ;; NOTE 2023-07-03: `prot-modeline.el' actually defines the counters
  ;; itself and ignores this.
  (setq flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))
  (setq flymake-show-diagnostics-at-end-of-line nil)) ; Emacs 30

;;; Elisp packaging requirements
(use-package package-lint-flymake
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; General configurations for prose/writing

;;;; `outline' (`outline-mode' and `outline-minor-mode')
(use-package outline
  :ensure nil
  :bind
  ("<f10>" . outline-minor-mode)
  :config
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t) ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil)) ; as above

;;;; `dictionary'
(use-package dictionary
  :ensure nil
  :bind ("C-c d" . dictionary-search)
  :config
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev" ; read doc string
        dictionary-create-buttons nil
        dictionary-use-single-buffer t))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(use-package logos
  :ensure t
  :bind
  (("C-x n n" . logos-narrow-dwim)
   ("C-x ]" . logos-forward-page-dwim)
   ("C-x [" . logos-backward-page-dwim)
   ;; I don't think I ever saw a package bind M-] or M-[...
   ("M-]" . logos-forward-page-dwim)
   ("M-[" . logos-backward-page-dwim)
   ("<f9>" . logos-focus-mode))
  :config
  (setq logos-outlines-are-pages t)
  (setq logos-outline-regexp-alist
        `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
          (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
          (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
          (conf-toml-mode . "^\\[")))

  ;; These apply when `logos-focus-mode' is enabled.  Their value is
  ;; buffer-local.
  (setq-default logos-hide-mode-line t)
  (setq-default logos-hide-header-line t)
  (setq-default logos-hide-buffer-boundaries t)
  (setq-default logos-hide-fringe t)
  (setq-default logos-variable-pitch t) ; see my `fontaine' configurations
  (setq-default logos-buffer-read-only nil)
  (setq-default logos-scroll-lock nil)
  (setq-default logos-olivetti t)

  (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)

;;;; Extra tweaks
  ;; place point at the top when changing pages, but not in `prog-mode'
  (defun prot/logos--recenter-top ()
    "Use `recenter' to reposition the view at the top."
    (unless (derived-mode-p 'prog-mode)
      (recenter 1))) ; Use 0 for the absolute top

  (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

(provide 'detvdl-emacs-langs)

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load: search below for the files
;; prot--pre-custom.el and prot--post-custom.el
(defgroup detvdl-emacs nil
  "User options for my dotemacs.
These produce the expected results only when set in a file called
prot--pre-custom.el.  This file must be in the same
directory as the init.el."
  :group 'file)

(defcustom detvdl-emacs-load-theme-family 'modus
  "Set of themes to load.
Valid values are the symbols `doric', `ef', `modus', and `standard',
which reference the `doric-themes', `ef-themes', `modus-themes', and
`standard-themes', respectively.

A nil value does not load any of the above (use Emacs without a
theme).

This user option must be set in the `prot--pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'detvdl-emacs
  :type '(choice :tag "Set of themes to load" :value modus
                 (const :tag "The `doric-themes' module" doric)
                 (const :tag "The `ef-themes' module" ef)
                 (const :tag "The `modus-themes' module" modus)
                 (const :tag "The `standard-themes' module" standard)
                 (const :tag "Do not load a theme module" nil)))

(defcustom detvdl-emacs-completion-ui 'vertico
  "Choose minibuffer completion UI between `default' or `vertico'.
If the value is nil, the default completion user interface is
used.  On Emacs 30, this is close the experience with `mct'.

This user option must be set in the `detvdl-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'detvdl-emacs
  :type '(choice :tag "Minibuffer user interface"
                 (const :tag "Default user interface" nil)
                 (const :tag "The `vertico' module" vertico)))

(defcustom detvdl-emacs-completion-extras t
  "When non-nil load extras for minibuffer completion.
These include packages such as `consult' and `embark'."
  :group 'detvdl-emacs
  :type 'boolean)

(defcustom detvdl-emacs-treesitter-extras t
  "When non-nil load extras for tree-sitter integration
These include packages such as `expreg' and generally anything
that adds functionality on top of what the major mode provides."
  :group 'detvdl-emacs
  :type 'boolean)

(defcustom detvdl-emacs-load-which-key t
  "When non-nil, display key binding hints after a short delay.
This user option must be set in the `detvdl-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'detvdl-emacs
  :type 'boolean)

(defcustom detvdl-emacs-load-icons t
  "When non-nil, enable iconography in various contexts.
This installs and uses the `nerd-icons' package and its variants.
NOTE that you still need to invoke `nerd-icons-install-fonts'
manually to first get the icon files.

This user option must be set in the `detvdl-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'detvdl-emacs
  :type 'boolean)

(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      (format ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
              'lisp-interaction-mode
              (propertize
               (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
               'face 'help-key-binding)))

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("detvdl-lisp" "detvdl-emacs-modules"))

;;;; Packages

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; NOTE 2023-08-21: I build Emacs from source, so I always get the
;; latest version of built-in packages.  However, this is a good
;; solution to set to non-nil if I ever switch to a stable release.
(setq package-install-upgrade-built-in nil)

(defvar detvdl-emacs-my-packages
  '()
  "List of symbols representing the packages I develop/maintain.")

(setq package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "gnu-elpa-devel"))
           detvdl-emacs-my-packages)))

;; Temp: Explicitly set PATH environment variable and update exec-path to match it.
;; Source environment variables from init shell on non-shell based init systems
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :demand t
  :custom
  (exec-path-from-shell-variables '("HOME" "PATH" "MANPATH"
                                    "PAGER" "TERM" "GPG_TTY"
                                    "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                                    "LANGUAGE" "LANG" "LC_CTYPE" "LC_ALL"
                                    "OPAM_SWITCH_PREFIX"))
  (exec-path-from-shell-arguments '("--login"))
  :config
  (exec-path-from-shell-initialize))

;; Disable the damn thing by making it disposable.
(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq custom-safe-themes t)

(defmacro detvdl-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let* (((keymapp ,keymap))
                 (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

;; Sample of `detvdl-emacs-keybind'

;; (detvdl-emacs-keybind global-map
;;   "C-z" nil
;;   "C-x b" #'switch-to-buffer
;;   "C-x C-c" nil
;; ;; Notice the -map as I am binding keymap here, not a command:
;;   "C-c b" beframe-prefix-map
;;   "C-x k" #'kill-buffer)

(defmacro detvdl-emacs-abbrev (table &rest definitions)
  "Expand abbrev DEFINITIONS for the given TABLE.
DEFINITIONS is a sequence of (i) string pairs mapping the
abbreviation to its expansion or (ii) a string and symbol pair
making an abbreviation to a function."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  `(if (abbrev-table-p ,table)
       (progn
         ,@(mapcar
            (lambda (pair)
              (let ((abbrev (nth 0 pair))
                    (expansion (nth 1 pair)))
                (if (stringp expansion)
                    `(define-abbrev ,table ,abbrev ,expansion)
                  `(define-abbrev ,table ,abbrev "" ,expansion))))
            (seq-split definitions 2)))
     (error "%s is not an abbrev table" ,table)))

(defvar detvdl-emacs-package-form-regexp
  "^(\\(detvdl-emacs-keybind\\|detvdl-emacs-abbrev\\) +'?\\([0-9a-zA-Z-]+\\)"
  "Regexp to add packages to `lisp-imenu-generic-expression'.")

(eval-after-load 'lisp-mode
  `(add-to-list 'lisp-imenu-generic-expression
                (list "Packages" ,detvdl-emacs-package-form-regexp 2)))

(defconst detvdl-emacs-font-lock-keywords
  '(("(\\(detvdl-emacs-\\(keybind\\|abbrev\\)\\)\\_>[ \t']*\\(\\(\\sw\\|\\s_\\)+\\)?"
     (3 font-lock-variable-name-face nil t))
    ("(\\(detvdl-emacs-comment\\)\\_>[ \t']*"
     (1 font-lock-preprocessor-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode detvdl-emacs-font-lock-keywords)

(require 'detvdl-emacs-theme)
(require 'detvdl-emacs-essentials)
(require 'detvdl-emacs-modeline)
(require 'detvdl-emacs-completion)
(require 'detvdl-emacs-search)
(require 'detvdl-emacs-dired)
(require 'detvdl-emacs-window)
(require 'detvdl-emacs-git)
(require 'detvdl-emacs-org)
(require 'detvdl-emacs-langs)
(when detvdl-emacs-load-which-key
  (require 'detvdl-emacs-which-key))
(when detvdl-emacs-load-icons
  (require 'detvdl-emacs-icons))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/detvdl-emacs-post-custom.el
;;
;; The purpose of the "post customisations" is to make tweaks to what
;; I already define, such as to change the default theme.  See above
;; for the `detvdl-emacs-pre-custom.el' to make changes BEFORE loading
;; any of my other configurations.
(load (locate-user-emacs-file "detvdl-emacs-post-custom.el") :no-error :no-message)

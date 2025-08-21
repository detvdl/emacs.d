;;; Essential configurations
(use-package emacs
  :ensure nil
  :demand t
  :config
;;;; General settings and common custom functions (prot-simple.el)
  (setq blink-matching-paren nil)
  (blink-cursor-mode -1)
  (setq custom-unlispify-tag-names nil)
  (setq delete-pair-blink-delay 0.1) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
  (setq delete-pair-push-mark t) ; Emacs 31
  (setq echo-keystrokes-help nil) ; Emacs 30
  (setq epa-keys-select-method 'minibuffer) ; Emacs 30
  (setq eval-expression-print-length nil)
  (setq find-library-include-other-files nil) ; Emacs 29
  (setq help-window-select t)
  (setq kill-do-not-save-duplicates t)
  (setq mode-require-final-newline 'visit-save)
  (setq next-error-recenter '(4)) ; center of the window
  (setq remote-file-name-inhibit-auto-save t)                 ; Emacs 30
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
  (setq save-interprogram-paste-before-kill t)
  (setq scroll-error-top-bottom t)
  (setq tramp-connection-timeout (* 60 10)) ; seconds
  (setq trusted-content '("~/Code" "~/Git")) ; Emacs 30
  (setq-default truncate-partial-width-windows nil)
  (setq mac-command-modifier 'meta
      mac-option-modifier 'super)
  ;; Keys I unbind here are either to avoid accidents or to bind them
  ;; elsewhere later in the configuration.
  :bind
  ( :map global-map
    ("<f2>" . toggle-input-method)  ; F2 overrides that two-column gimmick.  Sorry, but no!
    ("<insert>" . nil)
    ("<menu>" . nil)
    ("C-x C-d" . nil) ; never use it
    ("C-x C-v" . nil) ; never use it
    ("C-z" . nil) ; I have a window manager, thanks!
    ("C-x C-z" . nil) ; same idea as above
    ("C-x C-r" . restart-emacs) ; override `find-file-read-only'
    ("C-h h" . nil) ; Never show that "hello" file
    ("M-`" . nil)
    ("M-o" . delete-blank-lines) ; alias for C-x C-o
    ("M-SPC" . cycle-spacing)
    ("M-z" . zap-up-to-char) ; NOT `zap-to-char'
    ("M-c" . capitalize-dwim)
    ("M-l" . downcase-dwim) ; "lower" case
    ("M-u" . upcase-dwim)
    ("M-=" . count-words)
    ("C-x O" . next-multiframe-window)
    ("C-h K" . describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
    ("C-h u" . apropos-user-option)
    ("C-h F" . apropos-function) ; lower case is `describe-function'
    ("C-h V" . apropos-variable) ; lower case is `describe-variable'
    ("C-h L" . apropos-library) ; lower case is `view-lossage'
    ("C-h c" . describe-char) ; overrides `describe-key-briefly'
    ("<kp-delete>" . delete-char)
    :map prog-mode-map
    ("C-M-d" . up-list) ; confusing name for what looks like "down" to me
    ("<C-M-backspace>" . backward-kill-sexp)

    ;; Keymap for buffers (Emacs28)
    :map ctl-x-x-map
    ("f" . follow-mode)  ; override `font-lock-update'
    ("r" . rename-uniquely)
    ("l" . visual-line-mode)))

(use-package crux
  :ensure t
  :bind (([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ("C-a" . crux-move-beginning-of-line)
         ("C-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards))
  :config
  (crux-reopen-as-root-mode)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  )

;; (use-package mise
;;   :ensure t
;;   :ensure-system-package (mise . "brew install mise")
;;   :hook (after-init . global-mise-mode))

(use-package prot-common
  :ensure nil
  :functions (prot-common-truncate-lines-silently)
  :hook ((text-mode prog-mode dired-mode prot/fundamental-mode hexl-mode comint-mode) . prot-common-truncate-lines-silently)
  :init
  (defvar prot/fundamental-mode-hook nil
    "Normal hook for `fundamental-mode' (which is missing by default).")

  (defun prot/fundamental-mode-run-hook (&rest args)
    "Apply ARGS and then run `prot/fundamental-mode-hook'."
    (apply args)
    (run-hooks 'prot/fundamental-mode-hook))

  (advice-add #'fundamental-mode :around #'prot/fundamental-mode-run-hook)
  :config
  ;; NEVER tell me which key can call a command that I specifically
  ;; invoked with M-x: I have a good reason to use it that way.
  (advice-add #'execute-extended-command--describe-binding-msg :override #'prot-common-ignore))

(use-package prot-simple
  :ensure nil
  :demand t
  :config
  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")

  (advice-add #'save-buffers-kill-emacs :before #'prot-simple-display-unsaved-buffers-on-exit)

  ;; All `prot-simple-override-mode' does is activate a key map.
  ;; Below I add keys to that map.  Because the mode is enabled
  ;; globally, those keys take precedence over the ones specified by
  ;; any given major mode.  In principle, this means that my keys will
  ;; always work (though technically they can be overriden by another
  ;; minor mode, depending on which one is evaluated last).
  (prot-simple-override-mode 1)

  :bind
  ( :map prot-simple-override-mode-map
    ("C-v" . prot-simple-multi-line-below) ; overrides `scroll-up-command'
    ("<next>" . prot-simple-multi-line-below) ; overrides `scroll-up-command'
    ("M-v" . prot-simple-multi-line-above) ; overrides `scroll-down-command'
    ("<prior>" . prot-simple-multi-line-above) ; overrides `scroll-down-command'

    :map global-map
    ("<escape>" . prot-simple-keyboard-quit-dwim)
    ("C-g" . prot-simple-keyboard-quit-dwim)
    ("C-M-SPC" . prot-simple-mark-sexp)   ; will be overriden by `expreg' if tree-sitter is available
    ;; ("C-," . prot-simple-mark-sexp)   ; I also have `isearch-forward-symbol-at-point' on C-.
    ;; Commands for lines
    ("C-S-d" . prot-simple-delete-line-backward)
    ("C-S-k" . prot-simple-kill-line-backward)
    ("M-k" . prot-simple-copy-line-forward)
    ("M-K" . prot-simple-copy-line-backward)
    ("M-j" . delete-indentation)
    ("C-w" . prot-simple-kill-region)
    ("M-w" . prot-simple-kill-ring-save)

    ("C-S-w" . prot-simple-copy-line)
    ("C-S-y" . prot-simple-yank-replace-line-or-region)
    ("<C-return>" . prot-simple-new-line-below)
    ("<C-S-return>" . prot-simple-new-line-above)
    ("C-x x a" . prot-simple-auto-fill-visual-line-mode) ; auto-fill/visual-line toggle
    ;; Commands for text insertion or manipulation
    ;; ("C-=" . prot-simple-insert-date)
    ("C-<" . prot-simple-escape-url-dwim)
    ;; "C->" prot-simple-insert-line-prefix-dwim
    ("M-Z" . prot-simple-zap-to-char-backward)
    ;; Commands for object transposition
    ("C-S-p" . prot-simple-move-above-dwim)
    ("C-S-n" . prot-simple-move-below-dwim)
    ("C-t" . prot-simple-transpose-chars)
    ("C-x C-t" . prot-simple-transpose-lines)
    ("C-S-t" . prot-simple-transpose-paragraphs)
    ("C-x M-t" . prot-simple-transpose-sentences)
    ("C-M-t" . prot-simple-transpose-sexps)
    ("M-t" . prot-simple-transpose-words)
    ;; Commands for paragraphs
    ("M-Q" . prot-simple-unfill-region-or-paragraph)
    ;; Commands for windows and pages
    ("C-x o" . prot-simple-other-window)
    ("C-x n k" . prot-simple-delete-page-delimiters)
    ("M-r" . window-layout-transpose) ; Emacs 31 override `move-to-window-line-top-bottom'
    ("M-S-r" . rotate-windows-back) ; Emacs 31
    ;; Commands for buffers
    ("<C-f2>" . prot-simple-rename-file-and-buffer)
    ("C-x k" . prot-simple-kill-buffer-current)
    ("C-x K" . kill-buffer) ; leaving this here to contrast with the above
    ("M-s b" . prot-simple-buffers-major-mode)
    ("M-s v" . prot-simple-buffers-vc-root)
    ;; Commands for files
    ("C-x r ." . prot-simple-file-to-register)))

;;;; Scratch buffers per major mode (prot-scratch.el)
(use-package prot-scratch
  :ensure nil
  :bind ("C-c s" . prot-scratch-buffer)
  :config
  (setq prot-scratch-default-mode 'text-mode))

;;;; Insert character pairs (prot-pair.el)
(use-package prot-pair
  :ensure nil
  :bind
  (("C-'" . prot-pair-insert)
   ("M-'" . prot-pair-insert-directly)
   ("M-\\" . prot-pair-delete)))

;;;; Prefix keymap (prot-prefix.el)
(use-package prot-prefix
  :ensure nil
  :bind-keymap
  (("<insert>" . prot-prefix)
   ("C-z" . prot-prefix)))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25) ; I don't use the `menu-bar-mode', but this is good to know
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil))

;;;; Mouse and mouse wheel behaviour
(use-package mouse
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  :config
  ;; Some of these variables are defined in places other than
  ;; mouse.el, but this is fine.
  (setq mouse-autoselect-window t) ; complements the auto-selection of my tiling window manager
  (setq focus-follows-mouse t)

  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale))
        mouse-drag-copy-region nil
        make-pointer-invisible t
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)

  ;; Scrolling behaviour
  (setq-default scroll-preserve-screen-position t
                scroll-conservatively 100000 ; affects `scroll-step'
                scroll-margin 0
                next-screen-context-lines 2))

;;;; Repeatable key chords (repeat-mode)
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t))

;;;; Registers (register.el)
(use-package register
  :ensure nil
  :defer t ; its commands are autoloaded, so this will be loaded then
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'register-preview-default)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;;;; Auto revert mode
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

;;;; Delete selection
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;;;; Tooltips (tooltip-mode)
(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips t
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

;;;; `man' (manpages)
(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'

;;; Substitute
;; Another package of mine... Video demo:
;; <https://protesilaos.com/codelog/2023-01-16-emacs-substitute-package-demo/>.
(use-package substitute
  :ensure t
  :defer 1
  ;; Produce a message after the substitution that reports on what
  ;; happened.  It is a single line, like "Substituted `TARGET' with
  ;; `SUBSTITUTE' N times across the buffer.
  :hook (substitute-post-replace . substitute-report-operation)
  :commands
  (substitute-target-below-point ; Forward motion like isearch (C-s)
   substitute-target-above-point ; Backward motion like isearch (C-r)
   substitute-target-in-defun    ; inside of the current definition
   substitute-target-in-buffer)  ; throughout the buffer
  :config
  ;; Set this to non-nil to highlight all occurrences of the current
  ;; target.
  (setopt substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally.  Otherwise each command accepts a `C-u' prefix
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case nil)

  ;; C-c s is occupied by `prot-scratch-buffer'.
  (define-key global-map (kbd "C-c r") #'substitute-prefix-map))

(use-package goto-chg
  :ensure t
  :bind
  (("C-(" . goto-last-change)
   ("C-)" . goto-last-change-reverse)))

;;; Mark syntactic constructs efficiently if tree-sitter is available (expreg)
(when (and (treesit-available-p) detvdl-emacs-treesitter-extras)
  (use-package expreg
    :ensure t
    :functions (prot/expreg-expand prot/expreg-expand-dwim)
    ;; There is also an `expreg-contract' command, though I have no use for it.
    :bind ("C-=" . prot/expreg-expand-dwim) ; overrides `mark-sexp'
    :config
    (defun prot/expreg-expand (n)
      "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
      (interactive "p")
      (dotimes (_ n)
        (expreg-expand)))

    (defun prot/expreg-expand-dwim ()
      "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
      (interactive)
      (let ((symbol (bounds-of-thing-at-point 'symbol)))
        (cond
         ((equal (bounds-of-thing-at-point 'word) symbol)
          (prot/expreg-expand 1))
         (symbol (prot/expreg-expand 2))
         (t (expreg-expand)))))))

(use-package multiple-cursors
  :ensure t
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
         ("M-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package iedit
  :ensure t
  :commands (iedit-mode)
  :bind (("C-;" . iedit-mode)
         ("C-:" . iedit-dwim)
         :map iedit-mode-keymap
         ("C-g" . iedit--quit))
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

;;; Generic interface for shells or REPLs (comint)
(use-package comint
  :ensure nil
  ;; Support for OS-specific escape sequences such as what `ls
  ;; --hyperlink' uses.  I normally don't use those, but I am checking
  ;; this to see if there are any obvious advantages/disadvantages.
  :hook
  (comint-output-filter-functions . comint-osc-process-output)
  :config
  (setq ansi-color-for-comint-mode t) ; also see `ansi-color-for-compilation-mode'
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 9999)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input))

;;; Compilation interface (M-x compile)
(use-package compile
  :ensure nil
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq ansi-color-for-compilation-mode t)) ; also see `ansi-color-for-comint-mode'

;;; Standard Unix Shell (M-x shell)
(use-package shell
  :ensure nil
  :bind
  ( :map shell-mode-map
    ("C-c C-k" . comint-clear-buffer)
    ("C-c C-w" . comint-write-output))
  :config
  ;; Check my .bashrc which handles `comint-terminfo-terminal':
  ;;
  ;; # Default pager.  The check for the terminal is useful for Emacs with
  ;; # M-x shell (which is how I usually interact with bash these days).
  ;; #
  ;; # The COLORTERM is documented in (info "(emacs) General Variables").
  ;; # I found the reference to `dumb-emacs-ansi' in (info "(emacs)
  ;; # Connection Variables").
  ;; if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]
  ;; then
  ;;     export PAGER="cat"
  ;;     alias less="cat"
  ;;     export TERM=dumb-emacs-ansi
  ;;     export COLORTERM=1
  ;; else
  ;;     # Quit once you try to scroll past the end of the file.
  ;;     export PAGER="less --quit-at-eof"
  ;; fi
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t) ; Emacs 29.1
  (setq shell-has-auto-cd nil) ; Emacs 29.1
  (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (setq shell-kill-buffer-on-exit t) ; Emacs 29.1
  (setq shell-completion-fignore '("~" "#" "%"))
  (setq tramp-default-remote-shell "/bin/bash")

  (setq shell-font-lock-keywords
        '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
          ("^[^ \t\n]+:.*" . font-lock-string-face)
          ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face))))

(use-package prot-shell
  :ensure nil
  :bind (("<f1>" . prot-shell)) ; I don't use F1 for help commands
  :hook (shell-mode . prot-shell-mode))

(provide 'detvdl-emacs-essentials)

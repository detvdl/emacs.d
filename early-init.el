(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(dolist (variable '(initial-frame-alist default-frame-alist))
  (set variable `((width . 135)
                  (height . 60)
                  (horizontal-scroll-bars . nil)
                  (menu-bar-lines . 1) ; alternative to disabling `menu-bar-mode'
                  (internal-border-width . 5)
                  (left-fringe    . 2)
                  (right-fringe   . 1)
                  (tool-bar-lines . 0)
                  ,@(if x-toolkit-scroll-bars
                        (list
                         '(vertical-scroll-bars . nil)
                         '(scroll-bar-width . 12))
                      (list
                       '(vertical-scroll-bars . right)
                       '(scroll-bar-width . 6))))))

(defun emacs-no-minibuffer-scroll-bar (frame)
  "Remove the minibuffer scroll bars from FRAME."
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent)))

(add-hook 'after-make-frame-functions #'emacs-no-minibuffer-scroll-bar)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar emacs--file-name-handler-alist file-name-handler-alist)
(defvar emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 100 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist emacs--file-name-handler-alist
                  vc-handled-backends emacs--vc-handled-backends)))

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t)

;;;; General theme code

(defun emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`emacs-avoid-initial-flash-of-light'."
  (when-let* ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

;; NOTE 2023-02-05: The reason the following works is because (i) the
;; `mode-line-format' is specified again and (ii) the
(defun emacs-avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed.
New frames are instructed to call `prot--re-enable-frame-theme'."
  (progn
    (setq mode-line-format nil)
    (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
    (add-hook 'after-make-frame-functions #'emacs-re-enable-frame-theme)))

(emacs-avoid-initial-flash-of-light)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

;; Temp: Explicitly set PATH environment variable and update exec-path to match it.
;; (the string here should be copied from the PATH in Emacs.app/Contents/Info.plist)
(setenv "PATH" "/Users/detlevvandaele/.codeium/windsurf/bin:/Users/detlevvandaele/.opam/default/bin:/Users/detlevvandaele/.asdf/shims:/Users/detlevvandaele/.asdf/bin:/Users/detlevvandaele/.local/bin:/opt/homebrew/opt/bison/bin:/opt/homebrew/opt/make/libexec/gnubin:/opt/homebrew/opt/libtool/libexec/gnubin:/opt/homebrew/opt/gsed/libexec/gnubin:/opt/homebrew/opt/grep/libexec/gnubin:/opt/homebrew/opt/gnu-tar/libexec/gnubin:/opt/homebrew/opt/gnu-sed/libexec/gnubin:/opt/homebrew/opt/gnu-indent/libexec/gnubin:/opt/homebrew/opt/gawk/libexec/gnubin:/opt/homebrew/opt/findutils/libexec/gnubin:/opt/homebrew/opt/coreutils/libexec/gnubin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/TeX/texbin:/Users/detlevvandaele/Applications/iTerm.app/Contents/Resources/utilities:/Users/detlevvandaele/Library/Application Support/JetBrains/Toolbox/scripts:/opt/homebrew/opt/fzf/bin")
(setq exec-path (split-string (getenv "PATH") path-separator))

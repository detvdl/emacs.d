;;; init.el --- Fuck

;;; Commentary:

;;; Code:

;; Always load newest byte code

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-prefer-newer t)

(defvar emacs-dir (file-name-directory load-file-name))
(defvar emacs-core-dir (expand-file-name "core" emacs-dir))
(defvar emacs-modules-lang-dir (expand-file-name "modules/lang" emacs-dir))
(defvar emacs-modules-util-dir (expand-file-name "modules/util" emacs-dir))
(defvar emacs-themes-dir (expand-file-name "theme"))
(defvar emacs-savefile-dir (expand-file-name "savefile" emacs-dir))


;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path emacs-core-dir)
(add-to-list 'load-path emacs-modules-lang-dir)
(add-to-list 'load-path emacs-modules-util-dir)

;; reduce the frequency of garbage collection
;; default: 0.79MB
(setq gc-cons-threshold 5000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defvar detvdl-auto-save t)

;; Always load this first, as it provides use-package functionality
(require 'detvdl-packages)
(require 'detvdl-editor)
(require 'detvdl-ui)
(require 'detvdl-lang)
(require 'detvdl-util)

;; start server, so we can connect anytime with emacsclient
;; (unless noninteractive
;;   (setq server-socket-dir (format "/tmp/emacs-%d-%s-%d"
;;                                   (user-uid)
;;                                   (format-time-string "%Y%m%d-%H%M%S")
;;                                   (emacs-pid)))
;;   (server-start)
;;   (add-hook 'kill-emacs-hook #'(lambda () (delete-directory server-socket-dir t))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2cf7f9d1d8e4d735ba53facdc3c6f3271086b6906c4165b12e4fd8e3865469a6" default)))
 '(package-selected-packages
   (quote
    (elisp-slime-nav js2-mode rainbow-mode projectile-ripgrep company-anaconda pyenv-mode cider rainbow-delimiters drag-stuff use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

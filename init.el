;;; init.el --- Fuck
;;; Commentary:
;;; Code:

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/"))
      package-enable-at-startup nil
      package--init-file-ensured t)

(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

(mapc #'(lambda (add) (add-to-list 'load-path add))
      (eval-when-compile
        (package-initialize)
        ;; Install use-package if not installed yet.
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        (setq use-package-always-ensure t)
        (let ((package-user-dir-real (file-truename package-user-dir)))
          ;; The reverse is necessary, because outside we mapc
          ;; add-to-list element-by-element, which reverses.
          (nreverse (apply #'nconc
                           ;; Only keep package.el provided loadpaths.
                           (mapcar #'(lambda (path)
                                       (if (string-prefix-p package-user-dir-real path)
                                           (list path)
                                         nil))
                                   load-path))))))

;; Explicitly use in case of byte-compiled init-file
(use-package diminish :ensure t)
(use-package bind-key :ensure t)
(use-package use-package :ensure t)

;; Always load newest byte code
(setq load-prefer-newer t)

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|127.0.0.1\\)")))

(defvar emacs-core-dir (expand-file-name "core" user-emacs-directory))
(defvar emacs-modules-lang-dir (expand-file-name "modules/lang" user-emacs-directory))
(defvar emacs-modules-util-dir (expand-file-name "modules/util" user-emacs-directory))
(defvar emacs-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(defvar emacs-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))

;; add directories to Emacs' `load-path'
(add-to-list 'load-path emacs-core-dir)
(add-to-list 'load-path emacs-lisp-dir)
(add-to-list 'load-path emacs-modules-lang-dir)
(add-to-list 'load-path emacs-modules-util-dir)

;; reduce the frequency of garbage collection
;; default: 0.79MB
(setq gc-cons-threshold 5000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; use the auth-source API to resolve secrets and passwords from
;; (possibly GPG encrypted) files
(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

(require 'detvdl-editor)
(require 'detvdl-ui)
(require 'detvdl-shell)
(require 'detvdl-lang)
(require 'detvdl-util)

;; write custom-set-variables to a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(defvar open-at-start '())
(setq open-at-start '("~/Documents/TODO.org"))
;; (dolist (f open-at-start)
  ;; (find-file f))

;;; init.el ends here


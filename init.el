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
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file)))
  (mapc #'(lambda (add) (add-to-list 'load-path add))
        (eval-when-compile
          (package-initialize)
          ;; Install use-package if not installed yet.
          (unless (package-installed-p 'use-package)
            (package-refresh-contents)
            (package-install 'use-package))
          (setq  use-package-enable-imenu-support t
                 use-package-always-ensure t)
          (let ((package-user-dir-real (file-truename package-user-dir)))
            ;; The reverse is necessary, because outside we mapc
            ;; add-to-list element-by-element, which reverses.
            (nreverse (apply #'nconc
                             ;; Only keep package.el provided loadpaths.
                             (mapcar #'(lambda (path)
                                         (if (string-prefix-p package-user-dir-real path)
                                             (list path)
                                           nil))
                                     load-path)))))))
;; Explicitly use in case of byte-compiled init-file
(use-package diminish :ensure t)
(use-package bind-key :ensure t)
(use-package use-package :ensure t)

;; Always load newest byte code
(setq load-prefer-newer t)

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|127.0.0.1\\)")))

(defvar detvdl:load-paths
  (mapcar (lambda (p) (concat user-emacs-directory p))
          '("core"
            "modules/lang"
            "modules/util"
            "modules/themes"
            "site-lisp")))
(mapc (apply-partially 'add-to-list 'load-path) detvdl:load-paths)

(defvar emacs-savefile-dir (expand-file-name "savefile" user-emacs-directory))

(add-to-list 'custom-theme-load-path (expand-file-name "modules/themes/custom-themes" user-emacs-directory))

(setq backup-directory-alist `((".*" . ,(expand-file-name "auto-save-list" emacs-savefile-dir)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list" emacs-savefile-dir))))

(setq nsm-settings-file (expand-file-name "network-security.data" emacs-savefile-dir))

;; reduce the frequency of garbage collection
;; default: 0.79MB
(setq gc-cons-threshold (* 200 1024 1024))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; use the auth-source API to resolve secrets and passwords from
;; (possibly GPG encrypted) files
(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

;; Use GNUs to keep track of emacs issues

(setq gnus-select-method '(nntp "GNUS"
                                (nntp-address "news.gnus.org")))

(require 'detvdl-core)
(require 'detvdl-lang)
(require 'detvdl-util)
(require 'detvdl-theme)

;; write custom-set-variables to a separate file
(setq custom-file (expand-file-name "custom.el" emacs-savefile-dir))
(load custom-file 'noerror)

;;; init.el ends here
(put 'downcase-region 'disabled nil)

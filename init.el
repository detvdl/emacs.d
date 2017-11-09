;;; init.el --- Fuck
;;; Commentary:
;;; Code:

(defvar emacs-dir (file-name-directory load-file-name))
(setq package-user-dir (expand-file-name "elpa" emacs-dir)
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

(defvar emacs-core-dir (expand-file-name "core" emacs-dir))
(defvar emacs-modules-lang-dir (expand-file-name "modules/lang" emacs-dir))
(defvar emacs-modules-util-dir (expand-file-name "modules/util" emacs-dir))
(defvar emacs-themes-dir (expand-file-name "themes"))
(defvar emacs-savefile-dir (expand-file-name "savefile" emacs-dir))
(defvar emacs-persp-dir (expand-file-name ".persp-confs/" emacs-dir))

;; add directories to Emacs' `load-path'
(add-to-list 'load-path emacs-core-dir)
(add-to-list 'load-path emacs-modules-lang-dir)
(add-to-list 'load-path emacs-modules-util-dir)
(add-to-list 'load-path emacs-themes-dir)

;; reduce the frequency of garbage collection
;; default: 0.79MB
(setq gc-cons-threshold 5000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; autosave focused buffer when it loses focus
(defvar detvdl-auto-save t)

(require 'detvdl-editor)
(require 'detvdl-ui)
(require 'detvdl-lang)
(require 'detvdl-util)

;; write custom-set-variables to a separate file
(setq custom-file (expand-file-name "custom.el" emacs-dir))
(load custom-file 'noerror)

;;; init.el ends here

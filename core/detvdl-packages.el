;;; detvdl-packages.el --- Packages
;;; Commentary:
;;; Code:

(defvar emacs-dir
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(setq package-user-dir (expand-file-name "elpa" emacs-dir)
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil
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
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

(provide 'detvdl-packages)
;;; detvdl-packages.el ends here

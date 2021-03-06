;;; paths.init.el -- Platform-specific paths

;;; Commentary:
;;; This sets up various vars that point to environment-specific
;;; paths.  For instance, we set up the path to the local Eclipse
;;; location, as well git.

;;; Code:

;; These are just filler, as we don't yet have a local Linux
;; installation to test this under

(defun parse-version-in-directory (dir-name)
  "Extract JDK version from DIR-NAME."
  (if (string-match "\\(\\(1.[[:digit:]]\\)\\|\\([[:digit:]]+.[[:digit:]]+\\)\\).[[:digit:]]+\\(_[[:digit:]]+\\)?"
                    dir-name)
      (match-string 1 dir-name)))

(setq eclipse-dir "/usr/share/eclipse")
(setq jvm-homes-alist
      (let ((base "/usr/lib/jvm/"))
        (mapcar (lambda (candidate)
                  (let ((path (concat base candidate))
                        (version (parse-version-in-directory candidate)))
                    (cons version path)))
                (directory-files base nil
                                 "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))
(setq lombok-jar (expand-file-name "~/lombok/lombok.jar"))
(setq java-format-settings-file (expand-file-name "~/projects/defaultFormatterProfile.xml"))

(setq exec/git "/usr/bin/git")

(provide 'paths.init)

;;; paths.init.el ends here

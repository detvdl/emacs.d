;;; paths.init.el -- Platform-specific paths

;;; Commentary:
;;; This sets up various vars that point to environment-specific
;;; paths.  For instance, we set up the path to the local Eclipse
;;; location, as well git.

;;; Code:

(defun parse-version-in-directory (dir-name)
  "Extract JDK version from DIR-NAME."
  (if (string-match "\\(\\(1.[[:digit:]]\\)\\|\\([[:digit:]]+.[[:digit:]]+\\)\\).[[:digit:]]+\\(_[[:digit:]]+\\)?"
                    dir-name)
      (match-string 1 dir-name)))

(setq jvm-homes-alist
      (let ((base "/Library/Java/JavaVirtualMachines/"))
        (mapcar (lambda (candidate)
                  (let ((path (concat base candidate "/Contents/Home/"))
                        (version (parse-version-in-directory candidate)))
                    (cons version path)))
                (directory-files base nil
                                 "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))

(setq lombok-jar (expand-file-name "~/lombok/lombok.jar"))
(setq java-format-settings-file (expand-file-name "~/projects/defaultFormatterProfile.xml"))

(setq exec/git "/usr/local/bin/git")

(provide 'init-platform-paths)

;;; paths.init.el ends here

;;; detvdl-modeline.el --- Modeline customizations
;;; Commentary:
;;; Code:

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(diminish 'size-indication-mode)

(defun detvdl:-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:@-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1 :family ,(all-the-icons-octicon-family))
                 'display '(raise 0))
     (propertize (format " %s" branch)))))

(defvar detvdl:mode-line-vc
  '(:propertize
    (:eval (when vc-mode
             (cond
              ((string-match "Git[:@-]" vc-mode) (detvdl:-modeline-github-vc))
              (t (format "%s" vc-mode)))))
    face mode-line-buffer-id)
  "Formats the current directory.")

(defun detvdl:set-modeline ()
  (with-eval-after-load 'which-func
    (setq-default mode-line-format
                  `(" "
                    mode-line-mule-info
                    mode-line-modified
                    mode-line-frame-identification
                    mode-line-buffer-identification
                    "   "
                    mode-line-position
                    ,(when detvdl:mode-line-vc
                       (list
                        "  "
                        detvdl:mode-line-vc))
                    "   "
                    mode-line-modes
                    "   "
                    which-func-format)))

  (set-face-attribute 'mode-line nil
                      :box `(:line-width 2 :color ,(face-attribute 'mode-line :background))
                      :overline nil
                      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box `(:line-width 2 :color ,(face-attribute 'mode-line-inactive :background))
                      :overline nil
                      :underline nil))

(provide 'detvdl-modeline)
;;; detvdl-modeline.el ends here

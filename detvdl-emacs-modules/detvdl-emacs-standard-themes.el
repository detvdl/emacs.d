;;; The Standard themes

;; The themes are customisable.  Read the manual:
;; <https://protesilaos.com/emacs/standard-themes>.

(use-package standard-themes
  :ensure t
  :demand t
  :bind (("<f5>" . standard-themes-toggle)
         ("M-<f5>" . standard-themes-rotate))
  :config
  (setq standard-themes-bold-constructs t
        standard-themes-italic-constructs t
        standard-themes-mixed-fonts t
        standard-themes-variable-pitch-ui t

        ;; more complex alist to set weight, height, and optional
        ;; `variable-pitch' per heading level (t is for any level not
        ;; specified)
        standard-themes-headings
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch light 1.7))
          (3 . (variable-pitch semilight 1.6))
          (4 . (variable-pitch semilight 1.5))
          (5 . (variable-pitch 1.4))
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (1.3))
          (agenda-structure . (variable-pitch light 1.8))
          (t . (variable-pitch 1.1))))

  ;; Load a theme that is consistent with my session's theme.  Those
  ;; functions are defined in my init.el.
  (standard-themes-load-theme
   (if (detvdl-emacs-theme-environment-dark-p)
       'standard-dark
     'standard-light)))

(provide 'detvdl-emacs-standard-themes)

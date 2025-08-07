;;; The Ef (εὖ) themes

;; The themes are customisable.  Read the manual:
;; <https://protesilaos.com/emacs/ef-themes>.
(use-package ef-themes
  :ensure t
  :demand t
  :bind
  (("<f5>" . ef-themes-rotate)
   ("C-<f5>" . ef-themes-select))
  :config
  (setq ef-themes-variable-pitch-ui t
        ef-themes-mixed-fonts t
        ef-themes-to-rotate ef-themes-items
        ef-themes-headings ; read the manual's entry of the doc string
        '((0 . (variable-pitch light 1.9))
          (1 . (variable-pitch light 1.8))
          (2 . (variable-pitch regular 1.7))
          (3 . (variable-pitch regular 1.6))
          (4 . (variable-pitch regular 1.5))
          (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.3))
          (7 . (variable-pitch 1.2))
          (agenda-date . (semilight 1.5))
          (agenda-structure . (variable-pitch light 1.9))
          (t . (variable-pitch 1.1))))

  ;; The `ef-themes' provide lots of themes.  I want to pick one at
  ;; random when I start Emacs: the `ef-themes-load-random' does just
  ;; that (it can be called interactively as well).  I just check with
  ;; my desktop environment to determine if the choice should be about
  ;; a light or a dark theme.  Those functions are in my init.el.
  (if (detvdl-emacs-theme-environment-dark-p)
      (ef-themes-load-random 'dark)
    (ef-themes-load-random 'light)))

(provide 'detvdl-emacs-ef-themes)

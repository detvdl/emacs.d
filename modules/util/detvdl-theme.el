;;; detvdl-theme.el --- Themes and settings for theming
;;; Commentary:
;;; Code:

(use-package writeroom-mode
  :ensure t
  :bind (("C-c w" . writeroom-mode))
  :config
  (progn
    (setq writeroom-fringes-outside-margins nil
          writeroom-restore-window-config t)
    (add-hook 'writeroom-mode-hook
              (lambda () (if (bound-and-true-p writeroom-mode)
                             (display-line-numbers-mode -1)
                           (display-line-numbers-mode 1))))))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t
          sml/shorten-directory t
          sml/shorten-modes t
          sml/name-width 40
          sml/mode-width 'full
          sml/theme 'respectful)
    ;; (sml/setup)
    ))

;;; THEMES
(use-package plan9-theme
  :ensure t
  :init
  (progn
    (load-theme 'plan9 t)
    (custom-theme-set-faces
     'plan9
     `(fringe ((t (:background "#FFFFE8")))))))

(use-package doom-themes
  :ensure t
  :disabled t
  :defer t
  :config
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (doom-themes-org-config)))

(defun my/load-theme (frame)
  (select-frame frame)
  (load-theme 'plan9 t)
  (custom-theme-set-faces
   'plan9
   `(fringe ((t (:background "#FFFFE8")))))
  (sml/setup))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/load-theme)
  (my/load-theme (selected-frame)))

;; Other nice themes include
;; nord-theme.el
;; hc-zenburn.el
;; zenburn.el
;; ample-theme.el
;; solarized-theme.el
;; apropospriate-theme.el

(provide 'detvdl-theme)
;;; detvdl-theme.el ends here

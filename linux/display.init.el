(when (and (display-graphic-p) *is-linux*)
  (setq x-gtk-use-system-tooltips nil))

(set-face-attribute 'variable-pitch nil :family "iA Writer Quattro S" :height 120 :weight 'regular)
(set-face-attribute 'default nil :family "iA Writer Mono S" :height 110 :weight 'regular)
(set-frame-font "iA Writer Mono S-11:regular")

(provide 'display.init)

;;; display.init.el ends here

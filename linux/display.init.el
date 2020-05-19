(when (and (display-graphic-p) *is-linux*)
  (setq x-gtk-use-system-tooltips nil))

(set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 130 :weight 'regular)
(set-face-attribute 'default nil :family "Fira Code" :height 110 :weight 'light)
(set-frame-font "Fira Code-11:light")

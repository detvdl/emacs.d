;; base16-tomorrow-black-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Authors:
;; Scheme: Chris Kempson (http://chriskempson.com)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-tomorrow-black-colors
  '(:base00 "#000000"
    :base01 "#282a2e"
    :base02 "#373b41"
    :base03 "#969896"
    :base04 "#b4b7b4"
    :base05 "#c5c8c6"
    :base06 "#e0e0e0"
    :base07 "#ffffff"
    :base08 "#cc6666"
    :base09 "#de935f"
    :base0A "#f0c674"
    :base0B "#b5bd68"
    :base0C "#8abeb7"
    :base0D "#81a2be"
    :base0E "#b294bb"
    :base0F "#a3685a")
  "All colors for Base16 Tomorrow Night are defined here.")

;; Define the theme
(deftheme base16-tomorrow-black)

;; Add all the faces to the theme
(base16-theme-define 'base16-tomorrow-black base16-tomorrow-black-colors)

;; Mark the theme as provided
(provide-theme 'base16-tomorrow-black)

(provide 'base16-tomorrow-black-theme)

;;; base16-tomorrow-black-theme.el ends here

(deftheme default-acme-improved
  "Created 2019-02-10.")

(custom-theme-set-faces
 'default-acme-improved
 '(default ((t (:underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "#FCFFE9" :stipple nil :inherit nil))))
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 88) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 16) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 16) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 8)) (:foreground "black" :background "green")) (t (:inverse-video t))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "blue3")) (((class color) (min-colors 88) (background light) (type gtk)) (:background "gtk_selection_bg_color" :distant-foreground "gtk_selection_fg_color")) (((class color) (min-colors 88) (background light) (type ns)) (:background "ns_selection_bg_color" :distant-foreground "ns_selection_fg_color")) (((class color) (min-colors 88) (background light)) (:background "lightgoldenrod2")) (((class color) (min-colors 16) (background dark)) (:background "blue3")) (((class color) (min-colors 16) (background light)) (:background "lightgoldenrod2")) (((class color) (min-colors 8)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(font-lock-keyword-face ((default (:inherit (default)))))
 '(font-lock-constant-face ((default (:inherit (default)))))
 '(font-lock-function-name-face ((default (:inherit (default)))))
 '(font-lock-variable-name-face ((default (:inherit (default)))))
 '(font-lock-builtin-face ((default (:inherit (default)))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "gray60")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "gray60")) (((class color) (min-colors 88) (background light)) (:foreground "gray60")) (((class color) (min-colors 88) (background dark)) (:foreground "gray60")) (((class color) (min-colors 16) (background light)) (:foreground "gray60")) (((class color) (min-colors 16) (background dark)) (:foreground "gray60")) (((class color) (min-colors 8) (background light)) (:foreground "gray60")) (((class color) (min-colors 8) (background dark)) (:foreground "gray60")) (t (:slant italic :weight bold))))
 '(font-lock-doc-face ((t (:inherit (font-lock-comment-face)))))
 '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
 '(mode-line ((((class color) (min-colors 88)) (:foreground "black" :background "grey75" :box (:line-width -1 :color nil :style released-button))) (t (:inverse-video t))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

(provide-theme 'default-acme-improved)
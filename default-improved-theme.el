(deftheme default-improved
  "Created 2019-02-10.")

(custom-theme-set-faces
 'default-improved
 '(default ((t (:underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "#FCFFE9" :stipple nil :inherit nil))))
 '(font-lock-type-face ((default (:inherit (default)))))
 '(font-lock-keyword-face ((default (:inherit (default) :weight bold))))
 '(font-lock-constant-face ((default (:inherit (default)))))
 '(font-lock-function-name-face ((default (:inherit (default) :weight bold))))
 '(font-lock-variable-name-face ((default (:inherit (default)))))
 '(font-lock-builtin-face ((default (:inherit (default)))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "gray60")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "gray60")) (((class color) (min-colors 88) (background light)) (:foreground "gray60")) (((class color) (min-colors 88) (background dark)) (:foreground "gray60")) (((class color) (min-colors 16) (background light)) (:foreground "gray60")) (((class color) (min-colors 16) (background dark)) (:foreground "gray60")) (((class color) (min-colors 8) (background light)) (:foreground "gray60")) (((class color) (min-colors 8) (background dark)) (:foreground "gray60")) (t (:slant italic :weight bold))))
 '(font-lock-doc-face ((t (:inherit (font-lock-comment-face))))))

(provide-theme 'default-improved)

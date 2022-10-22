;;;; Ivy
(use-package ivy
  :straight t
  :blackout ivy-mode
  :hook (after-init . ivy-mode)
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         ("C-x C-f" . counsel-find-file)
         ("C-c y" . counsel-yank-pop)
         ("C-c g" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x 8" . counsel-unicode-char)
         ("C-x b" . ivy-switch-buffer)
	     ("C-c C-r" . ivy-resume)
         ("C-c C-u" . swiper-all)
         ("M-x" . counsel-M-x)
         :map ivy-occur-mode-map
         ("w" . ivy-wgrep-change-to-wgrep-mode)
         ("C-x C-q" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-m" . ivy-alt-done)
         ("C-j" . ivy-done)
         ("<escape>" . minibuffer-keyboard-quit))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  (enable-recursive-minibuffers t)
  (ivy-display-style nil)
  (ivy-height 8) ;; used when posframe is not available
  (ivy-virtual-abbreviate 'full)
  (ivy-extra-directories nil)
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (counsel-ag-function . ivy--regex-plus)
                           (counsel-grep-function . ivy--regex-plus)
                           (swiper-all . ivy--regex-plus)
                           (swiper-isearch . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-rg-base-command '("rg" "--max-columns" "240" "--with-filename" "--no-heading"
                             "--line-number" "--color" "never" "%s"))
  :config
  ;; Fuzzy matching
  (use-package flx :straight t))

(use-package swiper
  :straight t
  :after ivy
  :bind ("M-n" . swiper-thing-at-point)
  :config
  (setq swiper-use-visual-line-p #'ignore))

(use-package counsel
  :straight t
  :after swiper
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\'"))

(use-package ivy-prescient
  :straight t
  :after (ivy counsel prescient)
  :custom
  (ivy-prescient-enable-filtering nil)
  (ivy-prescient-sort-commands
   '(:not swiper swiper-isearch ivy-switch-buffer vrt-news))
  :config
  (ivy-prescient-mode))

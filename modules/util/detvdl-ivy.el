;;; detvdl-ivy.el --- <3 Abo-Abo
;;; Commentary:
;;; Code:

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-s" . counsel-grep-or-swiper)
         ("C-c C-u" . swiper-all)
         ("C-c C-e" . ivy-wgrep-change-to-wgrep-mode)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-m" . ivy-alt-done)
         ("C-j" . ivy-done))
  :config
  (progn
    ;; Fuzzy matching
    (use-package flx :ensure t)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t
          ivy-display-style 'fancy
          ivy-height 8
          ivy-virtual-abbreviate 'full
          ivy-extra-directories nil
          ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                  (t . ivy--regex-fuzzy)))
    (ivy-mode 1)))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x y" . counsel-yank-pop)
         ;; ("C-c g" . counsel-git)
         ;; ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-x 8" . counsel-unicode-char))
  :config
  ;; put most recent commands first in `counsel-M-x'
  (use-package smex :ensure t)
  ;; use the faster ripgrep for standard counsel-grep
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(provide 'detvdl-ivy)
;;; detvdl-ivy.el ends here

;;; detvdl-rust.el -- Rust packages and configuration
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (progn
    (use-package racer
      :ensure t
      :hook rust-mode
      :config
      (setq racer-cmd "~/.cargo/bin/racer"
            racer-rust-src-path "/Users/detlev/Workspaces/Git/rust/src"))

    (use-package cargo
      :ensure t
      :hook (rust-mode . cargo-minor-mode))

    (use-package flycheck-rust
      :ensure t
      :functions flycheck-rust-setup)

    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)

    (with-eval-after-load "flycheck"
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
    (setq rust-format-on-save t)))

(provide 'detvdl-rust)
;;; detvdl-rust.el ends here

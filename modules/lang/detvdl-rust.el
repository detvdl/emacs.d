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
      :commands racer-mode
      :config
      (setq racer-cmd "~/.cargo/bin/racer"
            racer-rust-src-path "/Users/detlev/Git/rust/src"))

    (use-package cargo
      :ensure t
      :commands cargo-minor-mode)

    (use-package flycheck-rust
      :ensure t
      :functions flycheck-rust-setup)

    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'rust-mode-hook #'cargo-minor-mode)
    (with-eval-after-load "flycheck"
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
    (setq rust-format-on-save t)))

(provide 'detvdl-rust)
;;; detvdl-rust.el ends here

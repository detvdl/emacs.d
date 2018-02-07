;;; detvdl-erc.el --- Emacs IRC settings
;;; Commentary:
;;; Code:

;; ERC is a standard emacs package, this is merely for completeness' sake.
(use-package erc :ensure nil)

(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#erc" "#rust")))
;; This causes ERC to connect to the Freenode network.
;; auth-source automatically resolves the password and port for us.
(defun erc-freenode ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "dvdael"))

(global-set-key "\C-cef" 'erc-freenode)

;; Rename server buffers to reflect the current network name instead
;; of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
;; This is useful when using a bouncer like ZNC where you have multiple
;; connections to the same server.
(setq erc-rename-buffers t
      ;; Interpret mIRC-style color commands in IRC chats
      erc-interpret-mirc-color t
      ;; Kill buffers for channels after /part
      erc-kill-buffer-on-part t
      ;; Kill buffers for private queries after quitting the server
      erc-kill-queries-on-quit t
      ;; Kill buffers for server messages after quitting the server
      erc-kill-server-buffer-on-quit t)

(provide 'detvdl-erc)
;;; detvdl-erc.el ends here

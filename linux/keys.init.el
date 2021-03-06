;; See X11 iso-lefttab explanation at https://emacs.stackexchange.com/a/53469
(define-key function-key-map [(control shift iso-lefttab)] [(control shift tab)])
(define-key function-key-map [(meta shift iso-lefttab)] [(meta shift tab)])
(define-key function-key-map [(meta control shift iso-lefttab)] [(meta control shift tab)])

(provide 'keys.init)

;;; keys.init.el ends here

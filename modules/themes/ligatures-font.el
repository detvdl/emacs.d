;;; ligatures-font.el --- Enable ligatures  -*- lexical-binding: t -*-

;;; Commentary:
;; Taken from:
;; https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md#appendix-b-update-1-firacode-integration

;;; Code:
(defconst ligatures-fira-code-start #Xe100)

(defconst ligatures-fira-code-list
  '("www" "**" "***" "*>" "*/" "\\\\" "\\\\\\" "]#" "::" ":::"
    ":=" "!!" "!=" "!==" "--" "---" "-->" "->" "->>" "-<"
    "-<<" "-~" "#{" "#[" "#!" "##" "###" "####" "#(" "#?"
    "#_" "#_(" ".-" ".=" ".." "..<" "..." ".?" "?:" "?="
    "?." "??" ";;" "/*" "/=" "/==" "/>" "//" "///" "__"
    "&&" "||" "|||>" "||=" "||>" "|=" "|>" "^=" "$>" "++"
    "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<=" "=<<"
    "=/=" ">-" ">->" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
    "<*>" "<|" "<||" "<|||" "<|>" "<$" "<$>" "<!--" "<-" "<--"
    "<->" "<-<" "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<"
    "<<-" "<<=" "<<<" "<~" "<~>" "<~~" "</" "</>" "~@" "~-"
    "~=" "~>" "~~" "~~>" "%%")
  "Ordered ligatures for Fira Code font")

(defun ligatures-correct-symbol-bounds (len char)
  "Prepend up to LEN non-breaking spaces with reference points to CHAR.
This way `compose-region' called by function `prettify-symbols-mode'
will use the correct width of the symbols instead of the width
measured by `char-width'."
  (let ((acc (list char)))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc)))
      (setq len (1- len)))
    acc))


(defun ligatures-make-alist (ligatures starting-code)
  "Construct text to ligature character.
For each string in LIGATURES list add replacement from STARTING-CODE
sequentially."
  (mapcar (lambda (l)
            (let ((n starting-code))
              (setq starting-code (1+ starting-code))
              (when l
                (cons l (ligatures-correct-symbol-bounds
                         (length l) n)))))
          ligatures))

(defun my:ligatures-fira-code-setup ()
  "Add Fira Code ligatures to `prettify-symbols-alist'."
  (setq prettify-symbols-alist (append (ligatures-make-alist
                                        ligatures-fira-code-list
                                        ligatures-fira-code-start))))
(provide 'ligatures-font)

;;; init-font.el ends here

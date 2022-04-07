
;; (defconst git--state-small-dot
;;   "/* XPM */
;; static char * data[] = {
;; \"14 7 3 1\",
;; \" 	c None\",
;; \"+	c #202020\",
;; \".	c %s\",
;; \"      +++     \",
;; \"     +...+    \",
;; \"    +.....+   \",
;; \"    +.....+   \",
;; \"    +.....+   \",
;; \"     +...+    \",
;; \"      +++     \"};")

;; (defconst git--state-large-dot
;;   "/* XPM */
;; static char * data[] = {
;; \"18 13 3 1\",
;; \" 	c None\",
;; \"+	c #000000\",
;; \".	c %s\",
;; \"                  \",
;; \"       +++++      \",
;; \"      +.....+     \",
;; \"     +.......+    \",
;; \"    +.........+   \",
;; \"    +.........+   \",
;; \"    +.........+   \",
;; \"    +.........+   \",
;; \"    +.........+   \",
;; \"     +.......+    \",
;; \"      +.....+     \",
;; \"       +++++      \",
;; \"                  \"};")

;; (defun git--state-color (state)
;;   "Return an appropriate color string for the given Git STATE."
;;   (cond ((eq state 'edited) "green")
;;         ((eq state 'added) "blue")
;;         ((memq state '(removed conflict unregistered)) "red")
;;         ((memq state '(needs-update needs-merge)) "purple")
;;         ((eq state 'up-to-date) "yellow")
;;         ((eq state 'staged) "yellow")
;;         ((memq state '(ignored unknown)) "gray50")
;;         (t "gray50")))

;; (defun git--state-dot (&optional state)
;;   "Return the appropriate bitmap dot for the given Git STATE."
;;   (let* ((backend (vc-backend buffer-file-name))
;;          (state (or state (if (and backend buffer-file-name)
;;                               (vc-state buffer-file-name backend)
;;                             'unknown)))
;;          (color (git--state-color state)))
;;     (propertize "   "
;;                 'help-echo (format "VC state: %s" state)
;;                 'display
;;                 `(image :type xpm
;;                         :data ,(format git--state-large-dot color)
;;                         :ascent center))))

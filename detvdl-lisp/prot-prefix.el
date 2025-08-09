;;; prot-prefix.el --- Prefix keymap for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Prefix keymap for my custom keymaps.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(declare-function prot-simple-kill-buffer-current "prot-simple" (&optional arg))
(declare-function prot-simple-rename-file-and-buffer "prot-simple" (name))
(declare-function prot-simple-buffers-major-mode "prot-simple")
(declare-function prot-simple-buffers-vc-root "prot-simple")
(declare-function beframe-buffer-menu "beframe" (&optional frame &key sort))

(defvar-keymap prot-prefix-buffer-map
  :doc "Prefix keymap for buffers."
  :name "Buffer"
  :prefix 'prot-prefix-buffer
  "m" #'beframe-buffer-menu
  "b" #'switch-to-buffer
  "B" #'prot-simple-buffers-major-mode
  "c" #'clone-indirect-buffer-other-window
  "f" #'fit-window-to-buffer
  "k" #'prot-simple-kill-buffer-current
  "g" #'revert-buffer-quick
  "r" #'prot-simple-rename-file-and-buffer
  "n" #'next-buffer
  "p" #'previous-buffer
  "v" #'prot-simple-buffers-vc-root)

(defvar-keymap prot-prefix-file-map
  :doc "Prefix keymaps for files."
  :name "File"
  :prefix 'prot-prefix-file
  "f" #'find-file
  "F" #'find-file-other-window
  "b" #'bookmark-jump
  "d" #'dired
  "l" #'find-library
  "m" #'man)

(defvar-keymap prot-prefix-insert-map
  :doc "Prefix keymap for character insertion."
  :name "Insert"
  :prefix 'prot-prefix-insert
  "i" #'insert-char
  "e" #'emoji-search
  "q" #'quoted-insert
  "s" #'emoji-search
  "l" #'emoji-list)

(declare-function logos-focus-mode "logos")
(declare-function keycast-mode-line-mode "keycast")
(declare-function rainbow-mode "rainbow")
(declare-function spacious-padding-mode "spacious-padding")

(defvar-keymap prot-prefix-mode-map
  :doc "Prefix keymap for minor mode toggles."
  :name "Toggle"
  :prefix 'prot-prefix-mode
  "f" #'flymake-mode
  "h" #'hl-line-mode
  "k" #'keycast-mode-line-mode
  "l" #'logos-focus-mode
  "m" #'menu-bar-mode
  "n" #'display-line-numbers-mode
  "t" #'toggle-truncate-lines
  "s" #'spacious-padding-mode
  "r" #'rainbow-mode
  "v" #'variable-pitch-mode)

(defvar-keymap prot-prefix-window-map
  :doc "Prefix keymap for windows."
  :name "Window"
  :prefix 'prot-prefix-window
  "u" #'winner-undo
  "r" #'winner-redo
  "b" #'balance-windows-area
  "d" #'toggle-window-dedicated
  "0" #'delete-window
  "1" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "2" #'split-window-below
  "@" #'split-root-window-below
  "3" #'split-window-right
  "#" #'split-root-window-right
  "o" #'other-window
  "^" #'tear-off-window
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "H" #'windmove-swap-states-left
  "J" #'windmove-swap-states-down
  "K" #'windmove-swap-states-up
  "L" #'windmove-swap-states-right)

(declare-function consult-find "consult" (&optional dir initial))
(declare-function consult-ripgrep "consult" (&optional dir initial))
(declare-function prot-search-grep "prot-search" (regexp &optional recursive))
(declare-function prot-search-grep-todo-keywords "prot-search" (&optional arg))
(declare-function prot-search-occur-browse-url "prot-search")
(declare-function prot-search-occur-outline "prot-search" (&optional arg))
(declare-function prot-simple-flush-and-diff "prot-simple" (regexp beg end))

(defvar-keymap prot-prefix-search-map
  :doc "Prefix keymap for search (and replace) commands."
  :name "Search"
  :prefix 'prot-prefix-search
  "f" #'consult-find
  "d" #'prot-simple-flush-and-diff
  "g" #'prot-search-grep
  "o" #'prot-search-occur-outline
  "r" #'consult-ripgrep
  "t" #'prot-search-grep-todo-keywords
  "u" #'prot-search-occur-browse-url)

(declare-function prot-simple-transpose-chars "prot-simple")
(declare-function prot-simple-transpose-lines "prot-simple" (arg))
(declare-function prot-simple-transpose-paragraphs "prot-simple" (arg))
(declare-function prot-simple-transpose-sentences "prot-simple" (arg))
(declare-function prot-simple-transpose-words "prot-simple" (arg))
(declare-function prot-simple-transpose-sexps "prot-simple" (arg))

(defvar-keymap prot-prefix-transpose-map
  :doc "Prefix keymap for object transposition."
  :name "Transpose"
  :prefix 'prot-prefix-transpose
  "c" #'prot-simple-transpose-chars
  "l" #'prot-simple-transpose-lines
  "p" #'prot-simple-transpose-paragraphs
  "s" #'prot-simple-transpose-sentences
  "w" #'prot-simple-transpose-words
  "x" #'prot-simple-transpose-sexps)

(defvar-keymap prot-prefix-expression-map
  :doc "Prefix keymap for s-expression motions."
  :name "S-EXP"
  :prefix 'prot-prefix-expression
  "a" #'beginning-of-defun
  "e" #'end-of-defun
  "f" #'forward-sexp
  "b" #'backward-sexp
  "n" #'forward-list
  "p" #'backward-list
  "d" #'up-list ; confusing name for what looks "out and down" to me
  "t" #'transpose-sexps
  "u" #'backward-up-list ; the actual "up"
  "k" #'kill-sexp
  "DEL" #'backward-kill-sexp)

(declare-function winner-undo "winner")
(declare-function winner-redo "winner")
(declare-function magit-status "magit" (&optional directory cache))
(declare-function prot-simple-other-windor-or-frame "prot-simple")

;; NOTE 2024-02-17: Some cons cells here have a symbol as a `cdr' and
;; some do not.  The former are those which define a prefix command
;; (per `define-prefix-command').  This is a symbol that references
;; the keymaps, thus making our binding an indirection: if we update
;; the key map, we automatically get the new key bindings.  Whereas
;; when we bind a key to the value of a variable, we have to update
;; the key map and then the binding for changes to propagate.
(defvar-keymap prot-prefix-map
  :doc "Prefix keymap with multiple subkeymaps."
  :name "Prot Prefix"
  :prefix 'prot-prefix
  "0" #'delete-window
  "1" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "^" #'tear-off-window
  "2" #'split-window-below
  "@" #'split-root-window-below
  "3" #'split-window-right
  "#" #'split-root-window-right
  "o" #'other-window
  "O" #'prot-simple-other-windor-or-frame
  "Q" #'save-buffers-kill-emacs
  "b" (cons "Buffer" 'prot-prefix-buffer)
  "c" #'world-clock
  "f" (cons "File" 'prot-prefix-file)
  "g" #'magit-status
  "h" (cons "Help" help-map)
  "i" (cons "Insert" 'prot-prefix-insert)
  "j" #'dired-jump
  "m" (cons "Minor modes" 'prot-prefix-mode)
  "n" (cons "Narrow" narrow-map)
  "p" (cons "Project" project-prefix-map)
  "r" (cons "Rect/Registers" ctl-x-r-map)
  "s" (cons "Search" 'prot-prefix-search)
  "t" (cons "Transpose" 'prot-prefix-transpose)
  "u" #'universal-argument
  "v" (cons "Version Control" 'vc-prefix-map)
  "w" (cons "Window" 'prot-prefix-window)
  "x" (cons "S-EXP" 'prot-prefix-expression))

(provide 'prot-prefix)
;;; prot-prefix.el ends here

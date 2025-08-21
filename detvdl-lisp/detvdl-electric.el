;;; detvdl-electric.el --- Electric extensions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025  detvdl

;; Author: detvdl <detvdl@pm.me>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
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
;; Extensions for Electric, specifically electric-pair-mode

;;; Code:
(defgroup detvdl-electric nil
  "Insert character pair around symbol or region."
  :group 'editing)

(defcustom detvdl-text-pairs
  '((?' . ?')
    (?\" . ?\")
    (?‘ . ?’)
    (?“ . ?”)
    (?` . ?`))
  "Electric pairs most often encountered in pure textual environments"
  :group 'detvdl-electric-group)

(defcustom detvdl-emphasis-pairs
  '((?= . ?=)
    (?* . ?*)
    (?/ . ?/)
    (?~ . ?~)
    (?_ . ?_)
    (?+ . ?+))
  "Electric pairs most often encountered in markup languages"
  :group 'detvdl-electric-group)

(defcustom detvdl-bracket-pairs
  '((?\(. ?\))
    (?\[ . ?\[)
    (?\{ . ?\})
    (?\< . ?\>))
  "Electric pairs most often encountered in programming modes"
  :group 'detvdl-electric-group)

(defcustom detvdl-prog-c-pairs
  `(,@detvdl-bracket-pairs
    ,@detvdl-text-pairs)
  "Electric pairs most often encountered in C-like languages"
  :group 'detvdl-electric-group)

(defcustom detvdl-prog-ocaml-pairs
  `(,@detvdl-bracket-pairs
    ,@(assq-delete-all ?' detvdl-text-pairs))
  "Electric pairs most often encountered in OCaml and ML-like languages (e.g. Reason)"
  :group 'detvdl-electric-group)

(defcustom detvdl-prog-lisp-pairs
  `(,@detvdl-bracket-pairs
    (?\" . ?\"))
  "Electric pairs most often encountered in Lispy languages"
  :group 'detvdl-electric-group)

(defcustom detvdl-org-mode-pairs
  `(,@detvdl-bracket-pairs
    ,@(assq-delete-all ?' detvdl-text-pairs)
    ,@detvdl-emphasis-pairs)
  "Electric pairs most often encountered in Org-mode prose"
  :group 'detvdl-electric-group)

(provide 'detvdl-electric)
;;; detvdl-electric.el ends here

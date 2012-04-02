;;; elu.el --- Emacs Lisp Utilities: general-purpose elisp utils
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://ilya.cc/elu
;; Version: 0.9
;;
;; This file is not yet part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Test code for elu module.

(require 'elu)
(require 'ert)
(eval-when-compile (require 'cl))

(defstruct elu-test a b c)

(ert-deftest elu-test-suite ()
  "Test various utilities in the elu module"
  (let ((s (make-elu-test :a 1 :b 2)))
    (should (equal (elu-modified-struct 'elu-test s :c 3)
		   (make-elu-test :a 1 :b 2 :c 3)))
    (should (equal (elu-with 'elu-test s (a b) (+ a b)) 3)))

  (should (equal (elu-remove-if 'evenp '(1 2 3 4 5 6)) '(1 3 5)))

  (should (equal (elu-make-seq 1) '(1)))
  (should (equal (elu-make-seq '(1 2)) '(1 2)))
  
  (should (equal (elu-gen-vector i 5 i) '[0 1 2 3 4])))

(set (make-local-variable 'elu-valu-number-names) '((one . 1) (two . 2)))

; (kill-local-variable 'elu-valu-number-name-range-rxx-def-local)
; (rxx-kill-local-vars)
(rxx-parse-string elu-valu valu "0")


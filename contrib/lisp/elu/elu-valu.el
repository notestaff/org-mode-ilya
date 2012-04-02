;;; elu-valu.el --- Handle values with units
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
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
;; Code to handle values with units.


(eval-when-compile (require 'cl))
(require 'elu)
(require 'rxx)

(defgroup elu-valu nil
  "Options for the elu-valu package"
  :tag "ELU - Emacs Lisp Utilities"
  :group 'elu
  :link '(url-link "http://sourceforge.net/projects/org-balance/"))

(defcustom elu-valu-units
  (quote ((time (second . 0.0166666666667) (minute . 1) (min . 1) (hour . 60) (hr . 60) (day . 1440) (week . 10080)
		(workweek . 7200) (month . 43200) (year . 525600))
	  (money (dollar . 1) ($ . 1) (cent . 0.01) (k . 1000))
	  (count (item . 1) (time . 1))
	  (unpleasantness (frog . 1))))
  "Units and their relative values"
  :group 'elu-valu
  :type '(alist :tag "Units used in elu-valu"
		:key-type (symbol :tag "Dimension")
		:value-type
		(alist :key-type (symbol :tag "Unit name")
		       :value-type (number :tag "Relative value"))))
;;;;;;;;;;;;;;;

(def-rxx-namespace elu-valu
  "Values with units"
  :exports (number number-range valu valu-range ratio-word valu-ratio))

(defun elu-valu-do-sum (start-value seq)
  "Sum a sequence SEQ of valus, starting with START-VALUE."
  (let ((result
	 (dolist (v seq start-value)
	   (setq start-value (add-elu-valu start-value v)))))
    result))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Values with units
;;
;; Code to represent values with units, e.g. "5 hours" or "100 dollars" or just "2 items".
;; 
;; Design note: the calc.el package included with emacs supports calculations on values with units;
;; but for several reasons it was better not to use that package here.  (It is too general and too complex,
;; and we don't want to mess up other calculations by adding elu-valu units to emacs standard units.)
;;
;; notes
;;
;; an alternative would be to set calc.el units only temporarily, while elu-valu code runs,
;; and restore them under unwind-protect.
;;
;; old comment:
;;
;; Code for parsing and representing time intervals and ratios.  Time intervals are phrases of the form "2 hours" or
;; "1 day"; code here parses them and converts them to a uniform representation (minutes).
;; Time ratios are phrases of the form "30 minutes per day" or "3 hours a week"; more generally,
;; "[time interval] per [time interval]".   Code here parses them and converts them to a uniform representation
;; (minutes per day).  Occurrence rates are phrases of the form "1 a week" or "3 every 2 days"; more generally,
;; [amount] per [time interval].  Code here parses them and converts them to a uniform representation
;; (amount per day).  There is also code to convert from the uniform representation back to the user's original
;; unit for display (e.g. from "8.57 minutes per day" to "1 hour a week").


;; for each unit, add plural form: make "seconds" mean the same thing as "second"
(defconst elu-valu-units-with-plurals
  (mapcar
   (lambda (dimension-info)
     (cons (car dimension-info)
	   (apply
	    'append
	    (mapcar
	     (lambda (unit-info)
	       (list unit-info (cons (intern (concat (symbol-name (car unit-info)) "s")) (cdr unit-info))))
	     (cdr dimension-info)))))
   elu-valu-units))

;; var: elu-valu-unit2dim-alist - assoc list mapping each unit to its dimension (time, money, count, ...)
(defconst elu-valu-unit2dim-alist
  (apply
   'append
   (mapcar
    (lambda (dimension-info)
      (mapcar (lambda (unit-info) (cons (car unit-info) (car dimension-info))) (cdr dimension-info)))
    elu-valu-units-with-plurals)))

(put 'elu-valu-error 'error-conditions '(error elu-valu-errors elu-valu-error))
(put 'elu-valu-error 'error-message "elu-valu error")


(defun elu-valu-is-unit (unit)
  (elu-assoc-val unit elu-valu-unit2dim-alist 'nil-ok))

(defun elu-valu-unit2dim (unit)
  "Given a unit, return the dimension that its measures"
  (elu-assoc-val unit elu-valu-unit2dim-alist))

;; var: elu-valu-unit2base-alist - assoc list mapping each unit to how many base units are in it
(defconst elu-valu-unit2base-alist (apply 'append (mapcar 'cdr elu-valu-units-with-plurals)))

(defun elu-valu-unit2base (unit)
  "Return the number of base units in the given unit.  For each dimension we have a base unit in terms of which all other
units are measured; for example, for time we use minutes."
  (elu-assoc-val unit elu-valu-unit2base-alist))

;; Struct: elu-valu - a value together with a given unit of measurement, e.g., 5 hours. 
(defstruct (elu-valu
	    (:constructor create-elu-valu
			  (val unit-name
			       &aux (unit
				     (if (stringp unit-name) (intern unit-name) unit-name)))))
	    val unit)

(defun scale-elu-valu (factor valu)
  "Return the value scaled by the factor"
  (elu-modified-struct 'elu-valu valu
    :val (* factor val)))

(defun new-elu-valu (val unit)
  (if (and (numberp val) (elu-valu-is-unit unit))
      (create-elu-valu val unit)
    (error "Invalid valu: %s %s" val unit)))

(defun convert-elu-valu (valu new-unit &optional multiples-of-new-unit)
  "Convert a valu to new units in the same dimension, e.g. 1 hour to 1/24 of a day.  If MULTIPLES-OF-NEW-UNIT is given,
we convert to the specified multiples of new unit."
  (when (stringp new-unit) (setq new-unit (intern new-unit)))
  (unless (eq (elu-valu-unit2dim (elu-valu-unit valu)) (elu-valu-unit2dim new-unit))
    (error "Cannot convert between incompatible units: %s and %s" (elu-valu-unit valu) new-unit))
  (unless multiples-of-new-unit (setq multiples-of-new-unit 1))
  (make-elu-valu :val (/ (/ (float (* (elu-valu-val valu)
				      (elu-valu-unit2base (elu-valu-unit valu))))
			    (float (elu-valu-unit2base new-unit)))
			 multiples-of-new-unit) :unit new-unit))

(defun add-elu-valu (valu1 valu2)
  "Add two values with units, converting them to a common unit.  Returns
a newly created valu representing the sum of VALU1 and VALU2."
  (let* ((unit1 (elu-valu-unit valu1))
	 (unit2 (elu-valu-unit valu2))
	 (smaller-unit (if (< (elu-valu-unit2base unit1) (elu-valu-unit2base unit2))
			   unit1 unit2))
	 (conv1 (convert-elu-valu valu1 smaller-unit))
	 (conv2 (convert-elu-valu valu2 smaller-unit)))
    (new-elu-valu (+ (elu-valu-val conv1)
		     (elu-valu-val conv2))
		  smaller-unit)))

(defun sub-elu-valu (valu1 valu2)
  "Subtract two values with units, converting them to a common unit.  Returns
a newly created valu representing the difference of VALU1 and VALU2."
  (add-elu-valu valu1 (scale-elu-valu -1 valu2)))

(defun add-elu-valu-vec (valu-vec-1 valu-vec-2)
  "Add two vectors of valus"
  (elu-map-vectors 'add-elu-valu valu-vec-1 valu-vec-2))

(defun scale-elu-valu-vec (factor valu-vec)
  "Scale all valus in VALU-VEC by FACTOR"
  (message "scaling by %s: %s" factor valu-vec)
  (elu-map-vectors (elu-apply-partially 'scale-elu-valu factor) valu-vec))

(put 'elu-valu-parse-error 'error-conditions '(error elu-valu-errors elu-valu-parse-error))
(put 'elu-valu-parse-error 'error-message "elu-valu: Could not parse")

(defconst elu-valu-number-names
  '((once . 1) (twice . 2) (thrice . 3) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6)
    (seven . 7) (eight . 8) (nine . 9)
    (ten . 10)))

(def-rxx elu-valu number-name
  "The string name of a number, for the few numbers often written as words.
Parsed as the numeric value of the number."
  (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car elu-valu-number-names))))
  (lambda (match) (cdr-safe (assoc (intern match) elu-valu-number-names))))

(def-rxx elu-valu number-name-range
  "A pair of numbers"
  (seq (number-name a)  blanks (number-name b))
  (cons a b))

(def-rxx elu-valu number
  "A general number -- floating-point or integer.
Some frequently-used numbers can also be written in English;
see variable `elu-valu-number-names'.
Parsed as the numeric value of the number."
  (or
   number-name
   (seq
    (opt (any "+-"))
    (or (seq digits (opt ".") digits?)
	(seq "." digits))
    (opt (any "eE") (opt (any "+-")) digits)))
  (lambda (match)
    (or number-name (string-to-number match))))

;; (defun elu-valu-is-valid-number-p (s)
;;   "Test if s is a number or a string representing a valid number (ignoring leading or trailing whitespace).
;; The whole string must match a floating-point number optionally surrounded by whitespace; extraneous characters in string
;; are not allowed.
;; "
;;   (if (numberp s) s
;;     (save-match-data
;;       (elu-full-match elu-valu-number-regexp s))))

(defun elu-valu-string-to-number (s)
  "Convert a string to a number, recognizing some number names for readability.  If s is already a number, just return it.
Unlike the standard `string-to-number', if the string as a whole cannot be interpreted as a valid number possibly surrounded
by whitespace, it throws an error rather than silently returning zero.
"
  (if (numberp s) s
    (rxx-parse-string elu-valu number s)))

(defalias 'elu-valu-parse-number 'elu-valu-string-to-number)

(def-rxx elu-valu number-range
  "A range of two numbers separated by a dash; or a single number,
in which case the range contains just that number.   
Parsed as a cons of range start and end."
  (seq (number range-start) (opt "-" (number range-end)))
  (cons range-start (or range-end range-start)))

(defvar elu-valu-parse-valu-hooks nil
  "List of hooks for parsing valu strings (value with units), such as `5 hours'.  Can be used e.g. to parse currency
such as $5 into the canonical form `5 dollars'.  Each hook must take a string as an argument and return either an
`elu-valu' struct if it successfully parsed the string, or nil if it didn't.")
;; FIXME: such hooks should also provide the regexp to much this.  so, an aregexp.

(def-rxx elu-valu unit
  "A unit name.   See customization variable "
  (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car elu-valu-unit2dim-alist)))))

(def-rxx elu-valu valu "Value with unit"
  ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
  ;; or an optional number (assumed to be 1 if not given) followed by a unit.
  ;; But either a number or a unit must be given.
  (or (sep-by blanks? (named-grp unit "$") number)
      (sep-by blanks number? unit)
      (sep-by blanks number unit?))
  (new-elu-valu (or number 1) (or unit "item")))

(defun parse-elu-valu (valu-str)
  "Given a string representing a value with units, parse it into an elu-valu structure."
  (or
   (run-hook-with-args-until-success 'elu-valu-parse-valu-hooks valu-str)
   (rxx-parse-string elu-valu valu valu-str)))

(def-rxx elu-valu valu-range
  "Value range"
  ;; Either a number range optionally followed by a unit (unit assumed to be "item" if not given),
  ;; or an optional number (assumed to be 1 if not given) followed by a unit.
  ;; But either a number or a unit must be given.
  (or (sep-by blanks number-range? unit)
      (sep-by blanks number-range  unit?))
  (let ((number-range (or number-range (cons 1 1)))
	(unit (or unit "item")))
    (cons (new-elu-valu (car number-range) unit)
	  (new-elu-valu (cdr number-range) unit))))

(defstruct (elu-valu-ratio (:include elu-ratio)))

(def-rxx elu-valu ratio-word "A word separating the numerator and denominator of a fraction."
  (or "per" "every" "each" "for every" "for each" "/" "a" "in a"))

;; allow the unit of a valu to be another valu.


(def-rxx elu-valu valu-ratio "A ratio of valus such as 1 hour per week"
  (sep-by blanks (valu numer) ratio-word (valu denom))
  (make-elu-valu-ratio :numer numer :denom denom))

(defun scale-elu-valu-ratio (factor elu-valu-ratio)
  "Multiply a valu ratio by a factor"
  (make-elu-valu-ratio :numer (scale-elu-valu factor (elu-valu-ratio-numer elu-valu-ratio))
		       :denom (elu-valu-ratio-denom elu-valu-ratio)))

(defun convert-elu-valu-ratio (old-valu-ratio new-valu-ratio)
  "Convert a valu ratio to new units, e.g. minutes per day to hours per week.  We keep the denominator of the new ratio,
changing only the numerator."
  (let ((new-num (elu-valu-ratio-numer new-valu-ratio))
	(new-denom (elu-valu-ratio-denom new-valu-ratio)))
    (make-elu-valu-ratio
     :numer (new-elu-valu (/ (elu-valu-val (convert-elu-valu
					    (elu-valu-ratio-numer old-valu-ratio)
					    (elu-valu-unit new-num)))
			     (elu-valu-val (convert-elu-valu
					    (elu-valu-ratio-denom old-valu-ratio)
					    (elu-valu-unit new-denom)
					    (elu-valu-val new-denom))))
			  (elu-valu-unit new-num))
     :denom new-denom)))

(defconst elu-valu-parse-test-defs
  '((number "three" 3)
    (number "3." 3)
    (number "3.3737" 3.3737)
    (number ".012340" 0.01234)
    (number "1e-5" 1e-5)
    (number "1.35e5" 1.35e5)
    (valu "$10.37" [cl-struct-elu-valu 10.37 $])
    (valu "33" [cl-struct-elu-valu 33 item])
    (valu "33 items" [cl-struct-elu-valu 33 items])
    (valu "item" [cl-struct-elu-valu 1 item])
    (valu "0" [cl-struct-elu-valu 0 item])
    (valu "week" [cl-struct-elu-valu 1 week])
    (valu-range "2-3 weeks" ([cl-struct-elu-valu 2 weeks] . [cl-struct-elu-valu 3 weeks]))
    (valu-range "2-3" ([cl-struct-elu-valu 2 item] . [cl-struct-elu-valu 3 item]))))


(provide 'elu-valu)

;;; elu-valu.el ends here


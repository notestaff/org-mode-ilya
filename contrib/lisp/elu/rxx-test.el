

(eval-when-compile (require 'cl))
(require 'rxx)
(require 'time-date)

(rxx-start-module nil)

;(eval-when-compile (defconst number-regexp (rxx (one-or-more digit) string-to-number)))
(defrxx number-regexp (one-or-more digit) string-to-number "a number")

(defrxx fraction-regexp (seq (number-regexp numerator) "/" (number-regexp denominator))
  (cons numerator denominator))

;; (assert
;;  ;; issue: when compiling, the previous defs are not available.
;;  ;; so, need to make a macro that translates the whole thing into something
;;  ;; that at runtime does the right thing.
;;  (equal (eval-when-compile
;; 	  (let* ((number-regexp (rxx (one-or-more digit) string-to-number))
;; 		 (fraction-regexp (rxx (seq (number-regexp numerator) "/" (number-regexp denominator))
;; 				       (cons numerator denominator)))
;; 		 (paren-regexp (rxx (seq "(" (fraction-regexp val) ")") val))
;; 		 (range-regexp (rxx (seq "[" (paren-regexp rmin) "]--[" (paren-regexp rmax) "]")
;; 				    (list rmin rmax))))
;; 	    (rxx-parse range-regexp "[(1/2)]--[(3/4)]"))) '((1 . 2) (3 . 4))))

(defrxxconst rxx-number-names
  '((once . 1) (twice . 2) (thrice . 3) (one . 1) (two . 2) (three . 3) (four . 4) (five . 5) (six . 6)
    (seven . 7) (eight . 8) (nine . 9)
    (ten . 10)))

(defrxx rxx-number-regexp
     (seq
      blanks?
      (or
       ;; either an english number name
       (named-grp number-name (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car rxx-number-names)))))
       
       ;; or a floating-point number, possibly in scientific notation
       (seq
	(optional (any "+-"))
	(or (seq (one-or-more (any digit))
		 (optional ".")
		 (optional (one-or-more (any digit))))
	    (seq "." (one-or-more (any digit))))
	(optional
	 (seq (any "eE")
	      (optional (any "+-"))
	      (one-or-more (any digit))))))
      
      (zero-or-more whitespace))
     (lambda (match)
       (if number-name (cdr (assoc (intern number-name) rxx-number-names))
	 (string-to-number match)))
     "number")
  
(defrxx rxx-number-range-regexp
  (seq
   (rxx-number-regexp range-start)
   (optional "-" (rxx-number-regexp range-end)))
  (cons range-start (or range-end range-start)))

(defrxxconst rxx-units
  '((time . ((second . 0.0166666666667) (minute . 1) (min . 1) (hour . 60) (hr . 60) (day . 1440) (week . 10080)
	     (workweek . 7200)
	     (month . 43200) (year . 525600) (bluemoon 1e12)))
    (money . ((dollar . 1) (cent . .01) (k . 1000)))
    (count . ((item . 1) (time . 1)))))

;; for each unit, add plural form: make "seconds" mean the same thing as "second"
(defrxxconst rxx-units
  (mapcar
   (lambda (dimension-info)
     (cons (car dimension-info)
	   (apply 'append
		  (mapcar
		   (lambda (unit-info)
		     (list unit-info (cons (intern (concat (symbol-name (car unit-info)) "s")) (cdr unit-info))))
		   (cdr dimension-info)))))
   rxx-units))

;; var: rxx-unit2dim-alist - assoc list mapping each unit to its dimension (time, money, count, ...)
(defrxxconst rxx-unit2dim-alist
  (apply 'append
	 (mapcar
	  (lambda (dimension-info)
	    (mapcar (lambda (unit-info) (cons (car unit-info) (car dimension-info))) (cdr dimension-info)))
	  rxx-units)))
  
(defrxx rxx-unit-regexp
  (eval-regexp (regexp-opt (mapcar 'symbol-name (mapcar 'car rxx-unit2dim-alist)))))


;(defrxx rxx-valu-regexp
;  ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
;  ;; or an optional number (assumed to be 1 if not given) followed by a unit.
;  ;; But either a number or a unit must be given.
;  (or (seq (optional (rxx-number-regexp val))
;	   (rxx-unit-regexp unit))
;      (seq (rxx-number-regexp val) (optional (rxx-unit-regexp unit))))
;  (cons (or val 1) (or unit "item"))
;  "value with unit")

(defrxx rxx-valu-regexp
  ;; Either a number optionally followed by a unit (unit assumed to be "item" if not given),
  ;; or an optional number (assumed to be 1 if not given) followed by a unit.
  ;; But either a number or a unit must be given.
  (or (seq (optional (rxx-number-regexp val))
	   (rxx-unit-regexp unit))
      (seq (rxx-number-regexp val) (optional (rxx-unit-regexp unit))))
  (cons (or val 1) (or unit "item"))
  "value with unit")

(assert (equal (rxx-parse rxx-valu-regexp "1 day") '(1 . "day")))

(defrxx
    rxx-valu-range-regexp
     ;; Either a number range optionally followed by a unit (unit assumed to be "item" if not given),
     ;; or an optional number (assumed to be 1 if not given) followed by a unit.
     ;; But either a number or a unit must be given.
     (or (seq (optional (seq (named-grp range rxx-number-range-regexp) (one-or-more whitespace)))
	      (named-grp unit rxx-unit-regexp))
	 (seq (named-grp range rxx-number-range-regexp) (optional (one-or-more whitespace)
								  (named-grp unit rxx-unit-regexp))))
     (let ((number-range (or range (cons 1 1)))
	   (unit (or unit "item")))
       (cons (cons (car number-range) unit)
	     (cons (cdr number-range) unit)))
    "value range")
  
(defrxxconst rxx-ratio-words (list "per" "every" "each" "/" "a" "in a"))
  
(defrxx rxx-polarity-regexp
  (seq (zero-or-more whitespace)
       (seq
	(or (named-grp atmost (eval-regexp (regexp-opt (list "at most"))))
	    (named-grp atleast (eval-regexp (regexp-opt (list "at least"))))))
       (zero-or-more whitespace))
  (if atmost 'atmost 'atleast)
  "polarity")
  
(defrxx rxx-valu-ratio-goal-regexp
  (seq
   (optional (named-grp polarity rxx-polarity-regexp))
   (named-grp num rxx-valu-range-regexp)
   (one-or-more whitespace) (eval-regexp (regexp-opt rxx-ratio-words)) (one-or-more whitespace)
   (named-grp denom rxx-valu-regexp)
   (optional
    ;; specify margin
    (seq
     (one-or-more whitespace)
     "+-"
     (zero-or-more whitespace)
     (seq
      (or
       (seq (named-grp margin-percent rxx-number-regexp)
	    (zero-or-more whitespace)
	    "%")
       (named-grp margin-val rxx-valu-regexp))))))
  (lambda (goal-str)
    (list
     :num-min (car num)
     :num-max (cdr num)
     :denom denom
     :polarity polarity
     :margin (or margin-percent margin-val)
     :text goal-str))
  "value ratio goal")
  
(assert (equal (rxx-parse rxx-valu-ratio-goal-regexp "at least once a day +- 5%")
	       '(:num-min (1 . "item") :num-max (1 . "item") :denom (1 . "day") :polarity atleast :margin 5 :text "at least once a day +- 5%")))

;(assert (equal (rxx-parse (rxx (seq (or (named-grp num (one-or-more digit))
;					(seq "hithere" (named-grp num (one-or-more digit))))
;				    (named-backref num))) "1212") "1212"))


;(assert (equal (rxx-parse (rxx (seq (or (named-grp num (one-or-more digit))
;					(seq "hithere" (named-grp num (one-or-more digit))))
;				    (named-backref num)) (string-to-number num)) "hithere1212") 12))

;;;borrowed from orgmode
(defrxxconst rxx-clock-string "CLOCK:")

(defconst rxx-ts-regexp0 "\\(\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\) *\\([^]-+0-9>\r\n ]*\\)\\( \\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)"
  "Regular expression matching time strings for analysis.
This one does not require the space after the date, so it can be used
on a string that terminates immediately after the date. 
Taken from org-ts-regexp0")


(defun rxx-parse-time-string (s &optional nodefault)
  "Parse the standard Org-mode time string.
This should be a lot faster than the normal `parse-time-string'.
If time is not given, defaults to 0:00.  However, with optional NODEFAULT,
hour and minute fields will be nil if not given."
  (if (string-match rxx-ts-regexp0 s)
      (list 0
	    (if (or (match-beginning 8) (not nodefault))
		(string-to-number (or (match-string 8 s) "0")))
	    (if (or (match-beginning 7) (not nodefault))
		(string-to-number (or (match-string 7 s) "0")))
	    (string-to-number (match-string 4 s))
	    (string-to-number (match-string 3 s))
	    (string-to-number (match-string 2 s))
	    nil nil nil)
    (error "Not a standard Org-mode time string: %s" s)))

(require 'time-date)

(defun rxx-float-time (&optional time)
  "Convert time value TIME to a floating point number.
TIME defaults to the current time."
  (if (featurep 'xemacs)
      (time-to-seconds (or time (current-time)))
    (float-time time)))

(defrxx rxx-clock-regexp
  (or (seq (named-grp left-bracket "<") (named-grp time (1+ (not (any ">")))) ">")
      (seq (named-grp left-bracket "[")
	   (named-grp time (1+ (not (any ">"))) "]")))
  (rxx-float-time (apply 'encode-time (rxx-parse-time-string time))))

(assert (equal (rxx-parse rxx-clock-regexp "<2010-09-01 Wed 15:49>") 1283370540.0))


(defrxx rxx-clock-range-regexp
  (seq (0+ whitespace) (eval rxx-clock-string) (0+ whitespace) (named-grp from rxx-clock-regexp) (1+ "-") (named-grp to rxx-clock-regexp)) (cons from to))

(assert (equal (rxx-parse rxx-clock-range-regexp "CLOCK:<2010-08-31 Tue 07:43>--<2010-08-31 Tue 13:43>") '(1283254980.0 . 1283276580.0)))
(assert (equal (rxx-parse rxx-clock-range-regexp "CLOCK:[2010-08-31 Tue 07:43]--<2010-08-31 Tue 13:43>") '(1283254980.0 . 1283276580.0)))

;(defrxx rxx-clock-range2-regexp
;  (seq (0+ whitespace) (eval rxx-clock-string) (0+ whitespace) (named-grp from rxx-clock-regexp) (1+ "-") (named-grp to rxx-clock-regexp))
;  (cons (rxx-match-val (list 'from 'time (intern "..") (intern "..") 'to 'time)) to))

;(assert (equal (rxx-parse rxx-clock-range2-regexp "CLOCK:<2010-08-31 Tue 07:43>--<2010-08-31 Tue 13:43>") '("2010-08-31 Tue 13:43" . 1283276580.0)))

;(defconst rxx-clock-range-regexp3
;  (rxx (seq (0+ whitespace) (eval rxx-clock-string) (0+ whitespace) (named-grp from rxx-clock-regexp) (1+ "-") (named-grp to rxx-clock-regexp)
;	    (optional (one-or-more whitespace) (named-backref (from left-bracket)))
;	    )
;       (cons (rxx-match-val '(from time .. .. to time)) to)))

;(rxx-parse rxx-clock-range-regexp2 "CLOCK:<2010-08-31 Tue 07:43>--<2010-08-31 Tue 13:43>      2010-08-31 Tue 07:43")

;(mapcar 'car (rxx-info-env (first (rxx-env-lookup '(from .. to) (rxx-info-env (get-rxx-info rxx-clock-range-regexp))))))

;(defconst rxx-clock-range-regexp2
;  (rxx (seq (0+ whitespace) (eval rxx-clock-string) (0+ whitespace) (named-grp from rxx-clock-regexp) (1+ "-")
;	    (named-grp to (eval-regexp (let ((rxx-replace-named-grps (list (cons (quote left-bracket) (quote (named-backref left-bracket)))))) rxx-clock-regexp)))) (cons from to)))
;(rxx-parse rxx-clock-range-regexp2 "")

(defrxx rxx-strange-expr (seq (named-grp para (seq (named-grp cifry (1+ digit)) (named-grp bukvy (1+ alpha))))
			  (named-backref (para cifry))) (rxx-match-val '(para cifry)))

(assert (equal (rxx-parse rxx-strange-expr "1b1") "1"))

;(assert (equal
;;	 (eval-when-compile (let* ((re2 (rxx (seq "zz" (zero-or-more (seq (named-grp areg rxx-number-regexp) whitespace))) areg-list)))
;	   (rxx-parse re2 "zz1 2 3 "))) '("1 " "2 " "3 ")))

;; (assert (equal (eval-when-compile (let* ((rexp (rxx (or (seq "(" (named-grp-recurs val rexp) ")")
;; 				     (rxx-number-regexp val)) val))
;; 		      (rxx-recurs-depth 2)
;; 		      (rexp2 (rxx-to-string (rxx-info-form (get-rxx-info rexp)) '(cons val nil))))
;; 		 (rxx-parse rexp2 "(1)"))) '(1)))


;; (eval-when-compile (let* ((op-regexp (rxx (or "+" "-" "*" "/") intern))
;;        (rexp (rxx (or (seq "(" (named-grp-recurs val rexp) ")")
;; 		      (rxx-number-regexp valn)
;; 		      (seq (named-grp-recurs left rexp) (optional (op-regexp op)  (named-grp-recurs right rexp)))) (or val valn (funcall op left right)))
;; 	     )
;;        (rxx-recurs-depth 2)
;;        (rexp2 (rxx-to-string (rxx-info-form (get-rxx-info rexp)) '(cons val nil))))
;;   rexp
;;   (rxx-parse rexp2 "(1+2)");
;; ;   (rxx-parse op-regexp "/")
;;   ))

;	 (rxx-parse rexp "1+2")
;	 (let ((rxx-recurs-depth 2))
;	   (rxx-parse (rxx (eval-regexp rexp) theval) "1+2")))
;; (eval-when-compile (let* ((number-regexp (rxx (one-or-more digit) string-to-number))
;;        (fraction-regexp (rxx (seq (named-grp numerator number-regexp) "/" (named-grp denominator number-regexp))
;; 			     (cons numerator denominator)))
;;        (paren-regexp (rxx (seq "(" (named-grp val fraction-regexp) ")") val))
;;        (range-regexp (rxx (seq "[" (named-grp rmin paren-regexp) "]--[" (named-grp rmax paren-regexp) "]")
;; 			  (list rmin rmax))))
;;   (rxx-parse number-regexp "1")))

;; (assert (equal (let* ((exp (rxx (or digit (seq "+" (recurse exp)))))
;; 		      (rxx-recurs-depth 3)
;; 		      (expr (rxx exp)))
;; 		 (rxx-parse expr "+++1")) "+++1"))

;; (assert (equal (eval-when-compile (let* ((num (rxx (1+ digit) string-to-number))
;; 		      (exp (rxx (or (named-grp sub (recurse (seq "(" (exp a) (named-grp op (or "+" "-" "*" "/")) (exp b) ")"))) (num d)) (if (boundp 'a) (list a b d) (list d))))
;; 		      (rxx-recurs-depth 2)
;; 		      (expr (rxx exp (lambda (full) (list full sub d  (rxx-match-val '(sub op))))))
;; 		      (s "(30+(42*57))"))
;; 		 (rxx-parse expr s 'part-ok))) '("(30+(42*57))" "(30+(42*57))" nil "+")))


;; (assert (equal (eval-when-compile (let* ((num (rxx (1+ digit) string-to-number))
;; 		      (exp (rxx (or (named-grp sub (recurse (seq "(" (exp a) (named-grp op (or "+" "-" "*" "/")) (exp b) ")"))) (num d)) (if (boundp 'a) (list a b d) (list d))))
;; 		      (rxx-recurs-depth 3)
;; 		      (expr (rxx exp (lambda (full) (list full sub d  (rxx-match-val '(sub op))))))
;; 		      (s "(1/(30+(42*57)))"))
;; 		 (rxx-parse expr s 'part-ok))) '("(1/(30+(42*57)))" "(1/(30+(42*57)))" nil "/")))

(unless (featurep 'xemacs)
(defun rxx-ppp (x) x)
(defun rxx-qqq (x) (declare (special rxx-ppp-orig)) (* (funcall rxx-ppp-orig x) x))

(assert (equal (elu-flet ( (rxx-ppp (x) (+ x x)) )
		 (declare (special rxx-ppp-orig))
		 (funcall rxx-ppp-orig 4)
		 (rxx-ppp 4)
		 ) 8))

(assert (equal (elu-flet ((rxx-ppp rxx-qqq))
		 (rxx-ppp 33)) 1089))

(defrxx rxx-num-regexp (1+ digit) string-to-number)
(defrxx rxx-op-regexp (or "+" "-" "*" "/") intern)
(defrxx rxx-expr (or
		  (seq "(" (rxx-op-regexp op) blanks (recurse (rxx-expr left)) blanks (recurse (rxx-expr right)) ")")
		  (seq "(" (recurse (rxx-expr in-paren)) ")")
		  (rxx-num-regexp plain-num)) (or (and op (funcall op left right)) in-paren plain-num))

(defrxxrecurse 3 rxx-expr3 (rxx-expr e) e)

  (assert (equal (rxx-parse rxx-expr3 "(+ 1 (* 2 3))") 7))

(defrxx rxx-list-regexp (zero-or-more (seq (named-grp areg rxx-number-regexp) whitespace)) areg-list)

(assert (equal
	 (rxx-parse rxx-list-regexp  "1. .2    \t3.4 ")
	 '(1 0.2 3.4)))
 
(let ((rxx-prefix "rxx-test"))
  (defrxx num digits string-to-number)
  (assert
   (equal (rxx-parse (rxx (sep-by blanks? "(" (sep-by (seq "," blanks?) (1+ num)) ")") num-list) "( 1, 2,3 )" )
	  '(1 2 3)))))

(defrxx an-rxx-num (1+ digit) string-to-number)
(defrxx an-rxx-range (: (an-rxx-num from) "-" (an-rxx-num to)) struct)

(elu-with 'an-rxx-range (rxx-parse an-rxx-range "1-2") (from to)
  (assert (= from 1))
  (assert (= to 2)))

;(rxx (or (& rxx-number-regexp rxx-unit-regexp?) (& rxx-number-regexp? rxx-unit-regexp)))

(message "All rxx tests seem to have passed")

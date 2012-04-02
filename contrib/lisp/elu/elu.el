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
;; General-purpose emacs utilities.  Also includes some functions present
;; in GNU Emacs but not XEmacs; putting them into the elu namespace
;; helps write portable code.
;; Also, includes some functions taken from the CL package, but
;; copied into this package's elu namespace; this allows them to
;; be called at runtime without triggering warnings
;; (see http://www.gnu.org/software/emacs/elisp/html_node/Coding-Conventions.html).
;;
;; See also: `elu-test.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Functions defined here so they can be used in other definitions
;;
;; Logically, they belong in other sections of the code.
;; They will be left in those sections in commented form.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile
  (defun elu-make-seq (x)
    "Return X if X is a list, otherwise return (list X).
Used for iterating over arguments that can be a list or a singleton value."
    (if (listp x) x (list x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Common Lisp functions
;;
;; Functions taken or adapted from the `cl' package.  Putting them into
;; the `elu' namespace allows them to be called at runtime without
;; violating Emacs coding guidelines.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elu-remove-if (predicate seq)
  "Return a new sequence containing elements of SEQ that do not satisfy
PREDICATE."
  (let (result)
    (dolist (e seq)
      (unless (funcall predicate e)
	(push e result)))
    (nreverse result)))

(eval-and-compile
  (defun elu-every (cl-pred cl-seq &rest cl-rest)
    "Return true if PREDICATE is true of every element of SEQ or SEQs.
\n(fn PREDICATE SEQ...).
Taken from `every'."
    (if (or cl-rest (nlistp cl-seq))
	(catch 'cl-every
	  (apply 'map nil
		 (function (lambda (&rest cl-x)
			     (or (apply cl-pred cl-x) (throw 'cl-every nil))))
		 cl-seq cl-rest) t)
      (while (and cl-seq (funcall cl-pred (car cl-seq)))
	(setq cl-seq (cdr cl-seq)))
      (null cl-seq)))

  (defvar *elu-gensym-counter* 0
    "Number suffix to append to variable names generated
by `elu-gensym'.")
  
;;;###autoload
  (defun elu-gensym (&optional prefix)
    "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\".
Taken from `gensym'."
    (let ((pfix (if (stringp prefix) prefix "G"))
	  (num (if (integerp prefix) prefix
		 (prog1 *elu-gensym-counter*
		   (setq *elu-gensym-counter* (1+ *elu-gensym-counter*))))))
      (make-symbol (format "%s%d" pfix num)))))

(eval-and-compile
  (defun elu-list* (arg &rest rest)   ; See compiler macro in cl-macs.el
    "Return a new list with specified ARGs as elements, consed to last ARG.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'.
\n(fn ARG...)
Taken from `list*'."
    (cond ((not rest) arg)
	  ((not (cdr rest)) (cons arg (car rest)))
	  (t (let* ((n (length rest))
		    (copy (copy-sequence rest))
		    (last (nthcdr (- n 2) copy)))
	       (setcdr last (car (cdr last)))
	       (cons arg copy))))))

;;; Support for `progv': copied from `cl' package to avoid runtime dependence on it.
(defvar elu-progv-save)
;;;###autoload
(defun elu-progv-before (syms values)
  "Taken from `progv-before'."
  (while syms
    (push (if (boundp (car syms))
		 (cons (car syms) (symbol-value (car syms)))
	       (car syms)) elu-progv-save)
    (if values
	(set (pop syms) (pop values))
      (makunbound (pop syms)))))

(defun elu-progv-after ()
  "Taken from `progv-after'."
  (while elu-progv-save
    (if (consp (car elu-progv-save))
	(set (car (car elu-progv-save)) (cdr (car elu-progv-save)))
      (makunbound (car elu-progv-save)))
    (pop elu-progv-save)))

(defmacro elu-progv (symbols values &rest body)
  "Bind SYMBOLS to VALUES dynamically in BODY.
The forms SYMBOLS and VALUES are evaluated, and must evaluate to lists.
Each symbol in the first list is bound to the corresponding value in the
second list (or made unbound if VALUES is shorter than SYMBOLS); then the
BODY forms are executed and their result is returned.  This is much like
a `let' form, except that the list of symbols can be computed at run-time.

Taken from `progv'."
  (declare (indent 2))
  (list 'let '((elu-progv-save nil))
	(list 'unwind-protect
	      (list* 'progn (list 'elu-progv-before symbols values) body)
	      '(elu-progv-after))))

(eval-and-compile
  (defun elu-mapcar-many (cl-func cl-seqs)
    "Copy  of `cl-mapcar-many', used by `elu-mapcar*'.

Taken from `cl-mapcar-many'."
    (if (cdr (cdr cl-seqs))
	(let* ((cl-res nil)
	       (cl-n (apply 'min (mapcar 'length cl-seqs)))
	       (cl-i 0)
	       (cl-args (copy-sequence cl-seqs))
	       cl-p1 cl-p2)
	  (setq cl-seqs (copy-sequence cl-seqs))
	  (while (< cl-i cl-n)
	    (setq cl-p1 cl-seqs cl-p2 cl-args)
	    (while cl-p1
	      (setcar cl-p2
		      (if (consp (car cl-p1))
			  (prog1 (car (car cl-p1))
			    (setcar cl-p1 (cdr (car cl-p1))))
			(aref (car cl-p1) cl-i)))
	      (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)))
	    (push (apply cl-func cl-args) cl-res)
	    (setq cl-i (1+ cl-i)))
	  (nreverse cl-res))
      (let ((cl-res nil)
	    (cl-x (car cl-seqs))
	    (cl-y (nth 1 cl-seqs)))
	(let ((cl-n (min (length cl-x) (length cl-y)))
	      (cl-i -1))
	  (while (< (setq cl-i (1+ cl-i)) cl-n)
	    (push (funcall cl-func
			   (if (consp cl-x) (pop cl-x) (aref cl-x cl-i))
			   (if (consp cl-y) (pop cl-y) (aref cl-y cl-i)))
		  cl-res)))
	(nreverse cl-res))))
  
(defun elu-mapcar* (cl-func cl-x &rest cl-rest)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types.
\n(fn FUNCTION SEQ...)

Copied from `mapcar*'.
"
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-x) (nlistp (car cl-rest)))
	  (elu-mapcar-many cl-func (cons cl-x cl-rest))
	(let ((cl-res nil) (cl-y (car cl-rest)))
	  (while (and cl-x cl-y)
	    (push (funcall cl-func (pop cl-x) (pop cl-y)) cl-res))
	  (nreverse cl-res)))
    (mapcar cl-func cl-x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: XEmacs compatibility functions
;;
;; Functions that exist in GNU Emacs but not XEmacs.
;; Putting them in this package helps write portable code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro elu-with-no-warnings (&rest forms)
  "Like `progn', but disables any warnings
inside the code."
  (cons (if (featurep 'xemacs) 'progn 'with-no-warnings) forms))

(defun elu-decompose-region (start end)
  "Decompose text in the current region.

When called from a program, expects two arguments,
positions (integers or markers) specifying the region.

Adapted from `decompose-region'."
  (interactive "r")
  (let ((modified-p (buffer-modified-p))
	(inhibit-read-only t))
    (remove-text-properties start end '(composition nil))
    (set-buffer-modified-p modified-p)))

(defun elu-apply-partially (fun &rest args)
  "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called.

Taken from `apply-partially'."
  (lexical-let ((fun fun) (args1 args))
    (lambda (&rest args2) (apply fun (append args1 args2)))))

(defun elu-remove-list-of-text-properties (start end list-of-properties &optional object)
  "Remove some properties from text from START to END.
The third argument LIST-OF-PROPERTIES is a list of property names to remove.
If the optional fourth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices into it.
Return t if any property was actually removed, nil otherwise."
  (if (featurep 'xemacs)
      (remove-text-properties start end
			      (apply 'append
				     (mapcar
				      (lambda (prop) (list prop nil))
				      list-of-properties))
			      object)
    (remove-list-of-text-properties start end list-of-properties object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Debugging helpers
;;
;; Functions and macros that help debug Emacs Lisp programs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro elu-dbg (&rest exprs)
  "Print the values of exprs, so you can write e.g. (dbg a b) to print 'a=1 b=2'.
Returns the value of the last expression."
  `(let ((expr-vals (list ,@exprs)))
     (when nil (apply 'message
		      (append (list ,(mapconcat (lambda (expr)
						  (concat (format "%s" expr) "=%s "))
						exprs
						""))
		    expr-vals)))
     (car-safe (elu-with-no-warnings (last expr-vals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Macro-writing helpers
;;
;; Functions that help you write macros.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro elu-with-new-symbols (symbols &rest forms)
  "Bind each symbol in SYMBOLS to a newly created uninterned
symbol, and execute FORMS.  Useful for defining temp vars used in
macros. "
  (declare (indent 1))
  (append (list 'let (mapcar (lambda (symbol)
			      (list symbol `(make-symbol ,(symbol-name symbol))))
			    (elu-make-seq symbols)))
	  forms))


(defmacro elu-with-gensyms (symbols &rest body)
  "Execute BODY in a context where the variables in SYMBOLS are bound to
fresh gensyms.  Adapted from URL http://www.emacswiki.org/emacs/macro-utils.el ."
  (assert (elu-every 'symbolp symbols))
  `(let ,(mapcar
	  (lambda (symbol)
	    (list symbol `(elu-gensym (symbol-name (quote ,symbol)))))
	  symbols)
    ,@body))

(defmacro elu-once-only (symbols &rest body)
  "Execute BODY in a context where the values bound to the variables in
SYMBOLS are bound to fresh gensyms, and the variables in SYMBOLS are bound
to the corresponding gensym.
Adapted from URL http://www.emacswiki.org/emacs/macro-utils.el ."
  (setq symbols (elu-make-seq symbols))
  (assert (elu-every #'symbolp symbols))
  (let ((gensyms (mapcar (lambda (x) (elu-gensym x)) symbols)))
    `(elu-with-gensyms ,gensyms
       (list 'let (elu-mapcar* #'list (list ,@gensyms) (list ,@symbols))
        ,(elu-list* 'let (elu-mapcar* #'list symbols gensyms)
           body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Utilities for working with structs (defined by `defstruct').
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro elu-with (struct-type struct fields  &rest body)
  "Locally bind fields FIELDS of structure STRUCT of type STRUCT-TYPE (defined by `defstruct') for easy access.
FIELDS is a list of fields; each field is aliased to a local variable of the same name, then BODY forms are executed.
Setting these local variables will set the corresponding fields of STRUCT.

For example, if you had
\(defstruct my-struct a b)
...
\(setq x (make-my-struct :a 1 :b 2))
then you could write
\(elu-with my-struct x (a b)
  (+ a b))

Similar to WITH construct in Pascal."
  (declare (indent 3))
  (elu-once-only struct
    `(symbol-macrolet
	   ,(mapcar (lambda (field)
		      (list field
			    (list (intern (concat (symbol-name (eval struct-type))
						  "-" (symbol-name field))) struct))) fields)
	 ,@body)))

(defmacro elu-set-fields (struct-type struct &rest clauses)
  "Set multiple fields of a structure"
  (let ((my-struct (make-symbol "my-struct")))
    (let (result)
      (while clauses
	(push (list 'setf (list (intern (concat (symbol-name (eval struct-type)) "-"
						(substring (symbol-name (first clauses)) 1))) my-struct)
		    (second clauses)) result)
	(setq clauses (cddr clauses)))
      (append (list 'let (list (list my-struct struct)))
	      (nreverse result) (list my-struct)))))

(defmacro elu-modified-struct (struct-type struct &rest clauses)
  "Return a copy of the given structure STRUCT of type STRUCT-TYPE, with specified fields given new values and the
remaining fields taking values from STRUCT.   CLAUSES has the form :field1 val1 :field2 val2 ..."
  (declare (indent 2))
  `(elu-with ,struct-type ,struct
		     ,(delq nil
			    (mapcar
			     (lambda (clause)
			       (when (keywordp clause) (intern (substring (symbol-name clause) 1)))) clauses))
     (elu-set-fields ,struct-type (,(intern (concat "copy-" (symbol-name (eval struct-type)))) ,struct) ,@clauses)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Utilities for working with strings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elu-trim-whitespace (s)
  "Trim trailing and leading whitespace from string"
  (save-match-data
    (replace-regexp-in-string "\\(\\`\\s-*\\\\)|\\(*\\'\\)"
     "" (if (symbolp s) (symbol-name s) s))))

(defun elu-not-blank (s)
  "Return nil if S is nil or a blank string, else return S."
  (when (and s (save-match-data (string-match "[^ \t]" s)))
    s))

(defun elu-full-match (re s)
  "Do a string match, but fail unless the regexp matches the full string"
  (and (string-match re s)
       (= (match-beginning 0) 0)
       (= (match-end 0) (length s))))


(defun elu-ends-with (s end)
  "Test if S ends with END"
  (and (>= (length s) (length end)) (string= (substring s (- (length s) (length end))) end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Working with sequences
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro* elu-do-seq ((var i seq &optional result) &rest body)
  "For each element of SEQ, assign it to VAR and its index in SEQ to I and execute
BODY; at the end, return RESULT.  Same as `dolist', but gives access to the index
of each item, like the `enumerate' function in Python.
Also, SEQ will be passed through `elu-make-seq', so if SEQ is a singleton we will
iterate over just that item."
  (declare (indent 3))
  `(let ((,i 0))
     (dolist (,var (elu-make-seq ,seq) ,result)
       (list ,@body (incf ,i)))))

(defun elu-zip (lists)
  "Zip several lists together"
  (when (car lists)
    (cons (mapcar 'car lists)
	  (elu-zip (mapcar 'cdr lists)))))

(defun elu-uniquify (lst &optional compare-fn)
  "Return a new list containing elements of list LST minus any duplicates.
Adapted from `org-uniquify'."
  (let (res)
    (mapc (lambda (x) (elu-add-to-list 'res x 'append compare-fn)) lst)
    res))

(defun elu-groupby (z key-func)
  "Group items in a list by their key, using the specified key extractor.
Return an a-list mapping keys to items with that key. 
Adapted from Python's itertools.groupby(). "
  (setq z (copy-sequence z))
  (setq z (sort z (lambda (x y) (< (funcall key-func x) (funcall key-func y)))))
  (let (result)
    (dolist (x z)
      (let* ((k (funcall key-func x)))
	(when (or (null result) (not (equal k (car (first result)))))
	  (push (cons k nil) result))
	(push x (cdr (first result)))))
    (reverse result)))

(defmacro elu-make-vector (length init)
  "Make a vector, evaluating the INIT expression for each element rather than just once."
  (elu-with-new-symbols (i v)
    `(let ((,v (make-vector ,length nil)))
       (dotimes (,i ,length ,v) (aset ,v ,i ,init)))))

(defmacro elu-gen-vector (i n &rest forms)
  "Construct a vector of size N from the expression for its I'th element."
  (declare (indent 2))
  (elu-with-new-symbols (save-n result)
    `(let* ((,save-n ,n)
	    (,result (make-vector ,save-n nil)))
       (dotimes (,i ,save-n ,result)
	 (aset ,result ,i (progn ,@forms))))))

(defun elu-map-vectors (function &rest vecs)
  "Map a function over one or more vectors, and return the result as a vector."
  (apply 'vector (apply 'elu-mapcar* (cons function vecs))))

(defun elu-number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is \(+ FROM \(* N INC)) where N counts from
zero.  TO is only included if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return \(FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list \(0.4),
whereas \(number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as \(+ FROM \(* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative).

Taken from `number-sequence' in GNU Emacs; present here for XEmacs
compatibility.
"
  (if (or (not to) (= from to))
      (list from)
    (or inc (setq inc 1))
    (when (zerop inc) (error "The increment can not be zero"))
    (let (seq (n 0) (next from))
      (if (> inc 0)
          (while (<= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc))))
        (while (>= next to)
          (setq seq (cons next seq)
                n (1+ n)
                next (+ from (* n inc)))))
      (nreverse seq))))

(defun elu-add-to-list (list-var element &optional append compare-fn)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `equal',
or with COMPARE-FN if that's non-nil.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

The return value is the new value of LIST-VAR.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job.

Taken from `add-to-list'.  Reimplemented here because XEmacs' version
does not have the COMPARE-FN parameter.
"
  (if (cond
       ((null compare-fn)
	(member element (symbol-value list-var)))
       ((eq compare-fn 'eq)
	(memq element (symbol-value list-var)))
       ((and (eq compare-fn 'eql) (fboundp 'memql))
	(memql element (symbol-value list-var)))
       (t
	(let ((lst (symbol-value list-var)))
	  (while (and lst
		      (not (funcall compare-fn element (car lst))))
	    (setq lst (cdr lst)))
          lst)))
      (symbol-value list-var)
    (set list-var
	 (if append
	     (append (symbol-value list-var) (list element))
	   (cons element (symbol-value list-var))))))

(defmacro elu-push-end (elt lst)
  "Push value ELT onto end of list LST, and return the ELT.
Notice that this returns the new element, not the new list.
Notice also that this is a macro that modifies the actual variable
passed as LST.  See also `push'."
  `(progn
     (setq ,lst (append ,lst (list ,elt)))
     ,elt))

(defun elu-caaddr-safe (lst)
  "Short for (car-safe (car-safe (cdr-safe (cdr-safe LST))))."
  (car-safe (car-safe (cdr-safe (cdr-safe lst)))))
  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Miscellaneous functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro elu-flet (bindings &rest body)
  "Temporarily replace functions like `flet', but bind the original definition of
each function F to F-orig so it can be called from the replacement definition.
Also, lets you use a function symbol for the replacement function definition."
  (declare (indent 1))
  `(let 
       ,(mapcar (lambda (binding)
		  (let ((orig-func-symbol (intern (concat (symbol-name (first binding)) "-orig"))))
		    (list orig-func-symbol
			  `(if (boundp (quote ,orig-func-symbol)) ,orig-func-symbol
			     (when (fboundp (quote ,(first binding)))
			       (symbol-function (quote ,(first binding)))))))) bindings)
     (flet ,(append
	     (mapcar
	      (lambda (binding)
		(let ((orig-fn (intern (concat (symbol-name (first binding)) "-orig"))))
		  (list orig-fn '(&rest args) `(apply (symbol-value (quote ,orig-fn)) args))))
	      bindings)
	     (mapcar
	      (lambda (binding)
		(if (and (= (length binding) 2)
			 (symbolp (second binding)))
		    (list (first binding) '(&rest args) `(apply (quote ,(second binding)) args))
		  binding))
	      bindings))
       ,@body)))

(defmacro elu-when-bound (x)
  "If the symbol X is bound then return the value of X, else return nil."
  `(when (boundp (quote ,x)) ,x))

(defun elu-assoc-val (key alist &optional error-message)
  "Looks up KEY in association list ALIST.  Unlike `assoc', returns the associated value rather than the associated pair.
Also, converts key to a symbol if it is a string.
If ERROR-MESSAGE is given, and the key is not in the list, throws an error with this message unless
ERROR-MESSAGE is the symbol `nil-ok', in which case just return nil.
"
  (let ((assoc-result (assoc (if (stringp key) (intern key) key) alist)))
    (if assoc-result (cdr assoc-result)
      (if (eq error-message 'nil-ok) nil
	(error
	 (list (if error-message error-message (format "Key %s not in alist %s" key alist))))))))

(defmacro elu-case-sensitive (&rest body)
  "Execute BODY with casen-sensitivity turned on"
  `(let ((case-fold-search t)) ,body))

(defmacro elu-case-insensitive (&rest body)
  "Execute BODY with casen-sensitivity turned off"
  `(let (case-fold-search) ,body))


(defun elu-format-seconds (string seconds)
  "Use format control STRING to format the number SECONDS.
The valid format specifiers are:
%y is the number of (365-day) years.
%t is the number of (30-day) months.
%w is the number of weeks.
%d is the number of days.
%h is the number of hours.
%m is the number of minutes.
%s is the number of seconds.
%z is a non-printing control flag (see below).
%% is a literal \"%\".

Upper-case specifiers are followed by the unit-name (e.g. \"years\").
Lower-case specifiers return only the unit.

\"%\" may be followed by a number specifying a width, with an
optional leading \".\" for zero-padding.  For example, \"%.3Y\" will
return something of the form \"001 year\".

The \"%z\" specifier does not print anything.  When it is used, specifiers
must be given in order of decreasing size.  To the left of \"%z\", nothing
is output until the first non-zero unit is encountered.

This function does not work for SECONDS greater than `most-positive-fixnum'.

This code is mostly copied from `format-seconds', with the addition of support
for weeks and months.  Also, it does not print zero units at the end (not implemented
yet).
"
  (let ((start 0)
        (units '(("y" "year"   31536000)
		 ("t" "month"  18144000)
                 ("w" "week"     604800)
                 ("d" "day"       86400)
                 ("h" "hour"       3600)
                 ("m" "minute"       60)
                 ("s" "second"        1)
                 ("z")))
        (case-fold-search t)
        spec match usedunits zeroflag larger prev name unit num zeropos)
    (while (string-match "%\\.?[0-9]*\\(.\\)" string start)
      (setq start (match-end 0)
            spec (match-string 1 string))
      (unless (string-equal spec "%")
	;; `assoc-string' is not available in Emacs 21.  So when compiling
	;; Gnus (`time-date.el' is part of Gnus) with Emacs 21, we get a
	;; warning here.  But `format-seconds' is not used anywhere in Gnus so
	;; it's not a real problem. --rsteib
        (or (setq match (assoc-string spec units t))
            (error "Bad format specifier: `%s'" spec))
        (if (assoc-string spec usedunits t)
            (error "Multiple instances of specifier: `%s'" spec))
        (if (string-equal (car match) "z")
            (setq zeroflag t)
          (unless larger
            (setq unit (nth 2 match)
                  larger (and prev (> unit prev))
                  prev unit)))
        (push match usedunits)))
    (and zeroflag larger
         (error "Units are not in decreasing order of size"))
    (dolist (u units)
      (setq spec (car u)
            name (cadr u)
            unit (nth 2 u))
      (when (string-match (format "%%\\(\\.?[0-9]+\\)?\\(%s\\)" spec) string)
        (if (string-equal spec "z")     ; must be last in units
            (setq string
                  (replace-regexp-in-string
                   "%z" ""
                   (substring string (min (or zeropos (match-end 0))
                                          (match-beginning 0)))))
          ;; Cf article-make-date-line in gnus-art.
          (setq num (floor seconds unit)
                seconds (- seconds (* num unit)))
          ;; Start position of the first non-zero unit.
          (or zeropos
              (setq zeropos (unless (zerop num) (match-beginning 0))))
          (setq string
                (replace-match
                 (format (concat "%" (match-string 1 string) "d%s") num
                         (if (string-equal (match-string 2 string) spec)
                             ""       ; lower-case, no unit-name
                           (format " %s%s" name
                                   (if (= num 1) "" "s"))))
                 t t string))))))
  (replace-regexp-in-string "%%" "%" string))

(defmacro elu-save (&rest saves-then-forms)
  "Shorthand for nesting requests such as save-excursion, save-restriction etc.
Instead of (save-excursion (save-window-excursion (save-restriction form1 form2)))
use (elu-save excursion window-excursion restriction form1 form2).
SAVES-THEN-FORMS is a list of save- forms to call, without the save- prefix;
followed by the list of forms to wrap in these save- calls.
"
  (declare (indent defun))
  (let (restriction-saved)
    (flet ((elu-save-recurs
	    (&rest saves-then-forms)
	    (let ((first-save (car-safe saves-then-forms)))
	      (if (not (memq first-save '(excursion window-excursion restriction match-data current-buffer)))
		  (cons 'progn saves-then-forms)
		(when (eq first-save 'restriction) (setq restriction-saved t))
		(when (and (eq first-save 'excursion) restriction-saved)
		  (error "elu-save: excursion should be saved before restriction, according to `save-restriction' docs"))
		(list (intern (concat "save-" (symbol-name first-save)))
		      (apply 'elu-save-recurs (cdr saves-then-forms)))))))
      (apply 'elu-save-recurs saves-then-forms))))

;; add a general elu-chain thing for chaining symbols.  elu-save can then call that.

(defmacro elu-ignoring (&rest ignore-funcs-then-forms)
  "Execute the given forms while ignoring calls to specified functions.
Temporarily bind function symbols at the head of the list
IGNORE_FUNCS_THEN_FORMS to `ignore', and execute the remaining
forms in this list.  See caveats re: limitations of flet at
URL http://www.gnu.org/software/emacs/manual/html_node/cl/Function-Bindings.html#Function-Bindings
"
  (declare (indent defun))
  (flet ((elu-ignoring-recurs
	  (&rest ignore-funcs-then-forms)
	  (let ((first-ignore (car-safe ignore-funcs-then-forms)))
	    (if (not (symbolp first-ignore))
		(cons 'progn ignore-funcs-then-forms)
	      `(flet ((,first-ignore (&rest args) nil))
		 ,(apply 'elu-ignoring-recurs (cdr ignore-funcs-then-forms)))))))
    (apply 'elu-ignoring-recurs ignore-funcs-then-forms)))

(defmacro elu-protect-vars (vars &rest forms)
  "Execute forms while saving and restoring the values of the variables in VARS.
Shorthand for (let ((var1 var1) (var2 var2)) forms)."
  (declare (indent 1))
  (list 'let (mapcar (lambda (var) (list var var)) vars)
	(cons 'progn forms)))

(defstruct elu-loc
  "A buffer and a position in that buffer.  Unlike for a marker, the position is not
updated as the buffer is edited, so you can created as many of these as you want without
slowing down emacs and not worry about manually disposing of them after use
(see URL http://www.gnu.org/software/emacs/manual/html_node/elisp/Overview-of-Markers.html#Overview-of-Markers ).
It also stores the filename of the buffer when available, so the location makes sense
even if the file gets closed.  It also stores the value of `buffer-chars-modified-tick' at the time
of creation, so it can detect when it is no longer valid."
  (buffer (current-buffer) :read-only t)
  (position (point) :read-only t)
  (filename (buffer-file-name) :read-only t)
  (chars-modified-tick (buffer-chars-modified-tick) :read-only t))

(defun elu-loc-valid-p (loc)
  "Test whether the given location LOC (a marker or an elu-loc) is still valid."
  (cond
   ((markerp loc) (buffer-live-p (marker-buffer loc)))
   ((elu-loc-p loc)
    (elu-with 'elu-loc loc (buffer)
      (and (buffer-live-p buffer)
	   (= (buffer-chars-modified-tick buffer)
	      (elu-loc-chars-modified-tick loc)))))
   ((error "elu-loc-valid-p got invalid loc %s" loc))))

(defun point-elu-loc ()
  "Return an `elu-loc' pointing to the current point."
  (make-elu-loc))

(defun point-min-elu-loc ()
  "Return an elu-loc for (point-min)"
  (make-elu-loc :position (point-min)))

(defun point-max-elu-loc ()
  "Return an elu-loc for (point-max)"
  (make-elu-loc :position (point-max)))

(defstruct
  (elu-loc-range
   (:constructor nil)
   (:constructor
    make-elu-loc-range
    (beg end &optional buffer)))
  
  "A contiguous range of locations within one buffer."
  beg end (buffer (current-buffer)))

(defun* elu-goto (loc &optional (buffer-switch-fn 'switch-to-buffer))
  "Got to a location, which can be a position in the current buffer, a marker,
or an `elu-loc'.  BUFFER-SWITCH-FN gives the function to use for switching
buffers (either `switch-to-buffer' or`set-buffer')."
  (unless loc (message "no loc??" ) (backtrace) (error "loc required"))
  (cond
   ((integerp loc) (goto-char loc))
   ((markerp loc)
    (funcall buffer-switch-fn (marker-buffer loc))
    (goto-char (marker-position loc)))
   ((elu-loc-p loc)
    (funcall buffer-switch-fn (elu-loc-buffer loc))
    (goto-char (elu-loc-position loc)))
   ((elu-loc-range-p loc)
    (elu-with 'elu-loc-range loc (buffer beg end)
      (funcall buffer-switch-fn buffer)
      (widen)
      (narrow-to-region (elu-loc-position beg)
			(elu-loc-position end))
      (goto-char (point-min))))
   ((error "elu-goto: invalid loc %s" loc))))
  

(defmacro elu-cache-at-point (prop form)
  ;; add option to use buffere-chars-modified-tick or buffer-modified-tick or another tick routine
  ;; add a macro that ignores new updates for this symbol so clear the cache(?)  but then future updates
  ;; do not matter for the purposes of this macro.

  ;; also have option to cache the results in a user-provided space.
  ;; and to invalidate the cache only if a given range of the file changes.
  ;; (so, we add a text property that, on change

  ;;
  ;; or just use first-change-hook

  ;;
  ;; check xemacs compatibility .  always safe to just not cache.

;;   modification-hooks
;; If a character has the property modification-hooks, then its value should be a list of functions; modifying that character calls all of those functions. Each function receives two arguments: the beginning and end of the part of the buffer being modified. Note that if a particular modification hook function appears on several characters being modified by a single primitive, you can't predict how many times the function will be called.
;; If these functions modify the buffer, they should bind inhibit-modification-hooks to t around doing so, to avoid confusing the internal mechanism that calls these hooks.

;; Overlays also support the modification-hooks property, but the details are somewhat different (see Overlay Properties). 

;; insert-in-front-hooks
;; insert-behind-hooks
;; The operation of inserting text in a buffer also calls the functions listed in the insert-in-front-hooks property of the following character and in the insert-behind-hooks property of the preceding character. These functions receive two arguments, the beginning and end of the inserted text. The functions are called after the actual insertion takes place.
;; See also Change Hooks, for other hooks that are called when you change text in a buffer. 
  ;;

  ;; make sure to bind inhibit-modification-hooks as needed here, esp. since removing the cache property
  ;; counts as a modification.
  
  "Cache the result of form"
  (elu-with-new-symbols (saved saved-tick saved-val cur-val cur-tick)
    `(let* ((,saved (get-text-property (point) ,prop))
	    (,saved-tick (car-safe ,saved))
	    (,saved-val (cdr-safe ,saved))
	    (,cur-tick (buffer-chars-modified-tick)))
       (if (equal ,saved-tick ,cur-tick) ,saved-val
	 (let* ((,cur-val ,form) inhibit-modification-hooks)
	   (put-text-property (point) (1+ (point)) ,prop
			      (cons ,cur-tick ,cur-val))
	   ,cur-val)))))

;;;;;;;;;;;;;;;;;;;

(defun elu-add-font-lock-keywords()
  "Make some elu macros look like keywords in font lock"
  (when (and (featurep 'font-lock)
	     (fboundp 'font-lock-add-keywords))
    (font-lock-add-keywords 'emacs-lisp-mode
			    '(("elu-save" . font-lock-keyword-face)
			      ("elu-ignoring" . font-lock-keyword-face)))))

(add-hook 'emacs-lisp-mode-hook 'elu-add-font-lock-keywords)

(defmacro elu-require (&rest modules)
  "Shorthand for requiring many modules in one command"
  (cons 'progn
	(mapcar (lambda (module) `(require (quote ,module)))
		modules)))

(defstruct elu-ratio
  "A ratio of two things."
  numer denom)

; add elu-protect to do (let ((a a)) ((b b)) ...forms...)

; a dynamic-binding version of symbol-macrolet, which will eval
; an expr every time .

; symbol-flet 
;

(provide 'elu)

;;; elu.el ends here


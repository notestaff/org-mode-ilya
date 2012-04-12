;;; rxx.el --- Easily build simple parsers using regexps
;; Copyright (C) 2010 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: regexp, parser, DSL
;; Homepage: http://ilya.cc/rxx
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
;; Tools for building complex regular expressions from simpler building
;; blocks.  Implemented as an extension of the `rx' macro that translates
;; regular expressions represented in sexp form to the string representation
;; passed to `string-match'.   Tools in the `rxx' module lets you define
;; a regexp in sexp form, associate with it a parser that constructs a
;; programmatic object from a match to the regexp, and use it as a building
;; block in larger regexps.
;;
;; See the docstring of `rxx' for detailed documentation.
;;

;;
;; Externally callable functions and macros:
;;
;;  Defining regexps:
;;
;;  `defrxx', `defrxxrecurse'
;;
;;  Parsing:
;;
;;  `rxx-parse'
;;
;;  General utils:
;;
;;  `rxx-make-shy'
;;

;;
;; Implementation notes:
;;
;; Extension to the `rx' macro for writing readable regexps; adds parsing of named subexpressions.
;; In effect lets you define and parse grammars.

;; explain the issue with modularity and group numbers in subexprs.

;;
;; todo:
;;
;;    - need a simple facility like string-match where you can just give an anonymous pattern and then parse
;;      the results, without constructing an object.  (but ok e.g. to have a special form within which to do it.)
;;
;;      but then also need an analog of save-match-data form.
;; 
;;    - flag duplicately-named groups within regexp
;;
;;    - consider what can be done at compile-time with rx as opposed to rx-to-string.
;;    - should we just add our extra info as a string property of the regexp?  then you can keep
;;      storing it as a string, and using it as regexp.
;;
;;  so, rxx-string-match would call string-match on the string.
;;  then, it would get the data from the string as text prop.

;;
;;  or, you could just have a working org-make-shy routine, and then you can include
;;  subexprs.  you'd just need to call the correct parse routines for them.
;;

;; user-callable functions: rxx-named-grp-num, rxx, rxx-match-val, rxx-match-string, rxx-parse


;;; History:
;; 

(require 'rx)
(require 'elu)
;;; Code:
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Customization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup rxx nil
  "Options for the rxx regexp library"
  :tag "rxx library options"
  :group 'elu)

(defcustom rxx-max-lisp-eval-depth 1200
  "Value to which to increase `max-lisp-eval-depth' while parsing.
Parsing can get highly nested so this can be needed especially if not byte-compiling."
  :group 'rxx
  :type 'integer)

(defcustom rxx-max-specpdl-size 1200
  "Value to which to increase `max-specpdl-size' while parsing.
Parsing can get highly nested so this can be needed especially if not byte-compiling."
  :group 'rxx
  :type 'integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Utils for manipulating regexp strings
;;
;; These work with plain Emacs regexp strings, not just the annotated ones
;; defined by this package.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rxx-check-regexp-valid (regexp)
  "Throw an error unless REGEXP is a valid regexp; if it is valid, return it."
  (string-match regexp "")
  regexp)

(defun rxx-replace-posix (re)
  "Return a version of regexp RE with POSIX classes replaced, for xemacs
compatibility.  Adapted from `org-re'."
  (when (featurep 'xemacs)
    (save-match-data
      (dolist (repl '((alnum "a-zA-Z0-9") (word "a-zA-Z0-9") (alpha "a-zA-Z") (digit "0-9")
		      (lower "a-z") (upper "A-Z")
		      (blank " \t") (ascii "\000-\127") (punct "\001-@[-`{-~")))
	(while (string-match (concat "\\[:" (symbol-name (first repl)) ":\\]") re)
	  (setq re (replace-match (second repl) 'fixedcase 'literal re))))))
  (rxx-check-regexp-valid re))

(defun rxx-remove-unneeded-shy-grps (re)
  "Return a regexp equivalent to RE but removing shy groups that have no effect."
  (while (and t (>= (length re) 10) (string= (substring re 0 8) "\\(?:\\(?:")
     	      (string= (substring re -4) "\\)\\)"))
    (setq re (substring re 4 -2)))
  (rxx-check-regexp-valid re))
  
(defun rxx-remove-outer-shy-grps (re)
  "Return regexp equivalent to RE but with any outer shy groups removed."
  (while (and nil (>= (length re) 6) (string= (substring re 0 4) "\\(?:")
	      (string= (substring re -2) "\\)"))
    (setq re (substring re 4 -2)))
  (rxx-check-regexp-valid re))

(defun rxx-subregexp-context-p (regexp pos &optional start)
  "Return non-nil if POS is in a normal subregexp context in REGEXP.
A subregexp context is one where a sub-regexp can appear.
A non-subregexp context is for example within brackets, or within a
repetition bounds operator `\\=\\{...\\}', or right after a `\\'.
If START is non-nil, it should be a position in REGEXP, smaller
than POS, and known to be in a subregexp context.

Taken from `subregexp-context-p'."
  ;; Here's one possible implementation, with the great benefit that it
  ;; reuses the regexp-matcher's own parser, so it understands all the
  ;; details of the syntax.  A disadvantage is that it needs to match the
  ;; error string.
  (save-match-data
    (condition-case err
	(progn
	  (string-match (substring regexp (or start 0) pos) "")
	  t)
      (invalid-regexp
       (not (member (cadr err) '("Unmatched [ or [^"
				 "Unmatched \\{"
				 "Trailing backslash")))))))

(defun rxx-make-shy (regexp)
  "Return a new regexp that makes all groups in REGEXP shy; and
wrap a shy group around the returned REGEXP.  WARNING: this will
kill any backrefs!  Adapted from `regexp-opt-depth'."
  (save-match-data
    (assert (not (string-match "\\\\[1-9]" regexp))
	    nil "rxx-make-shy: does not work on regexps with backrefs")
    ;; Hack to signal an error if REGEXP does not have balanced parentheses.
    (string-match regexp "")
    ;; Count the number of open parentheses in REGEXP.
    
      ;; so, you need to replace non-shy groups.
      ;; under emacs, you also need to replace numbered groups.
    (unless (featurep 'xemacs)
      ;; remove explicitly numbered groups (xemacs does not support them)
      (while (and (string-match "\\\\(\\?\\([0-9]+\\):" regexp)
		  (rxx-subregexp-context-p regexp (match-beginning 0)))
	(setq regexp (replace-match "" 'fixedcase 'literal regexp 1))))
    ;; remove unnumbered, non-shy groups.
    (if (featurep 'xemacs)
	(while (and (string-match "\\\\(\\([^?]\\)" regexp)
		    (rxx-subregexp-context-p regexp (match-beginning 0)))
	  (setq regexp (replace-match "\\(?:\\1" 'fixedcase (not 'literal) regexp)))
    (while (and (string-match "\\\\(\\(\\)[^?]" regexp)
		(rxx-subregexp-context-p regexp (match-beginning 0)))
      (setq regexp (replace-match "?:" 'fixedcase 'literal regexp 1))))
    (rxx-check-regexp-valid regexp)
    (assert (zerop (rxx-opt-depth regexp)))
    regexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: rxx-regexp representation
;;
;; Extra information we store with each constructed regexp.  This information
;; lets us reuse the regexp as a building block of larger regexps.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct
  rxx-def
  "The definition of an rxx regexp.

Fields:

   NAMESPACE - the namespace to which this rxx-def belongs.  a symbol.
   NAME - name of the aregexp.  a symbol.
   DESCR - the description of this regexp.  typically says what it matches as
     well as what it gets parsed into by the PARSER below.
   FORM - the form defining this regexp, in an extension of `rx' syntax
   PARSER - the parser for this regexp, that turns its matches into programmatic
     objects.  Either a lisp form, or a one-argument function.  Can refer to the
     result of parsing named subgroups by the subgroup names.
"
  ;; other things to store here:
  ;; case-sens, posix-search, 
  
  namespace name descr form (parser 'identity))



(defstruct rxx-inst
  "Information about a particular instantiation of an `rxx-def', either at the
top level or as part of a larger regexp.  When `rxx-instantiate' analyzes an
sexp defining a regexp, it creates one `rxx-inst' for the overall
regexp and one for each named subgroup within the regexp.
This extended information enables us to parse
matches of this regexp into programmatic objects, and to use this
regexp as a building block in larger regexps.

Fields:
     DEF - the definition of the rxx-regexp that we are instantiating
     ENV - environment for resolving references to named subgroups of this regexp.  Maps subgroup name to
       `rxx-inst' for the subgroup.
     NUM - the numbered group corresponding to matches of this rxx-inst within the larger regexp. (as would be passed to `match-string').
     REGEXP - the regexp string, as passed to `string-match' etc.
"
  def env num regexp)

(defun rxx-inst-form (rxx-inst) (rxx-def-form (rxx-inst-def rxx-inst)))
(defun rxx-inst-parser (rxx-inst) (rxx-def-parser (rxx-inst-def rxx-inst)))
(defun rxx-inst-descr (rxx-inst) (rxx-def-descr (rxx-inst-def rxx-inst)))


(defun get-rxx-inst (aregexp)
  "Extract rxx-inst from regexp struct AREGEXP,
if there, otherwise return nil."
  (when (rxx-inst-p aregexp)
    aregexp))

(defun rxx-opt-depth (aregexp)
  "Return `regexp-opt-depth' of the regexp string in AREGEXP."
  (regexp-opt-depth aregexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Error handling
;;
;; Error symbols used in this package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'rxx-error 'error-conditions '(error elu-error rxx-error))
(put 'rxx-error 'error-message "Error in module `rxx'")
(put 'rxx-parse-error 'error-conditions '(error elu-error rxx-error rxx-parse-error))
(put 'rxx-parse-error 'error-message "Error parsing an rxx-regexp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Environments
;;
;; Map from named group name to information about that named group.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rxx-new-env (&optional parent-env)
  "Create a fresh environment mapping group names to rxx-insts,
with the parent environment PARENT-ENV.
There is an environment for the top-level regexp, and also a
separate one within each named group (for nested named groups).
We represent the environment as an alist.  The alist always has
at least one cell; this lets us add entries to an environment
that is bound (pointed to) by several lisp symbols, without
having to find and rebind all the symbols."
  (list (cons nil nil)))

(defun rxx-parent-env (rxx-env)
  "Return the parent environment of the environment RXX-ENV."
  (cdr (first rxx-env)))

(defun rxx-env-lookup (grp-name rxx-env)
  "Lookup the rxx-inst for the named group GRP-NAME in the
environment RXX-ENV, or return nil if not found.  GRP-NAME is
either a symbol, or a list of symbols indicating a path through
nested named groups.  Since multiple groups may be bound to the
same name in an environment (because mutually exclusive variants
may contain named subgroups of the same name), this returns a list."
  (when (symbolp grp-name) (setq grp-name (list grp-name)))
  (if (eq (first grp-name) (intern ".."))
      (rxx-env-lookup (cdr grp-name) (rxx-parent-env rxx-env))
    (let ((grp-infos (cdr-safe (assq (first grp-name) (cdr rxx-env)))))
      (elu-uniquify
       (apply 'append
	      (mapcar
	       (lambda (grp-info)
		 (if (cdr grp-name)
		     (rxx-env-lookup (cdr grp-name)
				     (rxx-inst-env grp-info))
		   (list grp-info)))
	       grp-infos)) 'eq))))

(defun rxx-env-bind (grp-name rxx-inst rxx-env)
  "Bind group name GRP-NAME to group annotation RXX-INST in the
environment RXX-ENV.  If already bound, add to the binding."
  (let ((entry (or (assq grp-name rxx-env)
		   (let ((new-entry (list (cons grp-name nil))))
		     (nconc rxx-env new-entry)
		     (first new-entry)))))
    (setcdr entry (cons rxx-inst (cdr entry)))
    rxx-env))

(defmacro do-rxx-env (grp-name rxx-insts rxx-env &rest forms)
  "Execute forms for each (grp-name, rxx-inst) binding in this env"
  (declare (indent 3))
  (let ((cur-var (make-symbol "cur")))
    `(let ((,cur-var (cdr ,rxx-env)))
       (while ,cur-var
	 (let ((,grp-name (car (car ,cur-var)))
	       (,rxx-insts (cdr (car ,cur-var))))
	   ,@forms)
	 (setq ,cur-var (cdr ,cur-var))))))

(defun rxx-env-groups (rxx-env)
  "Return the list of top-level named groups in the environment RXX-ENV."
  (let (all-grps)
    (do-rxx-env grp-name rxx-insts rxx-env
      (push grp-name all-grps))
    all-grps))

(defun rxx-env-empty-p (rxx-env)
  "Return true if there are no bindings in the environment RXX-ENV."
  (null (cdr rxx-env)))

(defun rxx-named-grp-num (grp-name &optional aregexp)
  "Look up the explicitly numbered group number assigned to the
given named group, for passing as the SUBEXP argument to routines
such as `replace-match', `match-substitute-replacement' or
`replace-regexp-in-string'.  The annotated regexp must either be
passed in as AREGEXP or scoped in as RXX-AREGEXP. "
  (declare (special rxx-aregexp))
  (mapcar 'rxx-inst-num
   (rxx-env-lookup
    grp-name
    (rxx-inst-env
     (get-rxx-inst
      (or aregexp (elu-when-bound rxx-aregexp)
	  (error "The annotated regexp must be either passed in explicitly, or scoped in as `rxx-aregexp'")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Parsing the result of a regexp match into a programmatic object
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rxx-call-parser (rxx-inst match-str)
  "Call the parser to parse the given match string."
  (let ((rxx-env (rxx-inst-env rxx-inst)))
    ;; For each named subgroup, recursively parse what
    ;; it matched and assign the resulting parsed object
    ;; to a variable of the same name as the subgroup.
    (let* ((symbols (delq nil (mapcar 'car (rxx-inst-env rxx-inst))))
	   (symbol-vals (mapcar
			 (lambda (symbol)
			   (rxx-match-val symbol))
			 symbols))
	   (parser (rxx-inst-parser rxx-inst)))
      (elu-progv symbols symbol-vals
	(save-match-data
	  (if (functionp parser)
	      (funcall parser match-str)
	    (eval parser)))))))

(defun rxx-match-aux (code)
  "Common code of `rxx-match-val', `rxx-match-string', `rxx-match-beginning' and `rxx-match-end'.  Looks up the rxx-inst
for the relevant named group, so that we can get the corresponding group explicitly numbered group number and pass it
to `match-string', `match-beginning' or `match-end'."
  (declare (special grp-name object aregexp rxx-object rxx-aregexp rxx-env))
  (save-match-data
    (let* ((rxx-env
	    (or (elu-when-bound rxx-env)
	      (let ((aregexp (or aregexp (when (boundp 'rxx-aregexp) rxx-aregexp))))
		(rxx-inst-env
		 (or
		  (get-rxx-inst aregexp)
		  (error "Annotated regexp created by `rxx' must either be passed in, or scoped in via RXX-AREGEXP"))))))
	   (grp-infos (or (rxx-env-lookup grp-name rxx-env) (error "Named group %s not found" grp-name)))
	   (matches-here
	    (delq nil
		  (mapcar
		   (lambda (grp-info)
		     (let ((grp-num (rxx-inst-num grp-info)))
		       (when grp-num
			 (let ((match (match-string grp-num
						    (or object
							(elu-when-bound rxx-object)))))
			   (when match (cons match grp-info))))))
		   grp-infos))))
      (when matches-here
	(unless (= (length matches-here) 1) (error "More than one match to group %s: %s" grp-name matches-here))
	(let* ((match-info-here (first matches-here))
	       (match-here (car match-info-here))
	       (grp-info (cdr match-info-here))
	       (grp-num (rxx-inst-num grp-info)))
	  (funcall code))))))

(defun rxx-match-val (grp-name &optional object aregexp)
  "Return the parsed object matched by named group GRP-NAME.  OBJECT, if given, is the string or buffer we last searched;
may be scoped in via RXX-OBJECT.  The annotated regexp must either be passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special rxx-object rxx-aregexp grp-info match-here))
  (rxx-match-aux
   (lambda ()
       (let ((rxx-env (rxx-inst-env grp-info)))
	 (rxx-call-parser grp-info match-here)))))

(defun rxx-match-string (grp-name &optional object aregexp)
  "Return the substring matched by named group GRP-NAME.  OBJECT, if given, is the string or buffer we last searched;
may be scoped in via RXX-OBJECT.  The annotated regexp must either be passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special rxx-object rxx-aregexp match-here))
  (rxx-match-aux (lambda () match-here)))

(defun rxx-get-marker (pos)
  "If searching within a string copied from the buffer,
convert position within the string to buffer marker position"
  (when (boundp 'rxx-marker)
    (let ((m (copy-marker rxx-marker)))
      (setq pos (set-marker m (+ (marker-position m) pos) (marker-buffer m)))))
  pos)

(defun rxx-match-beginning (grp-name &optional object aregexp)
  "Return the beginning position of the substring matched by named group GRP-NAME.  The annotated regexp must either be
passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special grp-num))
  (rxx-match-aux
   (lambda () (rxx-get-marker (match-beginning grp-num)))))

(defun rxx-match-end (grp-name &optional object aregexp)
  "Return the end position of the substring matched by named group GRP-NAME.  The annotated regexp must either be
passed in via AREGEXP or scoped in via RXX-AREGEXP."
  (declare (special grp-num))
  (rxx-match-aux (lambda () (rxx-get-marker (match-end grp-num)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: A simple module system for aregexp names
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct rxx-namespace
  "A namespaces for regexps.

Fields:

   NAME - the name of this namespace
   IMPORTS - the names of any imported namespaces
"
  name imports)

(defun rxx-namespace-global-var (name)
  "Construct the name of the global var storing the given namespace"
  (intern (concat (symbol-name name) "-rxx-namespace")))

(defmacro* def-rxx-namespace (name &optional (descr (concat "Namespace" (symbol-name name))) &key imports)
  "Define an rxx namespace.  Each rxx-regexp define using `def-rxx-regexp' belongs to a particular
namespace -- typically, the name of the elisp module in which the regexp is defined.

Params:

   NAME - the name of the namespace, a symbol.
   DESCR - a string describing the namespace
   IMPORTS - the namespaces imported into this namespace.   Importing a namespace imports all symbols from that
      namespace into the namespace NAME.  Typically, this corresponds to the names of modules that your
      module requires (but only those modules that define rxx-regexps that your module uses).
"
  `(defconst ,(rxx-namespace-global-var name)
     (make-rxx-namespace :name (quote ,name) :imports (quote ,(elu-make-seq imports)))
     ,descr))

(defun rxx-def-global-var (namespace name)
  "Construct the name of the global var storing the given rxx-def"
  (intern (concat (symbol-name namespace) "-" (symbol-name name) "-rxx-def")))

(defmacro* def-rxx-regexp (namespace name descr form &optional (parser 'identity))
  "Define an rxx regexp.

  Params:

    NAMESPACE - the namespace for this regexp.
    NAME - the name for this regexp within NAMESPACE
    DESCR - a string documenting what this regexp matches, and what the matches are
      parsed into by PARSER
    FORM - the sexp defining the regexp (described below).
    PARSER - an elisp form or function that parses matches to this regexp into
programmatic objects.  If a function, it should take one argument -- the string that matched
the whole regexp.   The parser can refer to the results of parsing any named subgroup
by the name of the subgroup.  If a subgroup G is inside a repeat expression, the parser can
refer to the list of parsed matches as G-LIST.

 Syntax of FORM:

 Everything supported by `rx' is allowed.  In addition, the following extensions are supported:

   (named-grp NAME FORM)
"
  `(defconst ,(rxx-def-global-var namespace name)
     (make-rxx-def :namespace (quote ,namespace) :name (quote ,name) :descr ,descr
		   :form (quote ,form) :parser (quote ,parser))
     ,descr))

(defmacro def-rxx-regexps (namespace &rest regexp-defs)
  "Define several regexps in the same NAMESPACE."
  (cons 'progn
	(mapcar
	 (lambda (regexp-def)
	   (append (list 'def-rxx-regexp namespace) regexp-def))
	 regexp-defs)))

(defun* rxx-find-symbol-namespace (symbol &optional namespace)
  "Return the namespace for the given rxx-regexp name"
  (declare (special rxx-cur-namespace))
  (let ((rxx-cur-namespace (or namespace (elu-when-bound rxx-cur-namespace))))
    (when rxx-cur-namespace
      (dolist (test-namespace (cons rxx-cur-namespace (rxx-namespace-imports
						       (symbol-value
							(rxx-namespace-global-var
							 rxx-cur-namespace)))))
	(when (boundp (rxx-def-global-var test-namespace symbol))
	  (return-from rxx-find-symbol-namespace test-namespace))))))


;; 	  ;; FIXME need local instantiations _for different params_ such as recursion depth, maybe greediness, etc.
;; 	  ;; so, keep an alist.
;; 	  ;; maybe, a single one for all: rxx-insts.
;; 	  ;; then people can bind it to nil if they want to.
	  
(defun rxx-find-def (symbol &optional namespace)
  "If SYMBOL names an rxx-regexp defined in the current namespace, return its definition,
else return nil."
  (let ((namespace (rxx-find-symbol-namespace symbol namespace)))
    (when namespace
      (symbol-value (rxx-def-global-var namespace symbol)))))

;; operations in terms of which everything else can be written:

;; given a symbol:
;;   - return the full namespace:symbol, or nil if does not exist
;;        --> if specified, use that; if not, look if to see if this symbol was defined
;;         in rxx-cur-namespace or things it imports.   rxx-cur-namespace can start out
;;        as the global constant global-rxx-namespace.
;;   - store rxx-def for a given symbol in namespace.
;;   - store/load an instantiation of the symbol in a local namespace.
;;    local can be a buffer-local variable but also a local binding.
;     for the latter to work, it'd be best to have one var keep the definitions.
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Handling new `rx' forms, and extended handling of existing forms
;;          to recognize the use of aregexps.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rxx-process-named-grp (form)
  "Process the (named-grp GRP-NAME GRP-DEF) form, when called from `rx-to-string'.
GRP-DEF can be an annotated regexp, a plain regexp, or a form to
be recursively interpreted by `rxx'.  If it is an annotated
regexp, you can call `rxx-match-val' after doing a match to get
the parsed object matched by this named group."
  (declare (special rxx-num-grps rxx-env))
  (rx-check form)
  (or
   (and (boundp 'rxx-replace-named-grps)
	(cdr-safe (assoc (second form) rxx-replace-named-grps)))
   (let* ((grp-name (second form))
	  (grp-def-raw
	   (or (third form) (error "Missing named group definition: %s" form)))
	  (grp-num (incf rxx-num-grps))
	  (old-rxx-env rxx-env)
	  (rxx-env (rxx-new-env old-rxx-env))  ;; within each named group, a new environment for group names
	  (grp-def
	   (or
	    (and (symbolp grp-def-raw) (rxx-find-def grp-def-raw))
	    (and (eq (car-safe grp-def-raw) 'eval-rxx)
		 (make-rxx-def :form (eval (second grp-def-raw))))
	    (make-rxx-def :form grp-def-raw)))
	  (regexp-here-raw (rx-to-string (rxx-def-form grp-def) 'no-group))
	  (regexp-here (format "\\(%s\\)"
			       (if (and (boundp 'rxx-disable-grps) (member grp-name rxx-disable-grps))
				   ".*"
				 ;; here remove -any- shy groups around the whole thing.
				 (rxx-remove-outer-shy-grps regexp-here-raw)))))
     (rxx-env-bind grp-name (make-rxx-inst
			     :def grp-def
			     :num grp-num
			     :env rxx-env) old-rxx-env)
     regexp-here)))

(defun rxx-process-named-backref (form)
  "Process the (named-backref GRP-NAME) form, when called from `rx-to-string'."
  (declare (special rxx-env))
  (rx-check form)
  (let* ((grp-name (second form))
	 (prev-grp-defs (rxx-env-lookup grp-name rxx-env)))
    (unless prev-grp-defs (error "Group in backref not yet seen: %s" grp-name))
    (unless (= (length prev-grp-defs) 1) (error "Ambiguous backref to group %s" grp-name))
    (rx-backref `(backref ,(rxx-inst-num (first prev-grp-defs))))))

(defun rxx-process-eval-regexp (form &optional rx-parent)
  "Parse and produce code from FORM, which is `(eval-regexp FORM)'."
  (rx-check form)

  ;; FIXME do not add shy grp if not needed
  (concat "\\(?:" (rx-group-if (rxx-make-shy (eval (second form))) rx-parent) "\\)"))

(defun rxx-process-eval-rxx (form &optional rx-parent)
  "Parse and produce code from FORM, which is `(eval-rxx FORM)'."
  (declare (special rxx-num-grps))
  (rxx-process-eval-regexp
   (list 'regexp
	 (rxx-inst-regexp
	  (rxx-instantiate (make-rxx-def :form (eval (second form))))))))

;; The following functions are created by the `elu-flet' call
;; in `rxx-instantiate', rather than being explicitly defined.
;; They reference the original functions from the `rx' package
;; temporarily overridden during the `rxx-instantiate' call.
(declare-function rx-form-orig "rxx.el" t 'fileonly)
(declare-function rx-submatch-orig "rxx.el" t 'fileonly)
(declare-function rx-regexp-orig "rxx.el" t 'fileonly)
(declare-function rx-kleene-orig "rxx.el" t 'fileonly)

(defun rxx-form (form &optional rx-parent)
  "Handle named subexpressions.  Any symbol whose variable value
is an aregexp (e.g.  one defined by `def-rxx-regexp') can be used as a
form.  More specifically: if R is a symbol whose variable value
is an aregexp, the form (R name) creates a named group (see
`rxx-process-named-grp') with the name R matching that aregexp.
The form R by itself is equivalent to (R R) i.e. a named group
named R with the definition of R.  The latter only works if a
group named R is not yet defined in the current environment.  R
can be abbreviated (see `rxx-symbol').

Also, R? is translated to (opt R) for a slight reduction in verbosity.
"
  (declare (special rxx-env))
  (when (and (symbolp form) (elu-ends-with (symbol-name form) "?"))
    (setq form (list 'opt (intern (substring (symbol-name form) 0 -1)))))
  (cond (;; the (my-regexp grp-name) version, e.g. (number numerator)
	 (rxx-find-def (car-safe form))
	 (rxx-process-named-grp (list 'named-grp (second form)
				      (first form))))
	;; the 'my-regexp' version, e.g. just 'number'
	((and (symbolp form) (rxx-find-def form))
	 (rxx-process-named-grp (list 'named-grp form form)))
	(t (rx-form-orig form rx-parent))))

(defun rxx-submatch (form)
  "Keep a count of the number of non-shy subgroups, so that when a named
group is created (see `rxx-process-named-grp'), we will know the regexp group
number to which it corresponds."
  (declare (special rxx-num-grps))
  (incf rxx-num-grps)
  (rx-submatch-orig form))

(defun rxx-regexp (form)
  "Make all subgroups of the regexp shy."
  (rx-regexp-orig (list 'regexp (rxx-make-shy (second form)))))

(defun rxx-process-sep-by (form)
  "Process the sep-by form, which looks like (sep-by separator ....)"
  (let ((separator (second form))
	(seq-elems (cddr form)))
    (rx-form
     (let ((form-with-separators '(seq)) seen-non-optional)
       (dolist (seq-elem seq-elems form-with-separators)

	 (when (and (symbolp seq-elem) (elu-ends-with (symbol-name seq-elem) "?"))
	   (setq seq-elem (list 'opt (intern (substring (symbol-name seq-elem) 0 -1)))))

	 (when (and (consp seq-elem) (memq (first seq-elem) '(0+ zero-or-more 1+ one-or-more * *? + +?)))
	   (setq seq-elem (append (list (first seq-elem) :sep-by separator) (cdr seq-elem))))
	 
	 (let ((is-optional (and (consp seq-elem) (memq (first seq-elem) '(opt optional zero-or-one ? ??)))))
	   (setq form-with-separators
		 (append form-with-separators
			 (if is-optional
			     (list
			      (if (not seen-non-optional)
				  (append seq-elem (when (> (length form) 3) (list separator)))
				  (append (list (first seq-elem) separator) (cdr seq-elem))))
			   (if (not seen-non-optional) (list seq-elem)
			     (list separator seq-elem)))))
	   (unless is-optional (setq seen-non-optional t))
	   form-with-separators))))))

(put 'sep-by lisp-indent-function 1)

(defconst rxx-never-match
  (if (featurep 'xemacs) "[^\000-\127]"
    (rx (not (any ascii nonascii))))
  "A regexp that never matches anything.
Used for bottoming out bounded recursion (see `rxx-process-recurse').")

(defun rxx-process-recurse (form)
  "Process recurse"
  (declare (special rxx-recurs-depth))
  (if (or (not (boundp 'rxx-recurs-depth))
	  (< rxx-recurs-depth 1))
      rxx-never-match
    (let ((rxx-recurs-depth (1- rxx-recurs-depth)))
      (rx-group-if (rxx-remove-unneeded-shy-grps (rx-to-string (second form) 'no-group)) '*))))

(defun rxx-or (form)
  "When called via `rxx-instantiate', modify `rx-or' behavior
as described below.

For bounded recursion, remove any OR clauses consisting of
`recurs' forms for which recursion has bottomed out. (See
`rxx-parse-recurs' and `defrxxrecurse').  Formally, these `recurs'
forms represent never-matching expressions when they bottom out;
but simply removing them leads to a more compact result.

Also, keep track of the current path through the OR clauses.
This lets us disable particular OR clauses, for the purpose of
splitting up a large regexp into an OR of more manageable chunks.

Also, force each clause of the OR, as well as the whole expression,
to be wrapped in shy groups; this can prevent unexpected behaviors.
"
  (declare (special rxx-or-num) (special rxx-or-branch)
	   (special rxx-or-child))
  (if (null (cdr form)) rxx-never-match
    (let (clause-results)
      (elu-do-seq (clause i (cdr form))
	  (let ((rxx-or-branch (cons rxx-or-num rxx-or-branch))
		(rxx-or-child (cons i rxx-or-child)))
	    (push (rx-group-if (rx-form clause '|) '*) clause-results))))))

(defvar rx-greedy-flag)

(defun rxx-kleene (form)
  "When processing repeat constructs such as `zero-or-more' and `one-or-more',
turn each named group GRP inside the repeat into a corresponding named group GRP-LIST
whose parser returns the list of parsed individual matches.   So, a construct
such as (1+ num) containing the named group `num' that parses into a number
will have a named group `num-list' that parses into the list of numbered matched
by the repeat.   Note that this is in contrast to `match-string' which returns
only the last copy of the repeated expression.

Also, extend the syntax to allow specifying a separator regexp, e.g.
\(zero-or-more :sep-by REGEXP-FORM ...).  Then the copies of the repeat
are required to be separated by strings matching REGEXP-FORM.
The parsers that we construct for each named group return parsed matches
of the repeat contents only, not of the separators.
So, for the construct (1+ :sep-by blanks num), the parser for `num-list' would
return the list of parsed numbers, omitting the blanks.   See also
`rxx-process-sep-by'.
"
  (declare (special rxx-env rxx-num-grps))
  (if (memq (first form) '(optional opt zero-or-one ? ??))
      (rx-kleene-orig form)
    (let* (return-value
	   (wrap-grp-num (incf rxx-num-grps))
	   (rxx-num-grps rxx-num-grps)
	   (parent-rxx-env (elu-when-bound rxx-env))
	   (rxx-env (rxx-new-env parent-rxx-env))
	   (sep-by-form (when (eq (car-safe (cdr-safe form)) :sep-by) (third form)))
	   (have-sep-by (not (null sep-by-form)))
	   (body (cons 'seq (nthcdr (if sep-by-form 3 1) form)))
	   (form-sans-sep-by (list (car form) body))
	   (body-regexp (rx-to-string body))
	   (body-repeat-regexp
	    ;; remove sep-by if present
	    ;; call the original with the body as (regexp ,body-regexp).
	    ;; probably also later, factor out the common parts so that advice to
	    ;; rx-kleene, rx-repeat etc can call this common code.
	    (let ((form (delq
			 nil
			 (list
			  (if sep-by-form
			      (ecase (first form) ((1+ one-or-more 0+ zero-or-more) '0+) ((+ *) '*) ((+? *?) '*?))
			    (first form))
			  sep-by-form
			  (list 'regexp body-regexp)))))
	      (setq return-value (rx-kleene-orig form))))
	   (greedy-p (or (memq (first form) '(* + ?\s))
			 (and rx-greedy-flag (memq (first form) '(zero-or-more 0+ one-or-more 1+ >= repeat **))))))
      
      (when sep-by-form
	(setq
	 return-value
	 (rx-to-string
	   (ecase (first form)
	     ((1+ one-or-more + +?)
	      `(seq (regexp ,body-regexp) (regexp ,body-repeat-regexp)))
	     ((0+ zero-or-more * *?)
	      `(,(first form) (regexp ,body-regexp) (regexp ,body-repeat-regexp)))))))

      (setq return-value (format "\\(%s\\)" (rxx-make-shy return-value)))
      (progn
	(do-rxx-env grp-name rxx-insts rxx-env
	  (let ((new-parser
		 `(lambda (match-str)
		    (let ((rxx-cur-namespace (quote ,(elu-when-bound rxx-cur-namespace))))
		      (let ((repeat-form '(seq)) repeat-grp-names parse-result )
			(while (not parse-result)
			  (when (and repeat-grp-names ,have-sep-by)
			    (elu-push-end (quote ,sep-by-form) repeat-form))
			  (let ((new-grp-name (make-symbol "new-grp")))
			    (elu-push-end new-grp-name repeat-grp-names)
			    (elu-push-end (list 'named-grp new-grp-name
						(quote (seq ,@(cdr form-sans-sep-by)))) repeat-form)
			    (save-match-data
			      (setq parse-result
				    (rxx-parse
				     (rxx-instantiate
				      (make-rxx-def :form repeat-form
						    :parser
						    ,`(lambda (match-str)
							(mapcar (lambda (repeat-grp-name)
								  (rxx-match-val (list repeat-grp-name (quote ,grp-name))))
								(elu-when-bound repeat-grp-names)))
						    :namespace (quote ,(elu-when-bound rxx-cur-namespace))))
				     match-str
				     (not 'partial-ok) 'error-ok)))))
		      parse-result)))))
	    (rxx-env-bind (intern (concat (symbol-name grp-name) "-list"))
			  (make-rxx-inst
			   :def (make-rxx-def :parser new-parser) :env (rxx-new-env) :num wrap-grp-num)
			  parent-rxx-env))))
      return-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Defining aregexps
;;
;; User-callable functions and macros for defining aregexps.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rxx-instantiate (rxx-def)
  "Construct a regexp from its readable representation as a lisp FORM, using the syntax of `rx-to-string' with some
extensions.  The extensions, taken together, allow specifying simple grammars
in a modular fashion using regular expressions.

For detailed description, see `rxx'.

*** FIXME handle recursion depth here: keep track of definitions already instantiated,
(can base this on eq.)
if this one already was then do what we did before for recursion.
then don't need the special recurse form."
(declare (special rxx-defs-instantiated) (special rxx-recurs-depth))
;(message "rxx-instantiate rxx-def=%s rxx-defs-instantiated=%s rxx-recurs-depth=%s"
;	 rxx-def (elu-when-bound rxx-defs-instantiated) (elu-when-bound rxx-recurs-depth 0))
  (elu-flet ((rx-form rxx-form)
	     (rx-submatch rxx-submatch)
	     (rx-regexp rxx-regexp)
	     (rx-kleene rxx-kleene))
	     
    (let* ((max-lisp-eval-depth (max max-lisp-eval-depth rxx-max-lisp-eval-depth))
	   (max-specpdl-size (max max-specpdl-size rxx-max-specpdl-size))
	   (rxx-cur-namespace (rxx-def-namespace rxx-def))
	   (rxx-env (rxx-new-env))
	   (rxx-num-grps 0)
	   rxx-or-branch
	   rxx-or-child
	   (rxx-or-num 0)
	   ;; extend the syntax understood by `rx-to-string' with named groups and backrefs
	   (rx-constituents (append '((named-grp . (rxx-process-named-grp 1 nil))
				      (eval-regexp . (rxx-process-eval-regexp 1 1))
				      (eval-rxx . (rxx-process-eval-rxx 1 1))
				      (shy-grp . seq)
				      (& . seq)
				      (blanks . "\\(?:[[:blank:]]+\\)")
				      (digits . "\\(?:[[:digit:]]+\\)")
				      (space . "\\s-")
				      (sep-by . (rxx-process-sep-by 1 nil))
				      (recurse . (rxx-process-recurse 1 nil))
				      (named-group . named-grp) (shy-group . shy-grp)
				      (named-backref . (rxx-process-named-backref 1 1)))
				    rx-constituents))
	   (rx-constituents (cons '(or rx-or 0 nil) rx-constituents))
	   
	   ;; also allow named-group or ngrp or other names
	   ;; var: regexp - the string regexp for the form.
	   (regexp
	    ;; whenever the rx-to-string call below encounters a (named-grp ) construct
	    ;; in the form, it calls back to rxx-process-named-grp, which will
	    ;; Add a mapping from the group's name to rxx-grp structure
	    ;; to rxx-name2grp.
	    (rxx-remove-unneeded-shy-grps
	     (rxx-replace-posix (rx-to-string (rxx-def-form rxx-def) 'no-group)))))
      (assert (= rxx-num-grps (regexp-opt-depth regexp)))
      (rxx-check-regexp-valid regexp)
      (make-rxx-inst :def rxx-def 
		     :env rxx-env
		     :regexp regexp))))

(defmacro rxxlet* (bindings &rest forms)
  (list 'let* (mapcar (lambda (binding) (list (first binding)
					      (list 'rxx (second binding) (third binding) (symbol-name (first binding)))))
		      bindings)
	(or (car-safe forms) (and bindings (car-safe (car-safe (last bindings)))))))

(defun rxx-add-font-lock-keywords ()
  (when (featurep 'font-lock)
    (put 'def-rxx-namespace 'doc-string-elt 3)
    (put 'defrxx 'doc-string-elt 4)
    (put 'defrxxrecurse 'doc-string-elt 5)
    (when (fboundp 'font-lock-add-keywords)
      (font-lock-add-keywords
       nil
       `((,(rx (seq bow (group (or "def-rxx-namespace" "def-rxx-regexp" "def-rxx-regexps"))
		    (1+ blank) (group (1+ (not blank))))) .
		    ((1 font-lock-keyword-face) (2 font-lock-variable-name-face)))
	 (,(rx (seq bow (group "defrxxrecurse") (1+ blank) (1+ digit) (1+ blank) (group (1+ (not blank))))) .
	  ((1 font-lock-keyword-face) (2 font-lock-variable-name-face))))))))

(add-hook 'emacs-lisp-mode-hook 'rxx-add-font-lock-keywords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Searching and parsing
;;
;; User-callable functions for searching and parsing aregexps.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* rxx-parse (aregexp s &optional partial-match-ok error-ok)
  "Match the string against the given extended regexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;
  (check-type  aregexp (or rxx-def rxx-inst))
  (when (rxx-def-p aregexp) (setq aregexp (rxx-instantiate aregexp)))
  (save-match-data
    (let* ((rxx-inst (or (get-rxx-inst aregexp) (error "Need annotated regexp returned by `rxx'; got `%s'" aregexp)))
	   (error-msg (format "Error parsing \`%s\' as \`%s\'" s
			      (or (rxx-inst-descr rxx-inst) (rxx-inst-form rxx-inst)))))
      
      ;; so, what you need here is just:
      ;;   -- if full match, then parse and be done.
      ;;   -- if no match, go to the next.
      ;; if partial match ok, longer partial match is generally better.
      ;; so, possibly, match each string.  (or only when rxx-longest-match-p is true?)
      
      (if (not (funcall (if (elu-when-bound rxx-posix) 'posix-string-match 'string-match) (rxx-inst-regexp aregexp) s))
	  (unless error-ok (signal 'rxx-parse-error (list (format "%s: No match" error-msg))))
	(let (no-parse)
	  (unless partial-match-ok
	    (unless (= (match-beginning 0) 0)
	      (if error-ok (setq no-parse t) (signal 'rxx-parse-error (list (format "%s: match starts at %d" error-msg (match-beginning 0))))))
	    (unless (= (match-end 0) (length s))
	      (if error-ok (setq no-parse t) (signal 'rxx-parse-error (list (format "%s: match ends at %d" error-msg (match-end 0)))))))
	  (unless no-parse
	    (let* ((rxx-env (rxx-inst-env rxx-inst))
		   (rxx-object s))
	      (rxx-call-parser rxx-inst (match-string 0 s)))))))))

(defun* rxx-search-fwd (aregexp &optional bound noerror (partial-match-ok t))
  "Match the current buffer against the given extended regexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;
  (check-type  aregexp (or rxx-def rxx-inst))
  (when (rxx-def-p aregexp) (setq aregexp (rxx-instantiate aregexp)))
      (let* ((old-point (point))
	     (rxx-inst (or (get-rxx-inst aregexp) (error "Need annotated regexp returned by `rxx'; got `%s'" aregexp)))
	     (error-msg (format "Error parsing \`%s\' as %s"
				(if (and bound (>= bound old-point) (< (- bound old-point) 100))
				    (buffer-substring old-point bound)
				  "buffer text") (rxx-inst-form rxx-inst))))
	(if (not (re-search-forward (rxx-inst-regexp aregexp) bound 'noerror))
	    (unless noerror (error "%s" error-msg))
	  (unless (or noerror partial-match-ok)
	    (unless (= (match-beginning 0) old-point) (error "%s: match starts at %d" error-msg (match-beginning 0)))
	    (unless (= (match-end 0) bound) (error "%s: match ends at %d" error-msg (match-end 0))))
	  (let* ((rxx-env (rxx-inst-env rxx-inst))
		 rxx-object)
	    (rxx-call-parser rxx-inst (match-string 0))))))

(defun* rxx-search-bwd (aregexp &optional bound noerror (partial-match-ok t))
  "Match the current buffer against the given aregexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;   - and with posix searches
  ;;
      (let* ((old-point (point))
	    (rxx-inst (or (get-rxx-inst aregexp) (error "Need annotated regexp returned by `rxx'; got `%s'" aregexp)))
	    (error-msg (format "Error parsing \`%s\' as %s"
			       (if (and bound (>= bound old-point) (< (- bound old-point) 100))
				   (buffer-substring old-point bound)
				 "buffer text") aregexp)))
	(if (not (re-search-backward (rxx-inst-regexp aregexp) bound 'noerror))
	    (unless noerror (error "%s" error-msg))
	  (unless (or noerror partial-match-ok)
	    (unless (= (match-beginning 0) old-point) (error "%s: match starts at %d" error-msg (match-beginning 0)))
	    (unless (= (match-end 0) bound) (error "%s: match ends at %d" error-msg (match-end 0))))
	  (let* ((rxx-env (rxx-inst-env rxx-inst))
		 rxx-object)
	    (rxx-call-parser rxx-inst (match-string 0))))))

(defmacro rxx-do-search-fwd (namespace symbol var &rest forms)
  "Searches forward from point for matches to rxx AREGEXP, and evalutes FORMS
at each match after parsing the match into variable VAR.  If VAR is nil, 
creates a dummy var."
  (declare (indent 2))
  `(let (,(or var (make-symbol "dummy-var")))
     (while
	 (setq ,var
	       (rxx-search-fwd (rxx-find-def (quote ,symbol) (quote ,namespace)) (not 'boundary) 'no-error))
       ,@forms)))

(defmacro rxx-parse-string (namespace symbol string &optional partial-match-ok error-ok)
  `(rxx-parse (rxx-find-def (quote ,symbol) (quote ,namespace))
	      ,string
	      ,partial-match-ok ,error-ok))


(defmacro rxx-parse-string-func (namespace symbol string &optional partial-match-ok error-ok)
  `(rxx-parse (rxx-find-def ,symbol ,namespace)
	      ,string
	      ,partial-match-ok ,error-ok))

(defmacro rxx-parse-fwd (namespace symbol &optional bound partial-match-ok)
  "Parse from point until BOUND looking for a match to regexp defined by SYMBOL
in namespace NAMESPACE."

  `(let ((rxx-marker (point-marker)))  ;; FIXME release marker when done
     (rxx-parse-string-func (quote ,namespace) (quote ,symbol)
		       (buffer-substring-no-properties (point) (or ,bound (point-max)))
		       ,partial-match-ok (not 'error-ok))))

(defun rxx-parse-bwd (aregexp &optional bound partial-match-ok)
  "Match the current buffer against the given extended regexp, and return
the parsed result in case of match, or nil in case of mismatch."
  ;; add options to:
  ;;   - work with re-search-forward and re-search-bwd.
  ;;
  (elu-save match-data excursion
    (let ((old-point (point))
	  (rxx-inst (or (get-rxx-inst aregexp)
			(error "Need annotated regexp returned by `rxx'; got `%s'" aregexp))))
      (if (and (re-search-backward (rxx-inst regexp aregexp) bound 'noerror)
	       (or partial-match-ok
		   (and (= (match-beginning 0) bound)
			(= (match-end 0) old-point))))
	  (let* ((rxx-env (rxx-inst-env rxx-inst))
		 rxx-object)
	    (rxx-call-parser rxx-inst (match-string 0)))
	(error "Error parsing \`%s\' as %s" (if (and bound (>= bound old-point) (< (- bound old-point) 100))
						(buffer-substring old-point bound)
					      "buffer text")
	       (or (rxx-inst-descr rxx-inst) (rxx-inst-form rxx-inst)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct rxx-parsed-obj
  "An object that was parsed from a text string.

Fields:

   LOC - the location where the object was parsed
   TEXT - the full text of the original object
"
  loc text)

(defun rxx-reset ()
  "Kill local rxx vars"
  (interactive)
  (makunbound 'rxx-cur-namespace)
  (when nil
    (let (vars-killed)
      (dolist (var (delq nil (mapcar 'car-safe (buffer-local-variables))) vars-killed)
	(when (and (symbolp var)
		   (elu-ends-with (symbol-name var) "-rxx-def-local"))
	  (kill-local-variable var)
	  (push var vars-killed)))
      (message "%s rxx local vars killed: %s" (length vars-killed) vars-killed)))
  )

(provide 'rxx)

;;; rxx.el ends here

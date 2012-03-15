;;; org-fastmatch.el --- Speed up the matching of tags and properties
;; Copyright (C) 2008-2012 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: outlines, search
;; Homepage: http://orgmode.org
;; Version: 0.04
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

;; This module is a drop-in replacement for `org-make-tags-matcher' and
;; `org-scan-tags'.  Because these are so central to Org mode, this module
;; is implemented as advice that can be easily turned on/off.  However,
;; the intent is that nothing should be altered in the operation of these
;; routines except for speed.

(eval-when-compile
  (require 'cl))
(require 'org)
(require 'org-agenda)

(defstruct org-fastmatch-term
  "Parsed version of one matcher term.
We gather separately constraints on various parts of an entry.
"
  required-heritable-tags  ; can be regexps

  ; also keep status here.  or, say the above is a vector,
  ;; org-scan-tags can keep its own state -- probably better parallelism.

  ;; the regexp for this term
  regexp
  )



;; so, about the todo matcher:
;; the todo keywords are definitely all known.
;; so, we can just create a regexp.
;; if restricted to todo-keywords (and we also have this as an arg to org-scan-tags),
;; just require one of the keywords (or possibly non-done ones) at the appropriate point.
;; so then, we don't need to factor-multiply things out.
;; we can just process each term.
;; for each term, we get an expression, and then we or them.
;; if for any term the expression is "any", can short-circuit things and just say, any.
;; can use throw/catch for this.

;; so, for one term:
;;   - for each part of the headline, we can accumulate the constraints separately
;;   - for each inherited tag mentioned, we add an OR for just that tag

;;       --> whenever we exit a level where a prop or a tag was set, need to remove that.
;;           so, if we set an inherited prop or tag, then need to:
;;                -- find the corresponding exit level, and when searching forward,
;;                   give that as the limit.  keep a stack of these.

;;       --> be careful if a skip-function was specified.

;;   - check what happens for ALLTAGS
;;   - for each inherited property mentioned, including CATEGORY, we add an OR for just that prop.
;;   - 


(defun org-fastmatch-make-regexp-for-date-range (float-time-min float-time-max)
  "Construct a regexp that will definitely match every date string between FLOAT-TIME-MIN and FLOAT-TIME-MAX.
"
  (regexp-opt
   (let ((cur-time float-time-min)
	 (fmt (substring (org-time-stamp-format) 1 9))
	 day-strings)
     (while (< cur-time float-time-max)
       (push (format-time-string fmt (seconds-to-time cur-time))
	     day-strings)
       (incf cur-time 86400))
     day-strings) 'paren))

(defun org-ilya-recent ()
  "Show recent items"
  (interactive)
  (org-occur
   (org-fastmatch-make-regexp-for-date-range
    (org-matcher-time "<-4w>")
    (org-matcher-time "<+1w>"))))

(defun org-fastmatch-negate-if (should-negate val)
  "Negate a value or not, based on a flag"
  (if should-negate (not val) val))

(defun org-fastmatch-canonicalize-op (op)
  "Canonicalize a string op"
  (or (assoc-default
       op
       '(("=>" . ">=")
	 ("=<" . "<=")
	 ("=" . "==")
	 ("<>" . "!=")))
      op))

(defun org-fastmatch-negate-op (op)
  "Negate a canonicalized op."
  (assoc-default '((">=" . "<")
		   ("<=" . ">")
		   (">" . "<=")
		   ("<" . ">=")
		   ("==" . "!=")
		   ("!=" . "=="))))

(defun org-fastmatch-make-tags-matcher-filter (match only-not-done-todos start-level)
  "For a given TAGS/TODO matcher defined by MATCH,
construct a regexp that will match within the next entry at which `org-fastmatch-scan-tags'
must stop.  Any entry before the match, can be safely skipped as if it was not present
in the orgfile, without affecting the match results.


"
  ;; Parse the string and create a lisp form
  (let ((match0 match)
	(re (org-re "^&?\\([-+:]\\)?\\({[^}]+}\\|LEVEL\\([<=>]\\{1,2\\}\\)\\([0-9]+\\)\\|\\(\\(?:[[:alnum:]_]+\\(?:\\\\-\\)*\\)+\\)\\([<>=]\\{1,2\\}\\)\\({[^}]+}\\|\"[^\"]*\"\\|-?[.0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)\\|[[:alnum:]_@#%]+\\)"))
	minus tag mm
	tagsmatch todomatch tagsmatcher todomatcher kwd matcher
	orterms term orlist re-p str-p level-p level-op time-p
	prop-p pn pv po gv rest
	tags-min-regexp
	(all-todos (if only-not-done-todos
		       org-not-done-keywords
		     (cons "" org-todo-keywords-1)))
	
	todo-matcher-ok-todos
	)
    ;; Split match string into tags/properties matcher and todo matcher
    (if (string-match "/+" match)
	;; match contains also a todo-matching request
	(progn
	  (setq tagsmatch (substring match 0 (match-beginning 0))
		todomatch (substring match (match-end 0)))
	  (if (string-match "^!" todomatch)
	      (setq todo-only t todomatch (substring todomatch 1)))
	  (if (string-match "^\\s-*$" todomatch)
	      (setq todomatch nil)))
      ;; only matching tags
      (setq tagsmatch match todomatch nil))

    ;; Make a list of todos acceptable to the todo matcher
    (if (or (not todomatch) (not (string-match "\\S-" todomatch)))
	(setq todo-matcher-ok-todos all-todos)
      
      (setq orterms (org-split-string todomatch "|"))
      (while (setq term (pop orterms))
	;; Compute the list of todos accepted by this or-term
	(let ((or-term-ok-todos (copy-sequence all-todos))) 
	  (while (string-match re term)
	    (setq minus (and (match-end 1)
			     (equal (match-string 1 term) "-"))
		  kwd (match-string 2 term)
		  re-p (equal (string-to-char kwd) ?{)
		  term (substring term (match-end 0)))
	    ;; Remove any TODOs not accepted by this and-term
	    (setq or-term-ok-todos
		  (org-remove-if-not
		     (lambda (todo)
		       (let ((matches-p
			      (if re-p
				  (string-match  (substring kwd 1 -1) todo)
				(equal todo kwd))))
			 (if minus (not matches-p) matches-p)))
		     or-term-ok-todos)))
	  (dolist (todo or-term-ok-todos)
	    (unless (member todo todo-matcher-ok-todos)
	      (push todo org-matcher-ok-todos))))

	;;
	;; if org-agenda-skip-function-global or org-agenda-skip-function is
	;; (org-agenda-skip-entry-if todo ) or nottodo, further restrict org-matcher-ok-todos.
	;;

    ;; Make the tags matcher

	  ;; so, for the tags:
	  ;;   - if an inherited tag is mentioned in any term, that tag by itself
	  ;;     becomes an or of the regexp.  (and is removed from the regexp for that term)

	  ;;   - for each and term we make one regexp, and make an or of them, plus any standalone tags as mentioned above.
	  ;;      - so, for an or term:
	  ;;         - we keep and accumulate lists of constraints for the various parts of an entry:
	  ;;             level, priority (if priority not inherited), todo (limited by todo matcher), tags (except inherited ones),
	  ;;             scheduled, deadline, timestamp, timestamp_ia, closed.
	  ;;             properties.
	  ;;           within each list we try to group constraints into one regexp, e.g.
	  ;;           priority range, level range, date range.

	  ;;         - then, we combine all these into one regexp
	  ;;             - if things may be in any order, use a repeat, but with a minimum number of repeats.

	  ;;  -- require archive tag to be minus, based on options.
	  ;;     only useful if all possible tags are known.

	  ;;  -- if clocksum, forget it, no regexp for that
	  ;;  -- BLOCKED, check

	  ;; ** if ALLTAGS is given, then what?
	  ;;     -- if we know all possible inherited tags, make sure to stop for each one individually.
	  ;;        now, wait.  how is this property returned?
	  ;;
	  ;;     so, if ALLTAGS is given, and we don't know the full list of tags, then just stop
	  ;;     at any tag.  otherwise, how do you trust scanner tags?

	  ;;    -->

	  ;; ** wait.  what if this is used for mapping.
	  ;;    so, if we're just searching for a condition, then skipping tags is ok.
	  ;;    but if we're mapping a user-defined function --

	  ;;; so, how about: if we know all tags, or we know there are no inherited tags,
	  ;;    then do the above.
	  
	  ;;  otherwise, can just say, stop at any tag, period.
	  ;;  this still lets us skip non-tagged entries;  and, if the user specifies the full set of tags,
	  ;;; or limits inheritance to known ones, then we can stop at just those.


	  ;;
	  ;; so, advise org-map-entries to set a var, so that if we're doing org-scan-tags for that,
	  ;; we either try to make sure the scanner-tags are accurate for all tags,
	  ;; or else don't use org-scanner-tags.
	  ;
	  ;; but document a way for functions to tell us the tags they care about.
	  ;; but in the simplest, initial, case, can just stop either at all tags or at
	  ;; all inherited tags if these are known.

	  ;; (but no, let's only do that in the above case).

	  ;; what about the skipper function, can it ask for scanner-tags?
	  ;;

	  ;;   --> you can wrap the user's function in your own,
	  ;;    so that if it calls to get properties or tags, you will know.

	  ;; for non-inherited properties, we can require the property to be defined (unless the value nil is ok)
	  ;; if the restriction is to a given value or given regexp, we can include those.
	  ;; otherwise, only if the _all is given for the property.  (and even then, be careful as this can change
	  ;; during the search.  can say, if you want speed, make an re yourself.)
	  
	  ;; for tags, we only include non-inherited tags.
	  ;; if a tag is positively selected, including by re, we include it.
	  ;; any other tag constraints -- if we have the full list of tags, can make a regexp, otherwise no.

	  ;; for testing, can double-check that any entry this would have skipped,
	  ;; we don't care about.

	  ;;   - don't forget, in org-scan-tags, to unset inherited tags.
	  ;;   - we can make a global list of all properties we care about, and only get those.
	  ;;


	  ;; so, if a timestamp is needed and may be in the entry,
	  ;; then we can't require it to be after.
	  ;;
	  ;;
	  ;; but, if we drop the tags requirement, then all is well.

	  ;; we _could_ make the tags constraint just one of several here;
	  ;; since the tags are at the end of the line, they're unlikely to be matched by others anyway.

	  ;; so, if there is one date and some tags, can make two cases, one with date before one with after.

	  ;; so ok, so, it's not too bad -- the tags can be at one of few positions relative to this.
	  ;; so, can make an OR for each of these positions.
	  

	  ;; the code needs to be clear or it won't be put into the core.


	  ;; so, for each possible position of the dates vs the tags.
	  ;; say, no dates on the headline.
	  ;; we can match, ( (zero-or-more non-headline lines, non-greedy) ( non-headline line with date ) .
	  ;; ah, but there might be more than one date on a line.

	  ;; so, for speed can have an option where we assume no dates on the headline, and/or no more than
	  ;; one date per line.
	  ;; but without it -- we should just choose the strongest constraint and stick with it.
	  ;; e.g. a strong date constraint, together with a todo constraint.

	  ;;    --> but, how can we have a date constraint at all, without matching a date way downstream?
	  ;; let's say we _just_ want to match a date.
	  ;; so, the question is only about how we make an AND of a date and a headline constraint.
	  ;; for one date, we can say that it is either on the headline,
	  ;; or, we match the headline constraint followed by zero-or-more non-heading or empty lines,
	  ;; followed by the line with the constraint.
	  ;; so, basically, in a regexp for the term, besides constraints on the headline,
	  ;; we can add _one_ constraint on a property or a date.

	  ;;    --> on the other hand, should allow ITEMBODY and ITEMHEADLINE matches for a regexp?

	  ;; for an inherited prop, if it is undef and the value is required, the point is we shouldn't stop at any place
	  ;; until we get a value... except the places needed to keep the tags.

	  ;; another option is, when searching forward, to search twice.
	  ;; you can ask, when is the next instance of this element?


	
	(let (term-infos all-buffer-tags)
	  (flet ((get-all-buffer-tags
		  ()
		  (or all-buffer-tags
		      (setq all-buffer-tags
			    (or org-tag-alist (org-get-buffer-tags))))))
	    

	  (if (or (not tagsmatch) (not (string-match "\\S-" tagsmatch)))
	      (set min-regexp-or-terms
		   (when (or only-not-done-todos
			     (not (equal todo-matcher-ok-todos all-todos)))
		     ;; prepend levell constraint
		     ;; or pretend there is one term without any constraints.
		     (regexp-opt org-matcher-ok-todos 'words)))
	    )
	  
	(setq tagsmatcher t)
      (setq orterms (org-split-string tagsmatch "|") orlist nil)
      (while (setq term (pop orterms))
	(while (and (equal (substring term -1) "\\") orterms)
	  (setq term (concat term "|" (pop orterms)))) ; repair bad split
	
	;; For each part of an entry, gather constraints from this or-term
	;; on that part of the entry.
	;; Then, we'll create a regexp for each entry part matching on these constraints.
	(let ((level-min (or start-level 0))
	      (level-max 10000)
	      ok-todos (copy-sequence todo-matcher-ok-todos)
	      ok-priorities   ;; if priority is heritable, don't make a regexp for it
	      ok-tags (copy-sequence all-buffer-tags)
	      timestamp-min timetamp-max
	      timestamp_ia-min timestamp_ia-max
	      deadline-min deadline-max
	      scheduled-min scheduled-max
	      closed-min closed-max
	      
		;; be sure to check the minus!
		
		;; a list of property constraints (regexps)
		standard-prop-constraints
		;;
		)
	    )
	  
	  (while (string-match re term)
	    (setq
	     rest (substring term (match-end 0))
	     minus (and (match-end 1)
			(equal (match-string 1 term) "-"))
	     tag (save-match-data (replace-regexp-in-string
				   "\\\\-" "-"
				   (match-string 2 term)))
	     re-p (equal (string-to-char tag) ?{)
	     level-p (match-end 4)
	     prop-p (match-end 5)
	    
	     
	     mm (cond
		 (re-p
		  ;; so, we can build the list of all tags that are ok here.
		     ;; now, how do we make a stopping criterion for inherited tags?
		     ;; so, each term can require a set of tags or exclude a set of tags.
		     ;; if it requires a set of tags, we can add them all to the regexp,
		     ;; except for the inheritable ones.
		     ;; if it excludes a set of tags:
		     ;;   - we can say, "have no tags here or all the others".
		     ;;   - so, what will the tag regexp look like?  it will say,
		     ;;     repeat: reguired tag, space.  now that space,
		     ;;     can be all tags, but if some are excluded, then exclude them.
		     ;; so, wait.

		     ;; we can again start with all tags,
		     ;; and 

		     ;; if a tag is excluded, we're already skipping the subtree.

		     ;; so, what if we say, the tags regexp is just, ok-tag, repeat, ok-tag;
		     ;; and, at least the number of non-inherited required tags.

		     ;; so then, we just need:

		  (setq ok-tags
			(org-remove-if-not
			 (lambda (a-tag)
			   (org-fastmatch-negate-if
			    minus
			    (string-match (substring tag 1 -1) a-tag)))
			 ok-tags)))
		  
		 (level-p
		  (let ((level-op (org-fastmatch-canonicalize-op (match-string 3 term)))
			(level-val (string-to-number
				    (match-string 4 term))))
		    (if (not minus)
			(case (intern level-op)
			  ('> (setq level-min (max level-min (1+ level-val))))
			  ('>= (setq level-min (max-level-min level-val)))
			  ('< (setq level-max (min level-max (1- level-val))))
			  ('<= (setq level-max (min level-max level-val)))
			  ('== (setq level-min level-val level-max level-val)))
		      (case (intern level-op)
			('> (setq level-max (min level-max level-val)))
			('>= (setq level-max (min level-max (1- level))))
			('< (setq level-min (max level-min level-val)))
			('<= (setq level-min (max level-min (1+ level-val))))
			('!= (setq level-min level-val level-max level-val)))
		      )))
			
		    (prop-p

		     ;; check if it's one of the special props,
		     ;; esp alltags

		     ;; check priority

		     ;; possibly, check for props with _ALL values.
		     ;;  though there the user can probably just write a regexp themselves.
		     
		     (setq pn (match-string 5 term)
			   po (match-string 6 term)
			   pv (match-string 7 term)
			   re-p (equal (string-to-char pv) ?{)
			   str-p (equal (string-to-char pv) ?\")
			   time-p (save-match-data
				    (string-match "^\"[[<].*[]>]\"$" pv))
			   pv (if (or re-p str-p) (substring pv 1 -1) pv))
		     (if time-p (setq pv (org-matcher-time pv)))
		     (setq po (org-op-to-function po (if time-p 'time str-p)))

		     (cond
		      ((equal pn "TODO")
		       ;; restrict list of ok todos for this term.
		       (setq ok-todos
			     (remove-if-not
			      (lambda (a-todo)
				(org-fastmatch-negate-if
				 minus
				 (org-fastmatch-negate-if
				  (eq po 'org<>)
				  (save-match-data
				    (string-match pv a-todo))))))))
		      
		      ;; check about alltags!!
		      ;; just make a note of whether this is used at all?
		      ;; or can just grep the string for it.
							 
		       )
		     
		     (cond
		      ((equal pn "CATEGORY")
		       (setq gv '(get-text-property (point) 'org-category)))
		      ((equal pn "TODO")
		       (setq gv 'todo))
		      (t
		       (setq gv `(org-cached-entry-get nil ,pn))))
		     (if re-p
			 (if (eq po 'org<>)
			     `(not (string-match ,pv (or ,gv "")))
			   `(string-match ,pv (or ,gv "")))
		       (if str-p
			   `(,po (or ,gv "") ,pv)
			 `(,po (string-to-number (or ,gv ""))
			       ,(string-to-number pv) ))))
		    (t
		     (cons `(member ,tag tags-list)
			   (concat "^\\*+.*:\\(?:" tag "\\):"))
			   
			   )
		     )
		mm (if minus (list 'not mm) mm)
		term rest

		;; Construct a regexp that will NOT be matched by any
		;; entry (its headline + contents) which we must stop at
		;; to determine the truth of this term.
		term-min-re
		(cond
		 (
		 (t '')
		 )
		)
	  (push mm tagsmatcher))
	(push (if (> (length tagsmatcher) 1)
		  (cons 'and tagsmatcher)
		(car tagsmatcher))
	      orlist)
	(setq tagsmatcher nil))
      (setq tagsmatcher (if (> (length orlist) 1) (cons 'or orlist) (car orlist)))
      (setq tagsmatcher
	    (list 'progn '(setq org-cached-props nil) tagsmatcher)))
      
      ;; Make the todo matcher
      (if (or (not todomatch) (not (string-match "\\S-" todomatch)))
	  (setq todomatcher t)
	(setq orterms (org-split-string todomatch "|") orlist nil)
	(while (setq term (pop orterms))
	  (while (string-match re term)
	    (setq minus (and (match-end 1)
			     (equal (match-string 1 term) "-"))
		  kwd (match-string 2 term)
		  re-p (equal (string-to-char kwd) ?{)
		  term (substring term (match-end 0))
		  mm (if re-p
			 `(string-match  ,(substring kwd 1 -1) todo)
		       (list 'equal 'todo kwd))
		  mm (if minus (list 'not mm) mm))
	    (push mm todomatcher))
	  (push (if (> (length todomatcher) 1)
		    (cons 'and todomatcher)
		  (car todomatcher))
		orlist)
	  (setq todomatcher nil))
	(setq todomatcher (if (> (length orlist) 1)
			      (cons 'or orlist) (car orlist))))
      
      ;; Return the string and lisp forms of the matcher
      (setq matcher (if todomatcher
			(list 'and tagsmatcher todomatcher)
		      tagsmatcher))
      (cons match0 matcher)))
  
(defun org-fastmatch-scan-tags (action matcher &optional todo-only start-level)
  "Scan headline tags with inheritance and produce output ACTION.

ACTION can be `sparse-tree' to produce a sparse tree in the current buffer,
or `agenda' to produce an entry list for an agenda view.  It can also be
a Lisp form or a function that should be called at each matched headline, in
this case the return value is a list of all return values from these calls.

MATCHER is a Lisp form to be evaluated, testing if a given set of tags
qualifies a headline for inclusion.  When TODO-ONLY is non-nil,
only lines with a TODO keyword are included in the output.

START-LEVEL can be a string with asterisks, reducing the scope to
headlines matching this string."
  (require 'org-agenda)
  (let* ((re (concat "^"
		     (if start-level
			 ;; Get the correct level to match
			 (concat "\\*\\{" (number-to-string start-level) "\\} ")
		       org-outline-regexp)
		     " *\\(\\<\\("
		     (mapconcat 'regexp-quote org-todo-keywords-1 "\\|")
		     (org-re
		      "\\>\\)\\)? *\\(.*?\\)\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*$")))
	 (props (list 'face 'default
		      'done-face 'org-agenda-done
		      'undone-face 'default
		      'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'org-complex-heading-regexp org-complex-heading-regexp
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name
			       (or (buffer-file-name (buffer-base-buffer))
				   (buffer-name (buffer-base-buffer)))))))
	 (case-fold-search nil)
	 (org-map-continue-from nil)
         lspos tags
	 (tags-alist (list (cons 0 org-file-tags)))
	 (llast 0) rtn rtn1 level category i txt
	 todo marker entry priority)
    (when (not (or (member action '(agenda sparse-tree)) (functionp action)))
      (setq action (list 'lambda nil action)))
    (save-excursion
      (goto-char (point-min))
      (when (eq action 'sparse-tree)
	(org-overview)
	(org-remove-occur-highlights))
      (while (re-search-forward re nil t)
	(setq org-map-continue-from nil)
	(catch :skip
	  (setq todo (if (match-end 1) (org-match-string-no-properties 2))
		tags (if (match-end 4) (org-match-string-no-properties 4)))
	  (goto-char (setq lspos (match-beginning 0)))
	  (setq level (org-reduced-level (funcall outline-level))
		category (org-get-category))
	  (setq i llast llast level)
	  ;; remove tag lists from same and sublevels
	  (while (>= i level)
	    (when (setq entry (assoc i tags-alist))
	      (setq tags-alist (delete entry tags-alist)))
	    (setq i (1- i)))
	  ;; add the next tags
	  (when tags
	    (setq tags (org-split-string tags ":")
		  tags-alist
		  (cons (cons level tags) tags-alist)))
	  ;; compile tags for current headline
	  (setq tags-list
		(if org-use-tag-inheritance
		    (apply 'append (mapcar 'cdr (reverse tags-alist)))
		  tags)
		org-scanner-tags tags-list)
	  (when org-use-tag-inheritance
	    (setcdr (car tags-alist)
		    (mapcar (lambda (x)
			      (setq x (copy-sequence x))
			      (org-add-prop-inherited x))
			    (cdar tags-alist))))
	  (when (and tags org-use-tag-inheritance
		     (or (not (eq t org-use-tag-inheritance))
			 org-tags-exclude-from-inheritance))
	    ;; selective inheritance, remove uninherited ones
	    (setcdr (car tags-alist)
		    (org-remove-uninherited-tags (cdar tags-alist))))
	  (when (and

		 ;; eval matcher only when the todo condition is OK
		 (and (or (not todo-only) (member todo org-not-done-keywords))
		      (let ((case-fold-search t)) (eval matcher)))

		 ;; Call the skipper, but return t if it does not skip,
		 ;; so that the `and' form continues evaluating
		 (progn
		   (unless (eq action 'sparse-tree) (org-agenda-skip))
		   t)

		 ;; Check if timestamps are deselecting this entry
		 (or (not todo-only)
		     (and (member todo org-not-done-keywords)
			  (or (not org-agenda-tags-todo-honor-ignore-options)
			      (not (org-agenda-check-for-timestamp-as-reason-to-ignore-todo-item)))))

		 ;; Extra check for the archive tag
		 ;; FIXME: Does the skipper already do this????
		 (or
		  (not (member org-archive-tag tags-list))
		  ;; we have an archive tag, should we use this anyway?
		  (or (not org-agenda-skip-archived-trees)
		      (and (eq action 'agenda) org-agenda-archives-mode))))

	    ;; select this headline

	    (cond
	     ((eq action 'sparse-tree)
	      (and org-highlight-sparse-tree-matches
		   (org-get-heading) (match-end 0)
		   (org-highlight-new-match
		    (match-beginning 1) (match-end 1)))
	      (org-show-context 'tags-tree))
	     ((eq action 'agenda)
	      (setq txt (org-agenda-format-item
			 ""
			 (concat
			  (if (eq org-tags-match-list-sublevels 'indented)
			      (make-string (1- level) ?.) "")
			  (org-get-heading))
			 category
			 tags-list)
		    priority (org-get-priority txt))
	      (goto-char lspos)
	      (setq marker (org-agenda-new-marker))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker marker 'org-category category
		'todo-state todo
		'priority priority 'type "tagsmatch")
	      (push txt rtn))
	     ((functionp action)
	      (setq org-map-continue-from nil)
	      (save-excursion
		(setq rtn1 (funcall action))
		(push rtn1 rtn)))
	     (t (error "Invalid action")))

	    ;; if we are to skip sublevels, jump to end of subtree
	    (unless org-tags-match-list-sublevels
	      (org-end-of-subtree t)
	      (backward-char 1))))
	;; Get the correct position from where to continue
	(if org-map-continue-from
	    (goto-char org-map-continue-from)
	  (and (= (point) lspos) (end-of-line 1)))))
    (when (and (eq action 'sparse-tree)
	       (not org-sparse-tree-open-archived-trees))
      (org-hide-archived-subtrees (point-min) (point-max)))
    (nreverse rtn)))

(provide 'org-fastmatch)

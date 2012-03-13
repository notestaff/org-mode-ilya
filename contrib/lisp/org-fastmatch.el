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

(defstruct org-fastmatch-term
  "Parsed version of one matcher term.
We gather separately constraints on various parts of an entry.
"
  level
  todo
  priority
  tags

  scheduled
  deadline

  timestamp
  timestamp_ia
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



(defun org-fastmatch-make-tags-matcher-filter (match)
  "For a given TAGS/TODO matcher defined by MATCH,
construct a regexp that will match only necessary
entries.
"
  ;; Parse the string and create a lisp form
  (let ((match0 match)
	(re (org-re "^&?\\([-+:]\\)?\\({[^}]+}\\|LEVEL\\([<=>]\\{1,2\\}\\)\\([0-9]+\\)\\|\\(\\(?:[[:alnum:]_]+\\(?:\\\\-\\)*\\)+\\)\\([<>=]\\{1,2\\}\\)\\({[^}]+}\\|\"[^\"]*\"\\|-?[.0-9]+\\(?:[eE][-+]?[0-9]+\\)?\\)\\|[[:alnum:]_@#%]+\\)"))
	minus tag mm
	tagsmatch todomatch tagsmatcher todomatcher kwd matcher
	orterms term orlist re-p str-p level-p level-op time-p
	prop-p pn pv po gv rest
	tags-min-regexp
	)
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

    ;; Make the tags matcher
    (if (or (not tagsmatch) (not (string-match "\\S-" tagsmatch)))
	(setq tagsmatcher t)
      (setq orterms (org-split-string tagsmatch "|") orlist nil)
      (while (setq term (pop orterms))
	(while (and (equal (substring term -1) "\\") orterms)
	  (setq term (concat term "|" (pop orterms)))) ; repair bad split
	(while (string-match re term)
	  (setq rest (substring term (match-end 0))
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
		     (cons
		      `(org-match-any-p ,(substring tag 1 -1) tags-list)
		      ;; if overall list of tags is known, then can get the set of tags
		      ;; matching this, and then compute the regexp for the relevant tags.
		      ;; if a tag isn't inherited, and this term requires that tag then search for the tag.
		      ;; more generally, if list of possible tags is known, can make a regexp for the tags,
		      ;; and then only also stop separately at inherited tags or inherited props.
		      (if minus ''
			  (concat "^\\*+.*:\\(?:" (substring tag 1 -1) "\\):"
		      ))))
		    (level-p
		     (setq level-op (org-op-to-function (match-string 3 term)))
		     (cons `(,level-op level ,(string-to-number
					       (match-string 4 term)))
			   ;; the right number of stars, depending on the operator
			   ;; and on the odd-levels-only setting.
			   ''
			   ))
		    (prop-p
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

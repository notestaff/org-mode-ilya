;;; org-ilya.el --- Ilya Shlyakhter's miscellaneous extensions to org
;; Copyright (C) 2008-2012 Free Software Foundation, Inc.
;;
;; Author: Ilya Shlyakhter <ilya_shl at alum dot mit dot edu>
;; Keywords: outlines
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


(defun org-flag-drawer-fully (flag)
  "Hide or show a drawer, _including_ the drawer name line.
Useful mainly for hiding the property drawer when the only property there is
the entry creation date.
"
  (save-excursion
    (beginning-of-line 1)
    (when (looking-at "^[ \t]*:[a-zA-Z][a-zA-Z0-9]*:")
      (let ((b (1- (match-beginning 0))))
	(if (re-search-forward
	     "^[ \t]*:END:"
	     (save-excursion (outline-next-heading) (point)) t)
	    (outline-flag-region b (point-at-eol) flag)
	  (error ":END: line missing at position %s" b))))))

(defun org-ilya-hide-drawer ()
  (interactive)
  (org-flag-drawer-fully 'hide))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun do-sel-disp () (interactive) (setq selective-display t) (message "sel-disp is %s" selective-display))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (bh/insert-inactive-timestamp)))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp)
(remove-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp)


(defun org-make-tags-matcher-with-prefilter (match)
  "Create the TAGS/TODO matcher form for the selection string MATCH.
Also construct a regexp that limits the entries that must be considered
when searching for the match.
"
  ;; todo-only is scoped dynamically into this function, and the function
  ;; may change it if the matcher asks for it.
  (unless match
    ;; Get a new match request, with completion
    (let ((org-last-tags-completion-table
	   (org-global-tags-completion-table)))
      (setq match (org-completing-read-no-i
		   "Match: " 'org-tags-completion-function nil nil nil
		   'org-tags-history))))

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


(defun org-recent ()
  "Find entries that have a timestamp (active or inactive) within a specified time period,
and list them in reverse chronological order.  If you use timestamps to indicate recently-worked-on files,
"

  ;; prepare a custom regexp for just the recent entries.
  ;; option to include future dates or not.  (for this purpose, probably not.)
  
  )

(defun org-ilya-show-props ()
  (interactive)
  (message "%s" (org-entry-properties)))

(defun org-ilya-one-prop ()
  (interactive)
  (message "%s" (org-entry-get nil "PRIORITY" (not 'inherit))))


(defadvice org-scan-tags (around print-args-and-results first
				 (action matcher &optional todo-only start-level)
				 activate)
  (message "entering org-scan-tags: action=%s matcher=%s todo-only=%s start-level=%s"
	   action matcher todo-only start-level)
  (let ((result ad-do-it))
    (message "returning from org-scan-tags: result=%S" result)
	     
    result))

(ad-disable-advice 'org-scan-tags 'around 'print-args-and-results)
(ad-activate 'org-scan-tags)

(defun org-ilya-get-tags ()
  (interactive)
  (message "%s" (org-get-buffer-tags)))
  


(defvar testvar 239)

(let ((testvar nil))
  (makunbound 'testvar)
  (condition-case err
      (progn
	(message "testvar is %s" testvar)
	)
    (void-variable (message "tried to read var %s" err))))

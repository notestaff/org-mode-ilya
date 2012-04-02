;;; org-balance.el --- Balance your resources among areas of life
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
;; org-balance: orgmode tools for setting goals for how much time/effort you want to spend on various
;; areas of life, and tracking how well you meet them.
;;
;; See info node for this.
;;
;; Lets you set goals for how much time/effort to spend on various areas,
;; at various levels of granularity.  E.g. you can say "read or try something new" on average
;; once a week, and be very flexible about how you meet that.
;; 
;; Also lets you track progress towards specific goals.
;;
;; Externally usable functions:
;;
;;   Top-level dispatcher:
;;
;;      org-balance-menu - show a menu of org-balance commands
;;
;;   Recording:
;;
;;      org-balance-record-time - record time spent under the current entry.
;;      org-balance-record-
;;
;;   Reporting:
;;
;;      org-balance-done-in-range - show items completed in given date range (e.g. within last two weeks).
;;      org-balance-show-clocked-time - show things on which you worked recently
;;      org-balance-show-neglected-time - show things on which you spend less time than you want
;;      org-balance-show-non-neglected-time - show things on which you spend enough time
;;      org-balance-show-neglected-val - show things where you get less done than you want
;;

;; misc things to add:
;;   describe-structure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: external dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl))
(require 'elu)
(elu-require time-date org org-clock org-agenda org-compat org-macs
	     org-archive rxx elu-valu elu-intervals)

(def-rxx-namespace org-balance "Org-balance regexps"
  :imports elu-valu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: External variables referenced by org-balance module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-clock-report-include-clocking-task)
(defvar org-archive-reversed-order)

;; also do declare-function for anything we do not 'require

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: org-balance customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst org-balance-goal-todo-keyword "GOAL")

(defconst org-balance-goal-todo-keyword-multiple (concat org-balance-goal-todo-keyword "S"))

(defconst org-balance-actualtime-prop "actualtime"
  "The property representing the actual time within an interval")

(defconst org-balance-clockedtime-prop "clockedtime"
  "The property representing the clocked time intersecting an interval")

(defgroup org-balance nil
  "Options for the org-balance package"
  :tag "Org Balance"
  :group 'org
  :link '(url-link "http://sourceforge.net/projects/org-balance/")
  )

(defcustom org-balance-default-interval (cons "-1w" "now")
  "The default time interval for which to report goal status,
in the syntax of `org-matcher-time'."
  :group 'org-balance
  :type '(cons string string))

(defcustom org-balance-default-margin-percent 5.0
  "Report entries as neglected if they are neglected by at least this percentage"
  :group 'org-balance
  :type 'float)

(defcustom org-balance-default-comparison-op (quote ((time . >=) (count . >=) (money . <=)
						     (unpleasantness . >=)))
  "Default comparison op to use for specific properties"
  :group 'org-balance
  :type '(alist :key-type symbol :value-type (radio (const :tag "at least" >=)
						    (const :tag "at most" <=))))

(defcustom org-balance-agenda-sorting-strategy
  '(priority-down category-keep user-defined-down)
  "Sorting rules for sorting org-balance agenda views."
  :group 'org-balance
  :link '(variable-link org-agenda-sorting-strategy)
  :type `(repeat ,org-sorting-choice))

(defgroup org-balance-faces nil
  "Faces in org-balance"
  :tag "org-balance faces"
  :group 'org-balance)

(defface org-balance-malformed-goal
  '((((background light)) (:foreground "Firebrick"))
    (((background dark)) (:foreground "black")))
  "Face used for showing malformed goals"
  )

(defconst org-balance-custom-commands
  (quote (("b" . "Org-balance commands")
	  ("br" "Recent items" tags "TIMESTAMP>\"<-4w>\"|TIMESTAMP_IA>\"<-4w>\"/-GOAL"
	   ((org-agenda-overriding-header "Org-balance recent items")
	    (org-agenda-sorting-strategy (quote (user-defined-down)))
	    (org-agenda-cmp-user-defined (quote org-balance-cmp-recent))
	    (org-agenda-before-sorting-filter-function (quote org-balance-save-timestamp))
	    ;(org-agenda-skip-function (quote (org-agenda-skip-entry-if (quote nottimestamp))))
	    (org-show-hierarchy-above (quote ((agenda . t))))))
	  ("bt" "Recent todos" tags "DEADLINE={\\S-}&DEADLINE<\"<+1w\">/!-GOAL"
	   ((org-agenda-overriding-header "Org-balance recent todos")
	    (org-agenda-sorting-strategy (quote (priority-down
						 category-keep
						 user-defined-down)))
	    (org-agenda-cmp-user-defined (quote org-balance-cmp-recent))
	    (org-agenda-before-sorting-filter-function (quote org-balance-save-timestamp))
	    (org-show-hierarchy-above (quote ((agenda . t))))))
	  ("bd" "Deadlines" tags-todo "DEADLINE={\\S-}&DEADLINE<\"<+1w\">/!-GOAL-DONE"
	   ((org-agenda-overriding-header "Org-balance deadlines")
	    (org-agenda-sorting-strategy (quote (priority-down
						 category-keep
						 user-defined-down)))
	    (org-agenda-cmp-user-defined (quote org-balance-cmp-recent))
	    (org-agenda-before-sorting-filter-function (quote org-balance-save-timestamp))
	    (org-show-hierarchy-above (quote ((agenda . t))))))
	  ("bn" "Neglected items" tags "org_balance_delta_val<0&TODO=\"GOAL\""
	   ((org-agenda-overriding-header "Org-balance neglected items")
	    (org-agenda-sorting-strategy (quote (priority-down
						 category-keep
						 user-defined-up)))
	    (org-agenda-cmp-user-defined (quote org-balance-cmp))
	    (org-agenda-before-sorting-filter-function (quote org-balance-save-amt-neglected))
	    (org-show-hierarchy-above (quote ((agenda . t))))
	    (org-agenda-overriding-columns-format "%org_balance_heading %PRIORITY %CATEGORY %org_balance_delta_val(delta)")
	    (org-not-done-keywords (list "GOAL"))))
	  ("bs" "Neglected goals in current file" tags-tree "org_balance_delta_val<0/!GOAL" ((org-not-done-keywords (list "GOAL"))))
	  ("bk" "Ok items" tags "org_balance_delta_val>=0/!GOAL"
	   ((org-agenda-overriding-header "Org-balance neglected items")
	    (org-agenda-sorting-strategy (quote (priority-down
						 category-keep
						 user-defined-up)))
	    (org-agenda-cmp-user-defined (quote org-balance-cmp))
	    (org-agenda-before-sorting-filter-function (quote org-balance-save-amt-neglected))
	    (org-show-hierarchy-above (quote ((agenda . t))))
	    (org-not-done-keywords (list "GOAL"))))
	  ("bf" "Ok goals in current file" tags-tree "org_balance_delta_val>=0/!GOAL" ((org-not-done-keywords (list "GOAL"))))
	  ))
  "Custom agenda commands for accessing org results")

(defun org-balance-agenda ()
  "Show agenda dispatcher with org-balance custom definitions"
  (interactive)
  (let ((org-agenda-custom-commands org-balance-custom-commands))
    (call-interactively 'org-agenda)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Org-regexps
;;
;; Org mode regexps, defined here using rxx so they can be used as
;; building blocks in larger regexps.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defrxx org-balance priority )
;;   --> this just expands to creating a constant var that stores the definition and the module.
;;       code then passes this constant var to rxx-parse-fwd, which constructs the buffer-local
;;       interpretation if needed, then uses it.  so for future calls we have the string regexp as well
;;       as the parser, so don't care about compilations.

;; and it's good that the names are explicitly put into the module.
;; yes, it's a good design.

(def-rxx org-balance priority
  "A priority cookie on an Org headline.   Parsed as the entire cookie (e.g. [#A])."
  (seq "[#" (any upper digit) "]"))

(def-rxx org-balance goal-prefix
  "The part of a GOAL line up to the goal name: the stars, the GOAL keyword, and optionally the priority. "
  (seq bol (sep-by blanks (seq (1+ "*")) (seq bow (eval org-balance-goal-todo-keyword) eow) priority?) blanks?))

(def-rxx org-balance tag-name "The name of a tag" (1+ (any alnum "_@#%")))
(def-rxx org-balance tags "The tags at the end of a headline"
  (& blanks? (opt ":" (1+ (& tag-name ":")) eol)) tag-name-list)

(def-rxx org-balance matcher "A tags and properties matcher, as described at
URL 'http://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties'."
  (1+ nonl) org-make-tags-matcher)

(def-rxx org-balance inactive-timestamp
  "An Org-mode inactive timestamp, such as '[2010-09-07 Tue 18:30]'.
Parsed as a floating-point value of time in seconds."
  (seq "[" (named-grp time (1+ nonl)) "]" )
  (org-float-time (apply 'encode-time (org-parse-time-string time))))

(def-rxx org-balance clock
  "An Org-mode clock line giving a time interval, such as when
logging work time (see Info node `(org)Clocking work time').
E.g. '	CLOCK: [2010-09-09 Thu 06:30]--[2010-09-09 Thu 12:46] =>  6:16'.
Parsed as a cons of the start and end times."
  (seq bol blanks? (eval org-clock-string) blanks?
		   (inactive-timestamp from) (1+ "-")
		   (inactive-timestamp to)
		   " => " (1+ nonl))
  (cons from to))


(def-rxx org-balance closed
  "An Org-mode line indicating when an entry was closed.
Parsed as the floating-point time."
  (sep-by blanks bol (or (eval org-closed-string) "- State \"DONE\"       from \"NEXT\"      ")
	  (inactive-timestamp closed-time))
  ;; FIXME: support state logging.  for now, assume the default format.
  ;; so, if it goes as - State from   etc -- we take the timestamp.  but only if it was the done state!
  ;; figure out how to customize this in case the user's thing has been customized.
  closed-time)

(def-rxx org-balance prop-name "A valid name of an Org entry property" (& alpha (0+ (any alnum "_-"))))
(def-rxx org-balance link "An Org link. Parsed as an elu-loc at the target of the link."
  (named-grp link (eval-regexp (rxx-make-shy org-any-link-re)))
  (elu-save excursion restriction window-excursion match-data
    (goto-char (rxx-match-beginning 'link))
    (org-balance-open-at-point)
    (point-elu-loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section: Utils
;;
;; General-purpose utility routines and org-mode helper routines
;; used in org-balance module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-balance-get-property (prop &optional default-val)
  "Get the value of a property, represented either as text property or org property,
at point.  If PROP is a symbol or keyword it is assumed
to be a text property; otherwise (if it is a string) it is assumed
to be an org entry property. Some properties are represented as text properties, and some as org properties;
this function gives uniform access to a named property regardless of how it is represented.
The advantage of org properties is that they're saved with the file; the advantage of
text properties is that they don't create clutter for the user.

Properties stored as text properties are named using keywords, such a :myprop,
or symbols, such as 'myprop; properties stored as org properties are named using strings.
"
  (let ((prop-val (if (stringp prop)
		      (org-entry-get nil prop)
		    (get-text-property (point) prop))))
    (or prop-val default-val)))

(defun org-balance-set-property (prop val &optional default-val clear-when-default)
  "Set the value of an org property or a text property PROP
to the value VAL.  If PROP is a symbol or keyword it is assumed
to be a text property; otherwise (if it is a string) it is assumed
to be an org entry property.  The point is assumed to be on an org entry headline.  For
text property, it is set for the whole headline.
If DEFAULT-VAL is given and VAL is the DEFAULT-VAL, then the property
is not set (and is removed if it was set before and CLEAR-WHEN_DEFAULT is non-nil)."
  (let ((is-default (equal val default-val)))
    (if (stringp prop)
	(progn
	  (if is-default
	      (when clear-when-default (org-entry-delete nil prop))
	    (org-entry-put nil prop (format "%s" val))))	
      (progn
	(when (or (not is-default) clear-when-default)
	  (elu-remove-list-of-text-properties (point-at-bol) (point-at-eol) (list prop)))
	(unless is-default
	  (put-text-property (point-at-bol) (point-at-eol) prop val))))))

(defun org-balance-delete-property-globally (prop)
  "Delete either text or org property globally from all entries."
    (if (stringp prop)
	(org-delete-property-globally prop)
      (elu-remove-list-of-text-properties (point-min) (point-max) (list prop))))

(defun org-balance-open-at-point ()
  "Open link at point, silently"
  
  (elu-ignoring org-show-context org-remove-occur-highlights org-offer-links-in-entry
    (let ((org-link-search-must-match-exact-headline t))
      (org-open-at-point 'in-emacs))))

;; (defun org-balance-start-of-tags ()
;;   "Return position where tags start on the current headline"
;;   (elu-save match-data excursion
;;     (goto-char (point-at-eol))
;;     (rxx-search-bwd org-balance-tags-regexp (point-at-bol))
;;     (point)))

(defun org-balance-interval-to-string (interval)
  (elu-interval-to-string interval (org-time-stamp-format 'long 'inactive) "--"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Representing goals
;;
;; Data structures for representing goals, and parsing definitions
;; for parsing goal specifications.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct
  (org-balance-prop-sum-def
   (:constructor nil)
   (:constructor
    make-org-balance-prop-sum-def
    (&key
     prop link matcher
     &aux
     ((range subtree-p heading)
      (let (range subtree-p heading not-subtree-p)
	(unless (string= prop org-balance-actualtime-prop)
	  (elu-save excursion restriction window-excursion match-data
	    ;; Go to the root of the subtree over which we should sum;
	    ;; if no subtree there, then we sum over the whole file.
	    (if link (elu-goto link)
	      (condition-case nil
		  (progn
		    (outline-up-heading 1 'invisible-ok)
		    (when (string= (upcase (org-get-heading)) org-balance-goal-todo-keyword-multiple)
		      (outline-up-heading 1 'invisible-ok)))
		(error (setq not-subtree-p t))))
	    (setq subtree-p (and (not not-subtree-p) (org-at-heading-p)))
	    (if subtree-p
		(setq range (make-elu-loc-range (point-elu-loc)
						(save-excursion
						  (org-end-of-subtree 'invisible-ok)
						  (point-elu-loc)))
		      heading (org-get-heading 'no-tags 'no-todo))
	      (setq range (make-elu-loc-range (point-min-elu-loc) (point-max-elu-loc))
		    heading
		    (or
		     (buffer-file-name
		      (org-base-buffer (current-buffer)))
		     "unknown-file.org")))))
	(list range subtree-p heading))))))

  "Recipe for computing the sum of a property under a subtree.  (prop-sum-def stands for
'property summing definition').  The property is always summed for some specific time interval.

Fields:

  Passed to the constructor:

    PROP - an Org property name, 'clockedtime', or 'actualtime'.
         When summing the value of a property in a given subtree in a given time interval,
         for an Org property name we get that property's value in matching TODO entries closed
         in the time interval;
         for 'clockedtime' we get the intersection of any clocked time in the matching entries
         with the interval; and for 'actualtime' we get the full length of the interval.

         If PROP is 'actualtime' the remaining fields are nil.

    LINK - specifies where to sum the property PROP.  From the
       link, we compute the location range within an Org file
       over which to do the sum, and store that in RANGE (defined
       below).  It is an `elu-loc' for the
       root of the subtree under which to do the sum.  If no link
       is given, the link defaults to the parent of the Org node
       at point when the constructor is called. If that parent's headline text equals
       `org-balance-goal-todo-keyword-multiple',
       case-insensitively, then the link defaults to the parent
       of the parent (allowing you to group multiple goals for a
       headline under a single 'goals' headline).  If the target
       of the link is not the root of a subtree, then the sum is
       done over the entire Org file
       (respecting any current restriction on the file).

    MATCHER - if not nil, only consider Org entries matching the matcher, as specified at
       URL 'http://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties'.

  Computed from the passed arguments:

    RANGE - the range of a buffer within which we do summing.  An `elu-loc-range' struct.
       Defined by LINK.

    SUBTREE-P - whether RANGE defines summing over a subtree (t), or over entire file (nil).

    HEADING - if SUBTREE-P, the headline at the subtree root, else the filename of the
       file over which we're summing.
"
  prop link matcher
  range subtree-p heading)

(defun adapt-org-balance-prop-sum-def-to-point (psd)
  "Adapt PSD to current point"
  (elu-with 'org-balance-prop-sum-def psd (prop matcher link)
    (make-org-balance-prop-sum-def :prop prop :matcher matcher :link link)))

(def-rxx org-balance prop-sum-def
  "The name of a property to be summed for a subtree within a given time interval,
optionally followed by a link to the subtree.
The property can be an Org property name, 'clockedtime' or 'actualtime'.  Parsed as a 
struct with fields for the property name and the link.
So, this is really more than just a property name -- it is a specification of 
summing a particular property over a subset of entries of a particular subtree.
"
  (& prop-name (opt blanks "at" blanks link) (opt blanks "(" blanks? matcher blanks? ")") )

  ;; FIXME if link is blank, default to going up once or twice
  ;; we have the (match-beginning 'prop-name) to tell us the headline.

  ;; FIXME handle file-wide goals and summing.
  
  (make-org-balance-prop-sum-def
   :prop prop-name
   :matcher matcher :link link))

(defstruct (org-balance-prop-sum-ratio-def (:include elu-ratio))
  "Recipe for computing a ratio of two property sums.")
(defstruct (org-balance-prop-sum-ratio (:include elu-ratio))
  "Actual value of a ratio of two property sums.")

(defstruct (org-balance-goal (:include rxx-parsed-obj))
  "A goal for the value of a property ratio under a given subtree -- e.g.
'at least three hours per week' or 'at most twice a month'.

Fields:
  RATIO-DEF - ratio of property sum definitions, eg clockedtime / actualtime

  THRESH - the threshold value of this goal, eg 3 hrs/wk. represented as an
    elu-valu-ratio.

  OP - the operation to use for comparing the actual value of the ratio of PROPS
    to THRESH.  i.e. the goal is met if (OP PROPS THRESH) is true.

"
  ratio-def thresh op)

(def-rxx org-balance comparison-op "A comparison operator"
  (or ">" "<" ">=" "=>" "<=" "at most" "at least")
  (lambda (s)
    (cond ((equal s "at most") '<=)
	  ((equal s "at least") '>=)
	  ((intern s)))))

(defun org-balance-goal-delta-sign (goal)
  (case (org-balance-goal-op goal)
    ((> >=) +1)
    ((< <=) -1)
    (t (error "unknown sign in goal %s" goal))))

;; next: figure out how to record the range properly, in case the numer or the denom is omitted.
;; i.e. we have to do here what we do above when parsing prop-sum-def, so, maybe,
;; should 


(def-rxx org-balance goal
  "A goal, such as 'clockedtime / actualtime : >= 3 hours / week"
  (sep-by blanks?
    (opt (sep-by blanks? (prop-sum-def ratio-def-numer) (opt ratio-word blanks? (prop-sum-def ratio-def-denom)) ":"))
    (opt comparison-op)
    (valu thresh-numer) ratio-word (valu thresh-denom))
  (let ((thresh-numer-dim (elu-valu-unit2dim (elu-valu-unit thresh-numer))))
    (make-org-balance-goal
     :ratio-def
     (make-org-balance-prop-sum-ratio-def
      :numer
      (or ratio-def-numer
	  (make-org-balance-prop-sum-def
	   :prop
	   (case thresh-numer-dim
	     ('time org-balance-clockedtime-prop) ('count "done") ('money "spend")
	     (t (error "Can only omit property name if it's clockedtime or done or spend")))))
      :denom (or ratio-def-denom (make-org-balance-prop-sum-def :prop org-balance-actualtime-prop)))
     
     :op (or comparison-op (cdr (assq thresh-numer-dim org-balance-default-comparison-op)))
     :thresh (make-elu-valu-ratio :numer thresh-numer :denom thresh-denom))))

(defstruct (org-balance-relative-goal (:include org-balance-goal))
  "If this goal is expressed in terms of another goal, the following fields
  are used:

  ORIG - the original goal, in terms of which this goal is expressed.

  ACTUAL-P - if t, this goal is specified in terms of the actual
    value the props ratio at the orig goal. if nil, this goal is specified
    in terms of the THRESH of the orig goal.
 
  FACTOR - the numeric factor by which to multiply either the THRESH or the
    actual value at the orig goal to get our threshold.
"
  orig actual-p factor)

(def-rxx org-balance goal-or-relative-goal-headline "A goal expressed directly or in terms of another goal"
  (seq blanks?
       goal-prefix
       blanks?
       (or goal
	   (sep-by blanks? (number factor) "of" (opt (named-grp actual-p "actual")) link))
       blanks?
       tags?
       )
  (or
   goal
   (let ((orig-goal (elu-save excursion window-excursion
		      (elu-goto link)
		      (rxx-parse-fwd org-balance goal-or-relative-goal-headline (point-at-eol)))))
     (elu-with 'org-balance-goal orig-goal (ratio-def op thresh)
       (make-org-balance-relative-goal
	:actual-p actual-p :factor factor
	:orig orig-goal
	:ratio-def
	(elu-with 'elu-ratio ratio-def (numer denom)
	  (make-org-balance-prop-sum-ratio-def
	   :numer
	   (adapt-org-balance-prop-sum-def-to-point numer)
	   :denom
	   (adapt-org-balance-prop-sum-def-to-point denom)))
	:op op
	:thresh (scale-elu-valu-ratio factor thresh))))))

(defun org-balance-at-goal-p ()
  "Test whether we're at a goal"
  (save-match-data
    (and (org-at-heading-p)
	 (equal (org-entry-get nil "TODO") org-balance-goal-todo-keyword))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Computing actual prop-sums and prop-sum ratios.
;;
;; For a given set of intervals and a given subtree, compute the sum
;; of a given property, and the ratio of two such sums.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-balance-clock-sum-add-interval (intervals totals tmin tmax)
  "For each time interval in INTERVALS that overlap [TMIN,TMAX], add the length of the overlap
to the corresponding element of the vector TOTALS.  Used by `org-balance-clock-sum'."
  (do-elu-intervals-overlapping-interval intervals tmin tmax i tbeg tend
    (incf (aref totals i) (- tend tbeg))))

(defun org-balance-clock-sum (intervals)
  "For each time interval in INTERVALS, return the total clocked time intersecting that interval, in the current file restriction.
Adapted from `org-clock-sum'"
  (let (; var: total-seconds - for each interval in INTERVALS, the total number of seconds of clocked time overlapping that interval
	;      in the current file restriction.
	(total-seconds (make-vector (elu-intervals-n intervals) 0)))
    ; Add the running clock, if there is one
    (when (and
	   org-clock-report-include-clocking-task
	   (equal (org-clocking-buffer) (current-buffer))
	   (<= (point-min) (marker-position org-clock-hd-marker))
	   (<= (marker-position org-clock-hd-marker) (point-max)))
      (org-balance-clock-sum-add-interval intervals total-seconds (org-float-time org-clock-start-time) (org-float-time)))
    ; Loop over the clock records in the restriction; for each, find the intervals in INTERVALS overlapping that clock record,
    ; and add the length of the overlap to the corresponding TOTAL-SECONDS entry
    (elu-save excursion match-data
      (goto-char (point-min))
      (rxx-do-search-fwd org-balance clock clock-interval
	(org-balance-clock-sum-add-interval intervals total-seconds (car clock-interval) (cdr clock-interval))))
    ; Convert TOTAL-SECONDS (array of floating-point interval lengths in seconds) to a vector of elu-valus with seconds as the unit,
    ; and return.
    (let ((result (make-vector (length total-seconds) nil)))
      (dotimes (i (length total-seconds) result)
	(aset result i (new-elu-valu (aref total-seconds i) 'seconds))))))

(defun org-balance-gather-org-property (prop intervals prop-default-val)
  "For each interval in INTERVALS, for todo items in the current buffer or restriction that were
closed within that interval, compute the list of values of Org property PROP from these items.
The value of the property at an item 
Originally adapted from `org-closed-in-range'.
"
  ;; FIXOPT: if prop-default is zero then the regexp for that subtree should be, org-closed-string _and_ the prop is explicitly set _in that entry_.  (1+ (bol) (opt (not (any ?*))) (0+ nonl) (eol))
  ;; FIXME: find also state changes to DONE, or to any done state.
  (declare (special org-balance-num-warnings))
  (elu-save excursion match-data
    (goto-char (point-min))
    (let ((prop-occurrences (make-vector (elu-intervals-n intervals) nil))
	  (prop-default (concat "default_" prop)))
      (rxx-do-search-fwd org-balance closed closed-time
	
	;; first, if closed-time is completely outside our range of intervals, just skip.
	
	;; FIXME: then, if tags matcher is given, evaluate it and only count this headline if it says true.
	;; don't forget to give it tags, todo-only, and category, if it needs them!
	
	(do-elu-intervals-overlapping-interval
	 intervals closed-time closed-time i tbeg tend
	 (elu-save excursion match-data
	   (org-back-to-heading 'invis-ok)
	   (let*
	       ((pos-here (point))
		(prop-here
		 (or (org-entry-get nil prop)
		     (org-entry-get nil prop-default 'inherit)
		     prop-default-val
		     "1"))
		(prop-valu-here
		 (condition-case err
		     (parse-elu-valu prop-here)
		   (error
		    (message
		     "Warning: at line %d of file %s, could not add %s to list for goal %s; list-so-far is %s"
		     (line-number-at-pos (point)) (buffer-file-name (current-buffer)) prop-here prop prop-occurrences)
		    (incf org-balance-num-warnings)
		    nil))))
	     (when (and prop-valu-here (not (zerop (elu-valu-val prop-valu-here))))
	       ;(message "adding prop at %s val %s for prop %s" pos-here prop-valu-here prop)
	       (push (cons pos-here prop-valu-here) (aref prop-occurrences i)))))))
      prop-occurrences)))


(defun org-balance-sum-org-property (prop intervals unit prop-default-val)
  "For each interval in INTERVALS, compute the sum of values of Org property PROP from todo items closed in that interval.
The sum will be represented in unit UNIT.  The sum is computed within the current restriction, if any."
  (elu-map-vectors
   (lambda (vals)
     (elu-valu-do-sum (new-elu-valu 0 unit)
		      (mapcar 'cdr vals)))
   (org-balance-gather-org-property
    prop intervals prop-default-val)))


(defun org-balance-sum-property (prop intervals unit prop-default-val)
  "For each time interval in INTERVALS, compute the sum of property PROP over the current subtree
for that time interval.  The value of the property for each entry is determined as follows.  If property is the special
property 'clockedtime', then for each CLOCK line in the entry, we compute the intersection of that CLOCK line with
the interval, and the value is the duration of that intersection.  If property is the special property 'actualtime',
then the value is just the total length of the interval.  Otherwise, the property is an Org property; if the entry is
closed during the interval, we add the value of the property in that entry to the total."
  (cond ((string= prop org-balance-clockedtime-prop)
	 (org-balance-clock-sum intervals))
	((string= prop org-balance-actualtime-prop)
	 (make-vector (elu-intervals-n intervals)
		      (new-elu-valu (elu-intervals-width intervals) 'seconds)))
	(t (org-balance-sum-org-property prop intervals unit prop-default-val))))

(defstruct
  (org-balance-archive-loc
   (:constructor
    create-org-balance-archive-loc
    (loc
     &aux
     (file (elu-not-blank (org-extract-archive-file loc)))
     (heading (elu-not-blank (org-extract-archive-heading loc))))))
  "A destination for archived trees: an org file, and a heading within that file."
  file heading)

(def-rxx org-balance archive-loc
  "The archive location, specified as a property of an Org entry.
See Info node `(org) Archiving' and variable `org-archive-location'.
Parsed as the structure `org-balance-archive-loc'."
  (sep-by blanks bol ":ARCHIVE:" (named-grp loc (1+ nonl)))
  (create-org-balance-archive-loc loc))


(defun org-balance-find-all-archive-targets ()
  "Find all the places where an entry from the current subtree could have been archived."
  (elu-save excursion window-excursion restriction
    (let ((archive-locs (list (create-org-balance-archive-loc (org-get-local-archive-location)))))
      (org-narrow-to-subtree)
      (rxx-do-search-fwd org-balance archive-loc loc
	(elu-add-to-list 'archive-locs loc))
      archive-locs)))


(defun org-balance-sum-property-with-archives (prop intervals unit)
  "Sum a property within the current subtree, within each of a list of specified time intervals.
Include any entries that may have been archived from the current subtree."

  ;; special-case for actualtime: do not go to archives.

  ;; if num and denom are for same subtree, go over the archives only once.
  ;; (and determine which closed items intersect this).

  ;(message "SUMMING at %s; restr  = %s %s" (point) (point-min) (point-max))
  (let ((prop-sum
	 (elu-save excursion restriction window-excursion
	   (org-narrow-to-subtree)
;	   (message "summing %s within %s %s over %s as unit %s"
;		    prop (point-min) (point-max) intervals unit)
	   (org-balance-sum-property prop intervals unit (not 'prop-default-val))))
	(prop-default-val (org-entry-get nil (concat "default_" prop) 'inherit)))
    (unless (string= prop org-balance-actualtime-prop)
      (let ((olpath-regexp (concat "^[ \t]+:ARCHIVE_OLPATH: " (mapconcat 'identity (org-get-outline-path) "/"))))
	(dolist (loc (org-balance-find-all-archive-targets))
	  (elu-save excursion window-excursion restriction match-data
	    (when (org-balance-archive-loc-file loc)
	      (find-file (org-balance-archive-loc-file loc)))
	    (widen)
	    (when (org-balance-archive-loc-heading loc)
	      (save-match-data
		(when (re-search-forward
		       (concat "^" (regexp-quote (org-balance-archive-loc-heading loc))
			       (org-re "[ \t]*\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*\\($\\|\r\\)"))
		       nil t)
		  (progn
		    (goto-char (match-beginning 0))
		    (org-narrow-to-subtree)))))
	    
	    (goto-char (if org-archive-reversed-order (point-min) (point-max)))
	    (while (funcall (if org-archive-reversed-order 're-search-forward 're-search-backward)
			    olpath-regexp (not 'bounded) 'no-error)
	      (elu-save excursion restriction match-data
		(org-back-to-heading 'invisible-ok)
		(setq prop-sum
		      (add-elu-valu-vec
		       prop-sum
		       (org-balance-sum-property prop intervals unit
						 prop-default-val)))))))))
    prop-sum))


(defun org-balance-compute-prop-sums (prop-sum-def intervals unit)
  "Computes the actual property sum as defined by PROP-SUM-DEF,
expressed in units UNIT, for each of the given INTERVALS."
  ;; check for cache of sums.  possibly record sums together with the
  ;; buffer-chars-modified-tick.
  ;;    --> what happens for indirect buffers?
  ;;        if we create an indirect buffer to monitor part of a file,
  ;;        will that tell us if only part of the file has changed?
  (elu-save excursion window-excursion match-data
    ;(message "computing sum for def %s" prop-sum-def)
    (elu-with 'org-balance-prop-sum-def prop-sum-def (prop range matcher subtree-p)
      (if (string= prop org-balance-actualtime-prop)
	  (apply 'vector
		 (mapcar 
		  (lambda (interval)
		    (new-elu-valu (elu-interval-length interval) 'seconds))
		  (elu-intervals-list intervals)))
	(elu-goto range)
	(if subtree-p
	    (org-balance-sum-property-with-archives prop intervals unit)
	  (let ((sums
		 (elu-make-vector (elu-intervals-n intervals) (new-elu-valu 0 unit))))
	    (while (progn
		     (org-get-next-sibling)
		     (org-at-heading-p))
	      (elu-save excursion restriction match-data window-excursion
		(org-narrow-to-subtree)
		(setq
		 sums
		 (add-elu-valu-vec
		  sums
		  (org-balance-sum-property-with-archives prop intervals unit)))))
	    sums))))))

(defun org-balance-compute-prop-sum-ratios (prop-sum-ratio-def intervals express-as)
  "For each interval in INTERVALS, compute the ratio of the actual value of the 
given props in that interval, expressed in the same units as EXPRESS-AS and having
the same denominator value as EXPRESS-AS."
  (elu-with 'elu-valu-ratio express-as (numer denom)
    (elu-map-vectors
     (lambda (a-numer a-denom)
       (convert-elu-valu-ratio (make-elu-valu-ratio :numer a-numer :denom a-denom) express-as))
     (org-balance-compute-prop-sums (org-balance-prop-sum-ratio-def-numer prop-sum-ratio-def) intervals
				    (elu-valu-unit numer))
     (org-balance-compute-prop-sums (org-balance-prop-sum-ratio-def-denom prop-sum-ratio-def) intervals
				    (elu-valu-unit denom)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Computing goal status
;;
;; For a given goal, for a set of time intervals, compute how well
;; the goal was met in each interval.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct org-balance-goal-status
  "The status of one goal during given time intervals.
We compute it and store it programmatically in this structure,
so we can later present it to the user in various ways.

Fields:

  GOAL - the org-balance-goal structure representing the goal for
    which we're giving the status

  INTERVALS - the intervals during which this goal status is measured.
    An `elu-intervals' structure.

  ACTUALS - the actual value of the goal's property ratio (org-balance-goal-props GOAL)
    during each interval in INTERVALS.  This is to be compared with (org-balance-goal-thresh GOAL)
    using the comparison operator (org-balance-goal-op GOAL) to decide how well the goal is being met.
    An `elu-valu-ratio' with the same denominator as the goal's threshold.

  THRESHOLDS - the threshold against which ACTUALS is to be compared during each interval.
    The same for all intervals unless GOAL is relative to the actual value of another goal.

  DELTAS - the difference between ACTUALS and THRESHOLDS.
    a value indicating how well the goal is being met; zero means the goal is being met exactly,
    positive means it is being met with some to spare (and the magnitude indicates how much to spare),
    negative means it is not being met (and the magnitude indicates how much the actual is short
    of the goal).  An elu-valu-ratio with the same denominator as the goal's threshold.

  RELATIVE-DELTAS - if GOAL is relative, gives the factor (rather than the difference) by which
    the actual value differs from the threshold.  Undefined if the threshold is zero (can happen
    if the goal is defined relative to the actual value of another goal and that actual value
    is zero).

  ERR - if an error happens when computing the goal status (e.g. the goal specification could not be parsed),
    this stores the error; otherwise, nil.
"
  goal intervals actuals thresholds deltas relative-deltas err)

(defun org-balance-goal-thresholds (goal intervals)
  "Return the threshold valu of the goal"
  (if (not (org-balance-relative-goal-p goal))
      (make-vector (elu-intervals-n intervals) (org-balance-goal-thresh goal))
    (let* ((orig-goal (org-balance-relative-goal-orig goal))
	   (orig-goal-thresholds (org-balance-goal-thresholds orig-goal intervals)))
      (elu-map-vectors
       (apply-partially 'scale-elu-valu-ratio (org-balance-relative-goal-factor goal))
       (if (not (org-balance-relative-goal-actual-p goal))
	   orig-goal-thresholds
	 (org-balance-compute-prop-sum-ratios (org-balance-goal-ratio-def orig-goal)
					      intervals
					      (org-balance-goal-thresh goal)))))))

(defun* org-balance-compute-goal-status (&optional
					 pom
					 (intervals
					  (let ((beg (org-matcher-time (concat "<" (car org-balance-default-interval) ">")))
						(end (org-matcher-time (concat "<" (cdr org-balance-default-interval) ">"))))
					  (make-elu-intervals
					   :from beg :n 1 :shift (- end beg) :width (- end beg))
					  )))
  "Compute the status of the goal at point-or-marker POM during each interval in INTERVALS.
"
  (elu-save excursion restriction match-data window-excursion
    (org-with-point-at pom
      ;; back to heading
      ;; then, parse entry (stars) then the goal.
      ;; could use complex-heading-regexp.
      (condition-case err
	  (let* ((goal (rxx-parse-fwd org-balance goal-or-relative-goal-headline (point-at-eol))))
	    ;(message "parsed goal is %s" goal)
	    (elu-save excursion restriction window-excursion
	      ;; (outline-up-heading 1 'invisible-ok)
	      ;; (when (string= (upcase (org-get-heading)) "GOALS")
	      ;; 	(outline-up-heading 1 'invisible-ok))
	      ;; (org-narrow-to-subtree)
	      ;; (goto-char (point-min))
	      (let* (
		     (thresholds (org-balance-goal-thresholds goal intervals))
		     (actual-prop-sum-ratios
		      (org-balance-compute-prop-sum-ratios
		       (org-balance-goal-ratio-def goal)
		       intervals
		       (org-balance-goal-thresh goal)))
		     (deltas (elu-mapcar*
			      (lambda (actual-ratio threshold)
				(assert (equal (elu-valu-ratio-denom actual-ratio)
					       (elu-valu-ratio-denom threshold)))
				(scale-elu-valu (org-balance-goal-delta-sign goal)
						(sub-elu-valu (elu-valu-ratio-numer actual-ratio)
							      (elu-valu-ratio-numer threshold))))
			      actual-prop-sum-ratios
			      thresholds))
		     (relative-deltas
		      (when (org-balance-relative-goal-p goal)
			(elu-map-vectors
			 (lambda (actual-ratio threshold)
			   (let* ((threshold-numer-val (elu-valu-val (elu-valu-ratio-numer threshold)))
				  (actual-factor (if (zerop threshold-numer-val) 0.0e+NaN
						   (/ (elu-valu-val (elu-valu-ratio-numer actual-ratio))
						      threshold-numer-val))))
			     (* (org-balance-goal-delta-sign goal)
				(- actual-factor (org-balance-relative-goal-factor goal)))))
			 actual-prop-sum-ratios
			 thresholds))))
		
		(make-org-balance-goal-status
		 :goal goal :intervals intervals :actuals actual-prop-sum-ratios
		 :thresholds thresholds :deltas deltas :relative-deltas relative-deltas)
		
		;; - compute the actual ratio, based on the prop-def of the goal
		;; - compute the threshold for each interval
		;; - compare the actual ratio to the threshold to get the delta
		;; so, if the goal is relative, then we compute the actual factor
		;; for each interval, and 
		
		;; - if goal is relative, express the delta also in terms of the percent delta
		;;   (eg want this to be at least 30% and is actually 10%).
		
					;
					; so, next things:
		;;   - need to get the units of this.
		;;   - can just say, if this is in terms of another goal,
		;;     recursively get the goal status of that first.
		;;     then, compute our percentage in terms of that.
		
		;;  but, if it's in terms of the ideal of the other goal --
		;;  ah, but that ideal might still be expressed relatively.
		
					; so:
		;;
		;;  - say we're doing this for a set of goals.
		;;    then we could: parse them all, then compute
		;;    sums for all of them, then compute the status for all of them.
		
		;; but it might be simpler, if we enable timestamped caching
		;; of sums, to just do it one goal at a time.  there can't be too many goals.
		
		;; can cache using text props, or even just a scoped-in or passed-along cache indexed by pos, interval and prop.
		;;
		;; so, say we have parsed the thing we depend on.
		;;
		)))
	(error (make-org-balance-goal-status :err err))
	)
      )))


;; (defun org-balance-compute-delta (parsed-goals prop-ratio actuals)
;;   ""
;;   (elu-gen-vector i (length parsed-goals)
;;     (let ((parsed-goal (aref parsed-goals i))
;; 	  (actual (aref actuals i)))
;;       (let* ((polarity (or (org-balance-goal-polarity parsed-goal)
;; 			   (cdr (assoc (org-balance-prop-prop
;; 					(org-balance-prop-ratio-num  prop-ratio))
;; 				       org-balance-default-polarity))))
;; 	     (dummy (assert polarity))
;; 	     (margin org-balance-default-margin-percent)
;; 	     (goal-val (elu-valu-val (elu-valu-ratio-numer (org-balance-goal-thresh parsed-goal))))
;; 	     (range-min (- goal-val
;; 			   (if (numberp margin)
;; 			       (* (/ (float margin) 100.0) goal-val)
;; 			     (elu-valu-val margin))))
	     
;; 	     (range-max (+ goal-val
;; 			   (if (numberp margin)
;; 			       (* (/ (float margin) 100.0) goal-val)
;; 			     (elu-valu-val margin))))
	     
;; 	     (actual-num (elu-valu-val (elu-valu-ratio-numer actual)))
;; 	     (delta-val (cond ((and (<= range-min actual-num) (<= actual-num range-max)) 0)
;; 			      ((< range-max actual-num)
;; 			       (if (eq polarity 'atleast)
;; 				   (- actual-num goal-val)
;; 				 (- goal-val actual-num)))
;; 			      ((< actual-num range-min)
;; 			       (if (eq polarity 'atmost)
;; 				   (- goal-val actual-num)
;; 				 (- actual-num goal-val)))))
;; 	     (delta-percent
;; 	      (* 100 (/ delta-val goal-val))))
;; 	(cons delta-val delta-percent)))))

;; (defun* org-balance-compute-goal-deltas (&key intervals)
;;   "For each goal, determines the difference between the actual and desired average daily expenditure of
;; resource GOAL toward that goal in the period in each interval in INTERVALS.  Stores the results as properties
;; under each goal.
;; "
;;   (declare (special goal-update-time))
;;   (let ((org-show-hierarchy-above '((default . nil)))
;; 	(org-show-following-heading '((default . nil)))
;; 	(org-show-entry-below '((default . nil)))
;; 	(org-show-siblings '((default . nil))))
;;     (unless intervals (error "org-balance-compute-goal-deltas: need intervals"))
;;     (let ((num-errors 0)  ; number of errors (such as malformed goals) encountered
;; 	  (num-under 0)   ; number of goals where actual expenditure < goal
;; 	  (num-met 0)     ; number of goals where actual expenditure meets goal
;; 	  (num-over 0))   ; number of goals where actual expenditure > goal
;;       (save-excursion
;; 	(goto-char (point-min))
;; 	(save-restriction
;; 	  (save-match-data
;; 	    ;; FIXOPT if goals specified, make regexp for them
	    
;; 	    ;; Loop over all goals defined in the org file
;; 	    (rxx-do-search-fwd org-balance-goal-prefix-regexp nil
;; 	      (elu-save match-data restriction excursion
;; 		(condition-case err
;; 		    (let* (; var: goal-spec - the parsed specification of this goal, e.g. "clockedtime: one hour per week"
;; 					;   or "done: two times per day"
;; 			   (goal-spec (rxx-parse-fwd org-balance-goal-spec-regexp (org-balance-start-of-tags)))
;; 			   (prop-ratio (car goal-spec))
;; 			   (parsed-goal (cdr goal-spec)))
		      
;; 					; Some goal specs we encounter in the org file may be malformed (since they're user-typed).
;; 					; We tag such goal entries with the goal_error tag, so that the user can quickly find them.
;; 					; Initially, turn this tag off since we haven't found a problem with this goal spec yet.
;; 		      (save-match-data (org-toggle-tag "goal_error" 'off))
		      
;; 		      ;;
;; 		      ;; Compute the actual usage under this subtree, and convert to the same
;; 		      ;; units as the goal, so we can compare them.
;; 		      ;;
;; 		      (let (delta-val-and-percent)
;; 			(elu-save match-data excursion restriction
;; 			  (outline-up-heading 1 'invisible-ok)
;; 			  (when (string= (upcase (org-get-heading)) "GOALS")
;; 			    (outline-up-heading 1 'invisible-ok))
;; 			  (org-narrow-to-subtree)
;; 			  (goto-char (point-min))
;; 			  (setq delta-val-and-percent
;; 				(aref
;; 				 (org-balance-compute-delta
;; 				  parsed-goal prop-ratio
;; 				  (org-balance-compute-actual-prop-ratio prop-ratio intervals parsed-goal))
;; 				 0)))
;; 			(let ((delta-val (car delta-val-and-percent))
;; 			      (delta-percent (cdr delta-val-and-percent)))
;; 			  (cond ((< delta-val 0) (incf num-under))
;; 				((> delta-val 0) (incf num-over))
;; 				((= delta-val 0) (incf num-met)))
;; 			  (org-entry-put (point) "org_balance_delta_val" (format "%.2f" delta-val))
;; 			  (org-entry-put (point) "goal_delta_percent" (format "%.1f" delta-percent))
;; 			  ;; FIXME: include in goal_updated the period for which it was updated.
;; 			  ;; (org-entry-put (point) "goal_interval" (elu-format-seconds "%Y, %T, %W, %D%z"
;; 			  ;; 							       (- (elu-intervals-end intervals 0)
;; 			  ;; 								  (elu-intervals-start intervals 0))))
;; 			  (org-entry-put (point) "goal_updated"
;; 					 (format-time-string
;; 					  (org-time-stamp-format 'long 'inactive)
;; 					  (if (boundp 'goal-update-time) goal-update-time (current-time)))))))
;; 		  (error
;; 		   (unless (string= (upcase (org-get-heading)) "GOALS")
;; 		     (incf num-errors)
;; 		     (message "At %s: Error processing %s : %s " (point) (buffer-substring (point) (point-at-eol))
;; 			      err)
;; 		     (save-match-data
;; 		       (org-toggle-tag "goal_error" 'on)
;; 		       (org-entry-delete nil "org_balance_delta_val")
;; 		       (org-entry-delete nil "goal_delta_percent")
;; 		       (org-entry-put (point) "goal_updated"
;; 				      (format-time-string
;; 				       (org-time-stamp-format 'long 'inactive)
;; 				       (if (boundp 'goal-update-time) goal-update-time (current-time))))))
;; 		   nil)))))))
;;       (message "err %d under %d met %d over %d" num-errors num-under (+ num-met num-over) num-over))))
  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Interactive commands
;;
;; User-visible commands provided by org-balance.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-balance-show-status ()
  "Show the status of the goal at point"
  (interactive)
  (elu-save excursion restriction window-excursion
    (goto-char (point-at-bol))
    (message "status=%s" (org-balance-compute-goal-status
			  nil
			  (make-elu-intervals
			   :from (- (float-time) (* 60 60 24))
			   :n 1
			   :shift (* 60 60 24)
			   :width (* 60 60 24))))))

(defun org-balance-remove-props ()
  "Remove from each GOAL in the file, any previous record of how well this goal is being met.
Usually done in preparation for generating a new record of how well each goal is being met."
  (interactive)
  (elu-save excursion restriction
    (widen)
    (dolist (prop '("org_balance_delta_val" "goal_delta_percent" "goal_updated"))
      (org-delete-property-globally prop))
    (org-map-entries '(org-toggle-tag "goal_error" 'off) "+goal_error/!GOAL" 'file)))

;;;
;;; Subsection: Commands for recording items
;;;
;;; Most of the recording is done with standard Org commands,
;;; but this adds a few commands to make recording easier.

(defun org-balance-done-in-range ()
  "Sparse tree of items closed in a certain time range.
Still experimental, may disappear in the future.

Adapted from `org-closed-in-range' from org.el."
  (interactive)
  ;; Get the time interval from the user.
  (let* ((time1 (org-float-time
                 (org-read-date nil 'to-time nil "Starting date: ")))
         (time2 (org-float-time (org-current-time)))
         ;; callback function
         (callback (lambda ()
                     (let ((time
                            (org-float-time
                             (apply 'encode-time
                                    (org-parse-time-string
                                     (match-string 1))))))
                       ;; check if time in interval
                       (and (>= time time1) (<= time time2))))))
    ;; make tree, check each match with the callback
    (message "%s matches here" (org-occur "CLOSED: +\\[\\(.*?\\)\\]" nil callback))) )


(defun org-balance-record-time (&optional hours ago)
  "Record the given amount of time as time spent under the current org entry.
Useful when you did some task but weren't near a computer to start/stop the org timer
as you were doing it.
"
  (interactive)
  ;
  ; allow various specifications of time: :15 for 15 mins, 15m or any other (number, unit) thing.
  ; also compound specs such as 1 hour 15 mins (so, basically, ((value unit)+) as long as units
  ; are compatible and keep decreasing.
  ;
  ; also, have additional ways to enter some specific units -- e.g. $1 for money units,
  ; :15 for 15 mins, 01:15, etc.  1h30m, 1.5hrs, etc.
  ;
  (unless hours (setq hours (float (string-to-number (read-string "How many hours? ")))))
  (unless ago (setq ago (float (string-to-number (read-string "Finished how long ago (in hours)? " nil nil 0)))))

  (message "hours is %s ago is %s" hours ago)

  (let (target-pos (msg-extra ""))
    ;; Clock in at which position?
    (setq target-pos
	  (if (and (eobp) (not (org-on-heading-p)))
	      (point-at-bol 0)
	    (point)))

    (org-clock-find-position nil)

    (insert-before-markers "\n")
    (backward-char 1)
    (org-indent-line-function)
    (when (and (save-excursion
		 (end-of-line 0)
		 (org-in-item-p)))
      (beginning-of-line 1)
      (org-indent-line-to (- (org-get-indentation) 2)))
    (insert org-clock-string " ")

    (let* ((end-time (- (org-float-time) (* ago 60.0 60.0)))
	   (start-time (- end-time (* hours 60.0 60.0)))
	   (s (- end-time start-time))
	   (h (floor (/ s 3600.0)))
	   (s (- s (* 3600.0 h)))
	   (m (floor (/ s 60.0)))
	   (s (- s (* 60.0 s))))
      (org-insert-time-stamp (seconds-to-time start-time)
			     'with-hm 'inactive)
      (insert "--")
      (org-insert-time-stamp (seconds-to-time end-time)
			     'with-hm 'inactive)
      (insert " => " (format "%2d:%02d" h m)))))

(defconst org-balance-seconds-per-minute 60)
(defconst org-balance-minutes-per-hour 60)

(defun org-balance-record-done (&optional hours-ago)
  "Mark the current Org headline as DONE a specified time ago.
Useful for recording things done when you were not at the computer,
since for org-balance purposes it matters _when_ things got done.
See also `org-balance-record-time' and Info node `(org) MobileOrg'.
"
  (interactive)
  (save-excursion
    (org-back-to-heading 'invis-ok)
    (unless hours-ago
      (setq hours-ago (float (string-to-number (read-string "Finished how long ago (in hours)? " nil nil 0)))))
    (let* ((seconds-ago (* hours-ago org-balance-minutes-per-hour org-balance-seconds-per-minute))
	   (done-time (time-subtract (org-current-time) (seconds-to-time seconds-ago))))
      (flet ((org-current-time () done-time))
	(org-todo 'done)))))

(defun org-balance-update-goal-status-at (&optional pom)
  "Update the status of the goal at point"
  (interactive)
  (message "updating goal status at %s" (point))
  (when (org-balance-at-goal-p)
    ;; for what interval?
    ;; eg keep two strings, -1w and now, and update goal status just for that.
    (let ((status (org-balance-compute-goal-status pom)))
      (elu-with 'org-balance-goal-status status
		(goal intervals actuals thresholds deltas relative-deltas heading err)
	(org-entry-put pom "org_balance_updated" (format-time-string (org-time-stamp-format 'long 'inactive)))
	(if (not err)
	    (progn
	      (org-entry-delete pom "org_balance_error")
	      (apply 'org-entry-put-multivalued-property pom "org_balance_intervals"
		     (mapcar 'org-balance-interval-to-string (elu-intervals-list intervals)))
	      (org-entry-put pom "org_balance_actuals" (format "%s" actuals))
	      (org-entry-put pom "org_balance_thresholds" (format "%s" thresholds))
	      (org-entry-put pom "org_balance_deltas" (format "%s" deltas))
	      (org-entry-put pom "org_balance_delta_val" (format "%.2f" (elu-valu-val (first deltas))))
	      (org-entry-put pom "org_balance_heading" (org-balance-prop-sum-def-heading (elu-ratio-numer (org-balance-goal-ratio-def goal)))))
	  
	  (org-entry-put pom "org_balance_error" (format "%s" err))
	  (org-entry-delete pom "org_balance_intervals")
	  (org-entry-delete pom "org_balance_actuals")
	  (org-entry-delete pom "org_balance_thresholds")
	  (org-entry-delete pom "org_balance_deltas")))
      status)))

(defun org-balance-update-all-goal-status ()
  "Update the status of specified goals."
  (interactive)
  (let ((num-ok 0) (num-err 0))
    (org-map-entries
     (lambda ()
       (let ((status (org-balance-update-goal-status-at)))
	 (if (org-balance-goal-status-err status)
	     (incf num-err)
	   (incf num-ok))))
     "TODO=\"GOAL\"")
    (message "%s goals updated ok, %s had errors" num-ok num-err)))


;; (defun org-balance-deltas-week ()
;;   "Compute deltas for the preceding week"
;;   (interactive)
;;   (elu-ignoring org-show-context
;;     (let* ((secs-per-min 60)
;; 	   (mins-per-hour 60)
;; 	   (hours-per-day 24)
;; 	   (days-per-week 7)
;; 	   (secs-per-week (* secs-per-min mins-per-hour hours-per-day days-per-week))
;; 	   (cur-time (org-float-time))
;; 	   (week-ago (- cur-time secs-per-week)))
;;       (org-balance-compute-goal-deltas :intervals (make-elu-intervals :from week-ago :n 1 :width (- cur-time week-ago) :shift 0)))))

;;
;; Subsection: Menu access to org-balance commands
;;

(require 'easymenu)
(defconst org-balance-menu
  '(("Record your activity..."
     ["Record time spent" org-balance-record-time t]
     ["Record a DONE item" org-balance-record-time t]
    ["Update goal status" org-balance-update-goal-status t])))

;;;;;;;;;;;;;;

(defun org-balance-setup ()
  "Do the initial configuration needed for org-balance"
  ;; add (type "|" "GOAL") to org-todo-keywords, if not there already
  ;; set org-log-done to at least time
  )

 (defun org-balance-init-in-buffer ()
   (interactive)
;   (easy-menu-add-item org-mode-map nil (easy-menu-create-menu "OrgBal" '("An orgb")))
   (easy-menu-change '("Org") "Org-balance" org-balance-menu)
   )

(defun org-balance-insinuate ()
   (interactive)
   (add-hook
    'org-mode-hook 'org-balance-init-in-buffer)
   ;(org-add-agenda-custom-command org-balance-custom-commands)
   )

(defun org-balance-deinsinuate ()
  (interactive)
  (remove-hook 'org-mode-hook 'org-balance-init-in-buffer)
  (easy-menu-remove-item nil '("Org") "Org-balance"))


(defun org-balance-save-amt-neglected (agenda-line)
  "Given an agenda line, save the 'neglect amount' value of the corresponding org entry
as a text property, for later use by org-balance-cmp.  Also, add the neglect amount value
to the agenda line.
"
  (let ((orig-entry (get-text-property 0 'org-hd-marker agenda-line)))
    (when orig-entry
      (elu-save excursion current-buffer
	(set-buffer (marker-buffer orig-entry))
	(goto-char (marker-position orig-entry))
	(let ((org_balance_delta_val (string-to-number (or (org-entry-get (point) "org_balance_delta_val") "0")))
	      (heading (or (org-entry-get nil "org_balance_heading") "")))
	    (put-text-property 0 1 :org-balance-delta org_balance_delta_val agenda-line)
	    (concat agenda-line "::: " heading " ::: " (number-to-string org_balance_delta_val)))))))


(defun org-balance-save-timestamp (agenda-line)
  "Given an agenda line, save the most recent timestamp value of the corresponding org entry
as a text property, for later use by org-balance-cmp-recent.
"
  (let ((orig-entry (get-text-property 0 'org-hd-marker agenda-line)))
    (when orig-entry
      (let* ((timestamp (org-entry-get orig-entry "TIMESTAMP"))
	     (timestamp_float (if timestamp
				(org-float-time (apply 'encode-time (org-parse-time-string timestamp)))
				-1.0e+INF))
	     (timestamp_ia (org-entry-get orig-entry "TIMESTAMP_IA"))
	     (timestamp_ia_float (if timestamp_ia
				   (org-float-time (apply 'encode-time (org-parse-time-string timestamp_ia)))
				   -1.0e+INF))
	     (ts (max timestamp_float timestamp_ia_float)))
	(put-text-property 0 1 :org-balance-recent ts agenda-line)))
    agenda-line))

(defun org-balance-cmp (a b)
  "Compare agenda entries by the amount of neglect, with the most-neglected at the top."
  (let ((pa (or (get-text-property 0 :org-balance-delta a) 0))
	(pb (or (get-text-property 0 :org-balance-delta b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1)
	  (t nil))))

(defun org-balance-cmp-recent (a b)
  "Compare agenda entries by the recency of their timestamp."
  (let ((pa (or (get-text-property 0 :org-balance-recent a) 0))
	(pb (or (get-text-property 0 :org-balance-recent b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1)
	  (t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Section: Testing
;;
;; Regression testing code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst org-balance-regtest-dir "/cvar/selection/sweep2/nsvn/Tools/org/orgb/Org-balance/regtests/")
(defconst org-balance-regtest-defs
  '(;("mythings.org" 1283015820.0 1285607849.393998 (19616 55415 443943))
    ("rt1.org" 1284760020.0 1285624025.292002 (19617 4313 292003))))

(defconst org-balance-parse-test-defs
  '((inactive-timestamp "[2010-09-28 Tue 16:11]" 1285704660.0)
    (clock "		 CLOCK: [2010-09-07 Tue 21:07]--[2010-09-08 Wed 00:07] =>  3:00" (1283908020.0 . 1283918820.0))
    (closed "			 	CLOSED: [2009-08-27 Thu 11:58]" 1251388680.0)
    (closed "		- State \"DONE\"       from \"NEXT\"       [2009-08-27 Thu 11:58]" 1251388680.0)
;    (archive-loc "	 :ARCHIVE:  %s_archive::work archive"
;		 [cl-struct-org-balance-archive-loc "/cvar/selection/sweep2/nsvn/Tools/org/sf/trunk/org-balance.el_archive"
;						    "work archive"])
    (number "three" 3)
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
    (valu-range "2-3" ([cl-struct-elu-valu 2 item] . [cl-struct-elu-valu 3 item]))
;    (rxx-parse elu-valu-range-regexp "2-3")
;    (rxx-parse org-balance-goal-regexp "at least once every two weeks +- 11%")
    (goal "once a month"
	  [cl-struct-org-balance-goal [cl-struct-elu-valu 1 item] [cl-struct-elu-valu 1 item]
				      [cl-struct-elu-valu 1 month] nil nil "a" "once a month" nil])
    (goal "at least 2-3 times a week"
	  [cl-struct-org-balance-goal [cl-struct-elu-valu 2 times]
				      [cl-struct-elu-valu 3 times]
				      [cl-struct-elu-valu 1 week] atleast nil "a" "at least 2-3 times a week" nil])
    (goal "at most 1200 dollars per year +- 100 dollars"
	  [cl-struct-org-balance-goal [cl-struct-elu-valu 1200 dollars] [cl-struct-elu-valu 1200 dollars]
				      [cl-struct-elu-valu 1 year] atmost [cl-struct-elu-valu 100 dollars]
				      "per" "at most 1200 dollars per year +- 100 dollars" nil])
    (goal "at least once every two weeks +- 11%"
	  [cl-struct-org-balance-goal [cl-struct-elu-valu 1 item] [cl-struct-elu-valu 1 item]
				      [cl-struct-elu-valu 2 weeks] atleast 11
				      "every" "at least once every two weeks +- 11%" nil])))

(defun org-balance-test-parsing ()
  (interactive)
  (let ((num-ok 0))
    (dolist (parse-test org-balance-parse-test-defs)
      (message "parse-test is %s" parse-test)
      (let* ((the-symbol 
	      (first parse-test))
	     (dummy (message "the-symbol is %s" the-symbol))
	     (parse-result (rxx-parse-string-func
			    'org-balance
			    the-symbol
			    (second parse-test))))
	(unless (equal parse-result
		       (third parse-test))
	  (message "failed test: %s %s" parse-test parse-result)
	  (assert nil))))
    (incf num-ok)
    (message "%d parsing tests ok" num-ok)))

(defconst org-balance-test-files '("regtests/rt2.org"))

(defun org-balance-new-regtests ()
  (interactive)

  (dolist (test-file org-balance-test-files)
    (find-file test-file)
    (widen)
    (goto-char (point-min))
    (org-mode)

    (org-map-entries
     (lambda ()
       (let* ((intervals
	       (org-split-string (org-entry-get nil "REGTEST_INTERVALS" 'inherit)))
	      (ref-deltas-and-intervals
	       (org-split-string (org-entry-get nil "REFERENCE_DELTAS")))
	      (ref-deltas
	       (org-remove-if-not (lambda (v) (equal (substring v 0 1) "["))
				  ref-deltas-and-intervals))
	      (ref-intervals
	       (org-remove-if (lambda (v) (equal (substring v 0 1) "["))
			      ref-deltas-and-intervals))
	      )
	 (dolist (interval intervals)
	   ;; compute the actual delta here for this interval.
	   ;; (in fact, compute it for all intervals and return, first).

	   ;; then, for each interval, check if it's in the refernce list.
	   ;; if not, then save, if yes, then check.

	   ;; then, can easily add new regtests just by editing the file.


	   ;; *** need to compute all actuals first,
	   ;;     or else do this recursively somehow -- else cannot compute
	   ;;     the delta.

	   ; so, let's say we start changing things, and then re-establish the regtests.
	   ; the regtest values btw we can reuse.

	   ; so, need here a parse step.
	 
	 )
       )
     "LEVEL>0/!GOAL"
     )

    ; so, for a given entry, we can get the value
    ; of the (possibly inherited) regtest-intervals property,
    ; and then for each such interval, compute the current delta,
    ; and compare it with the saved one or save it if there is not
    ; saved one.   and record when the new one was entered,
    ; also as a property.

    ; can flag any mismatches as tags or properties also, if we want to;
    ; on the other hand, don't want to update the version-controlled
    ; file unless have to.

    ;
    ;
    ;
    
    )
  ))

;; (defun org-balance-regtests ()
;;   (interactive)
;;   (message "====================== org-balance regtests ===========================")
;;   (org-balance-test-parsing)
;;   (elu-save excursion window-excursion restriction match-data
;;     (let ((num-ok 0) (num-failed 0))
;;       (dolist (regtest org-balance-regtest-defs)
;; 	(let* ((test-file (concat org-balance-regtest-dir (first regtest)))
;; 	       (ref-file (concat (file-name-sans-extension test-file) "_ref.org")))
;; 	  (if (not (and (file-readable-p test-file) (file-readable-p ref-file)))
;; 	      (progn
;; 		(incf num-failed)
;; 		(message "Could not read test file %s or reference file %s" test-file ref-file))
;; 	    (find-file test-file)
;; 	    (widen)
;; 	    (goto-char (point-min))
;; 	    (org-mode)
;; 	    (org-balance-remove-props)
;; 	    (let ((goal-update-time (fourth regtest)))
;; 	      (org-balance-compute-goal-deltas
;; 	       :intervals (make-elu-intervals :from (second regtest) :n 1 :width
;; 						      (- (third regtest) (second regtest)) :shift 0)))
;; 	    (save-buffer)
;; 	    (if (zerop (call-process "diff" (not 'infile) (not 'destination) (not 'display)
;; 				     "-bBE" test-file ref-file))
;; 		(progn
;; 		  (incf num-ok)
;; 		  (kill-buffer (current-buffer)))
;; 	      (incf num-failed)
;; 	      (message "Failed test on %s" test-file)
;; 	      (show-all)
;; 	      (find-file ref-file)
;; 	      (show-all)
;; 	      (find-file test-file)
;; 	      (ediff-files test-file ref-file))))
;; 	(message "%s tests ok, %s tests failed" num-ok num-failed)))))
	      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'org-balance)

;;; org-balance.el ends here

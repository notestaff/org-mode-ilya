
(require 'rxx)
(require 'elu-valu)

(def-rxx-namespace org
  "Org mode expressions"
  :imports elu-valu
  :exports (priority-char priority-cookie)
  )

;; should this be def-rxx-regexp instead?

;; so, next thing will be:
;; to make a routine that takes a namespace name and regexp name,
;; and returns the fully constructed regexp and its parser.

;; add error checking etc:

;;    -- when doing imports, make sure conflicting symbols are not imported from different namespaces
;;    -- when adding a symbol to a namespace, make sure it does not conflict with ones imported
;;    (or say it will override them?)

;;    -- in an expr, allow specifying a symbol's namespace, as in (from-namespace namespace symbol).
;;    -- in :import, allow specifying particular symbols to import from a namespace, or :except to import everything
;;       except given ones.
;;    

;; it will be called by the top-level functions such as rxx-string-match and rxx-parse etc,
;; but also where currently rxx-symbol is called.

;; what it will do is:

;;   - check if there is a buffer-local non-void setting for this already; if yes, just return that.
;;   - if not, we'll compute the result then store it buffer-locally.
;;   - to compute the result:
;;       - bind rxx-cur-namespace to the namespace
;;       - call rxx-to-string on the global definition of the symbol

;;         when that encounters a symbol and needs to know if the symbol is an aregexp,
;;         it will check rxx-cur-namespace and its imports, and look for the symbol there.

;;    - if namespace is not specified, get the scoped-in one (error if not there).
;;

;
; afterward, replace all uses of advice with elu-flet.
;

; so, this way, the expressions do not refer to each other -- they're just symbols.
; now, when we need to interpret them, at that point we do that by constructing symbol names
; according to the standard convention.
; so the binding is as late as possible.
; and, 

(def-rxx-regexps org
  (priority-char "A priority character.  Parsed as the priority char."
		 (eval-regexp (format "[%c-%c]" org-highest-priority org-lowest-priority)))

  (priority-cookie "A priority cookie.  Parsed as the priority char."
		   (seq "[#" priority-char "]") priority-char)


  (matcher "A tags-and-properties matcher.  Parses as the corresponding form."
  (seq tags-and-props-matcher? (opt "/" todo-matcher))
  `(and ,tags-and-props-matcher ,todo-matcher))

  (tags-and-props-matcher "Tags and properties matcher."
			  (1+ alnum))

  (todo-matcher "Todo matcher."
		(1+ alnum))
  )

;; so, one solution is to have the module explicitly define a const containing the exported regexps.
;; that's not such a bad solution, and is what i had before.

;; so, in the same clause where it says import, we can say what we export.
;; now, this is not quite ideal as import is implementation detail and export is the interface.
;; but this would definitely be usable and probably safe.

;; now, otherwise, where will we put things?
;; suppose we do def-rxx-namespace as eval-and-compile.

;; another could be to use defvar.  

;; (defrxx org tag-name "Tag name, parses as tag name" (1+ (any alnum "_@#%")))
;; (defrxx org tags "List of tags.  Parses as list of tags."  (& blanks? (opt ":" (1+ (& tag-name ":")) eol)) tag-name-list)

;; (defrxx org stars "Stars at start of headline.  Parses as the level, taking odd-only into account."
;;   (seq bol (0+ (named-grp star "*") :sep-by (if org-odd-levels-only "*" "")))
;;   (length star-list))

;; (defrxx org todo "A todo keyword" (eval-words org-todo-keywords-1))

;; (defrxx org headline-text "Headline text" (1+ nonl))
;; (defrxx org stats-cookie "statistics cookie" (seq "[" alnum "/" alnum "]"))

;; (defrxx org headline (sep-by blanks stars priority? todo? headline-text stats-cookie? tags?) 'parse-as-struct)

;; ;; special support for multi-line regexps?
			     
;; (defrxxnamespace org-balance :import (org elu-valu))
;; (defrxx org-balance goal-headline (replace-grp headline todo (eval-words org-balance-goal-keywords)) 'parse-as-struct)

;; ;; allow (defrxx org-balance (def1) (def2))

;; (defrxx org-balance link "An Org link. Parsed as an elu-loc at the target of the link."
;;   (named-grp link (eval-regexp (rxx-make-shy org-any-link-re)))
;;   (elu-save excursion restriction window-excursion match-data
;;     (goto-char (rxx-match-beginning 'link))
;;     (org-balance-open-at-point)
;;     (point-elu-loc)))

;; so then, need to implement get-symbol that takes a namespace and a name in it,
;; as well as, separately, that just takes a name and uses the current scoped-in namespace.

;; 


(rxx-parse-string org matcher "privet/lunatikam")
	

;;; test-org-matcher.el

;; Copyright (c) ߚ Ilya Shlyakhter
;; Authors: Ilya Shlyakhter

;; Released under the GNU General Public License version 3
;; see: http://www.gnu.org/licenses/gpl-3.0.html

;;;; Comments:

;; Template test file for test of Org mode tags/properties
;; matching facilities.

(ert-deftest test-org-matcher/org-map-1 ()
  "Ensure properties accumulate in subtree drawers."
  (org-test-in-example-file "org-matcher.org"
    (let ((tests '(("-atag" ("top" "entry A" "basic matching"))
		   ("+atag" ("code block"))
		   ("a>5" ("entry A")))))
      (mapc
       (lambda (a-test)
	 (should
	  (equal
	   (second a-test)
	   (mapcar
	    (lambda (heading)
	      (set-text-properties 0 (length heading) nil heading)
	      heading)
	    (org-map-entries (lambda () (org-get-heading 'no-tags)) (first a-test))))))
       tests))))

(provide 'test-org-matcher)






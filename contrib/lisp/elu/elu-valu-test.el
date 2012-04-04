(require 'elu-valu)
(require 'ert)
(eval-when-compile (require 'cl))


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


(ert-deftest elu-valu-test-suite ()
  "Test elu-valu"
  (dolist (td elu-valu-parse-test-defs)
    (let* ((regexp-name (first td)) (test-str (second td)) (test-result (third td))
	   (result-now (rxx-parse-string-func 'elu-valu regexp-name test-str)))
      (message "regexp %s test-str %s result-old %s result-now %s"
	       regexp-name test-str test-result result-now)
      (should (equal result-now test-result)))))



(in-package :qvm-app-ng-tests)

(deftest test-handle-run-program ()
  ;; This test demonstrates calling a handler directly, rather than through the HTTP interface.
  (is (equalp (yason:parse (qvm-app-ng::handle-run-program
                            :allocation-method "native"
                            :simulation-method "pure-state"
                            :compiled-quil "DECLARE ro BIT; X 0; MEASURE 0 ro[0]"
			    :addresses (alexandria:plist-hash-table '("ro" t))))
              (alexandria:plist-hash-table '("ro" ((1))) :test 'equal))))

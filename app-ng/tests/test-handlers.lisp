(in-package #:qvm-app-ng-tests)

(deftest test-handle-run-program ()
  ;; This test demonstrates calling a handler directly, rather than through the HTTP interface.
  (let ((response (qvm-app-ng::handle-run-program
                   :allocation-method "native"
                   :simulation-method "pure-state"
                   :compiled-quil "DECLARE ro BIT; X 0; MEASURE 0 ro[0]"
                   :addresses (alexandria:plist-hash-table '("ro" t))))
        (expected (alexandria:plist-hash-table '("ro" ((1))) :test 'equal)))
    (is (typep response 'qvm-app-ng::json-response))
    (is (= qvm-app-ng::+http-ok+ (qvm-app-ng::response-status response)))
    (is (equalp expected (qvm-app-ng::response-data response)))))

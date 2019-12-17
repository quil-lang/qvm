(in-package #:qvm-app-ng-tests)

(deftest test-valid-uuid-string-p ()
  (is (qvm-app-ng::valid-uuid-string-p "fa8acf54-201d-4eef-8777-de4c5057aac9"))
  (is (qvm-app-ng::valid-uuid-string-p "fa8acf54-201d-4eef-9777-de4c5057aac9"))
  (is (qvm-app-ng::valid-uuid-string-p "fa8acf54-201d-4eef-a777-de4c5057aac9"))
  (is (qvm-app-ng::valid-uuid-string-p "fa8acf54-201d-4eef-b777-de4c5057aac9"))
  (is (qvm-app-ng::valid-uuid-string-p (qvm-app-ng::make-uuid-string)))

  ;; uppercase rejected
  (is (not (qvm-app-ng::valid-uuid-string-p "FA8ACF54-201D-4EEF-9777-DE4C5057AAC9")))

  ;; invalid version field
  (is (not (qvm-app-ng::valid-uuid-string-p "fa8acf54-201d-5eef-9777-de4c5057aac9")))

  ;; invalid clock sequence high-order bits
  (is (not (qvm-app-ng::valid-uuid-string-p "fa8acf54-201d-4eef-7777-de4c5057aac9"))))

(deftest test-parse-simualtion-method ()
  (is (eq 'qvm-app-ng::pure-state (qvm-app-ng::parse-simulation-method "pure-state")))
  (is (eq 'qvm-app-ng::full-density-matrix (qvm-app-ng::parse-simulation-method "full-density-matrix")))
  (is (eq 'qvm-app-ng::pure-state (qvm-app-ng::parse-simulation-method "PURE-STATE")))
  (is (eq 'qvm-app-ng::full-density-matrix (qvm-app-ng::parse-simulation-method "FULL-DENSITY-MATRIX")))
  (signals qvm-app-ng::user-input-error
    (qvm-app-ng::parse-simulation-method ""))
  (signals qvm-app-ng::user-input-error
    (qvm-app-ng::parse-simulation-method "parse-simulation-method"))
  (signals qvm-app-ng::user-input-error
    (qvm-app-ng::parse-simulation-method "cons"))
  (signals qvm-app-ng::user-input-error
    (qvm-app-ng::parse-simulation-method "pure-states")))

(deftest test-parse-allocation-method ()
  (is (eq 'qvm-app-ng::native (qvm-app-ng::parse-allocation-method "native")))
  (is (eq 'qvm-app-ng::foreign (qvm-app-ng::parse-allocation-method "foreign")))
  (is (eq 'qvm-app-ng::native (qvm-app-ng::parse-allocation-method "NATIVE")))
  (is (eq 'qvm-app-ng::foreign (qvm-app-ng::parse-allocation-method "FOREIGN")))
  (signals qvm-app-ng::user-input-error
    (qvm-app-ng::parse-allocation-method ""))
  (signals qvm-app-ng::user-input-error
    (qvm-app-ng::parse-allocation-method "parse-allocation-method"))
  (signals qvm-app-ng::user-input-error
    (qvm-app-ng::parse-allocation-method "most-positive-fixnum"))
  (signals qvm-app-ng::user-input-error
    (qvm-app-ng::parse-allocation-method "natives")))

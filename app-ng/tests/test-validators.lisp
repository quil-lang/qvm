(in-package :qvm-app-ng-tests)

(deftest test-valid-persistent-qvm-token-p ()
  (is (qvm-app-ng::valid-persistent-qvm-token-p "fa8acf54-201d-4eef-8777-de4c5057aac9"))
  (is (qvm-app-ng::valid-persistent-qvm-token-p "fa8acf54-201d-4eef-9777-de4c5057aac9"))
  (is (qvm-app-ng::valid-persistent-qvm-token-p "fa8acf54-201d-4eef-a777-de4c5057aac9"))
  (is (qvm-app-ng::valid-persistent-qvm-token-p "fa8acf54-201d-4eef-b777-de4c5057aac9"))
  (is (qvm-app-ng::valid-persistent-qvm-token-p (qvm-app-ng::make-persistent-qvm-token)))

  ;; uppercase rejected
  (is (not (qvm-app-ng::valid-persistent-qvm-token-p "FA8ACF54-201D-4EEF-9777-DE4C5057AAC9")))

  ;; invalid version field
  (is (not (qvm-app-ng::valid-persistent-qvm-token-p "fa8acf54-201d-5eef-9777-de4c5057aac9")))

  ;; invalid clock sequence high-order bits
  (is (not (qvm-app-ng::valid-persistent-qvm-token-p "fa8acf54-201d-4eef-7777-de4c5057aac9"))))

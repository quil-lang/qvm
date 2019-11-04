(in-package #:qvm-app-ng-tests)

(defun enter-quietly (argv)
  (qvm-app-ng::%entry-point (append '("--verbose" "0") argv)))

(deftest test-initial-rpc-request-version ()
  (is (string= (with-output-to-string (*standard-output*)
                 (qvm-app-ng::show-version))
               (with-output-to-string (*standard-output*)
                 (enter-quietly '("--rpc-request" "{\"type\": \"version\"}"))))))

(deftest test-initial-rpc-create-qvm ()
  (unwind-protect
       (extract-and-validate-token
        (with-output-to-string (*standard-output*)
          (enter-quietly `("--rpc-request" ,(plist->json '(:type "create-qvm"
                                                           :num-qubits 20
                                                           :simulation-method "pure-state"
                                                           :allocation-method "native"))))))
    (qvm-app-ng::reset-persistent-qvms-db)))

(deftest test-show-version ()
  (dolist (flag '("-v" "--version"))
    (is (string= (with-output-to-string (*standard-output*)
                   (qvm-app-ng::show-version))
                 (with-output-to-string (*standard-output*)
                   (enter-quietly (list flag)))))))

(deftest test-show-help ()
  (dolist (flag '("-h" "--help"))
    (is (string= (with-output-to-string (*standard-output*)
                   (qvm-app-ng::show-help))
                 (with-output-to-string (*standard-output*)
                   (enter-quietly (list flag)))))))

(in-package :qvm-app-ng-tests)

(defun %make-argv (option-names values)
  (mapcan (lambda (name value)
            (list (format nil "-~:[-~;~]~(~A~)" (characterp name) name)
                  (format nil "~A" value)))
          (alexandria:ensure-list option-names)
          (alexandria:ensure-list values)))

(defun %make-config-file (option-names values)
  (with-output-to-string (*standard-output*)
    (prin1 (mapcan #'list (alexandria:ensure-list option-names) (alexandria:ensure-list values)))))

(defun %make-random-tmp-path (&optional directory-p)
  (merge-pathnames (format nil "~A~:[~;/~]" (uuid:make-v4-uuid) directory-p)
                   (uiop:default-temporary-directory)))

(defun %ensure-non-existent (path-or-namestring)
  (cond ((or (uiop:file-exists-p path-or-namestring)
             (uiop:directory-exists-p path-or-namestring))
         (warn "Warning: the path ~A actually exists. Returning NIL." path-or-namestring)
         nil)
        (t path-or-namestring)))

(defun %do-validator-test-valid (option-name input)
  (not-signals error (load-config (%make-argv option-name input) nil))
  (is (equal (get-config option-name) input))
  (not-signals error (load-config '() (%make-config-file option-name input)))
  (is (equal (get-config option-name) input)))

(defun %do-validator-test-invalid (option-name input)
  (signals error (load-config (%make-argv option-name input) nil))
  (is (null (get-config option-name)))
  (signals error (load-config '() (%make-config-file option-name input)))
  (is (null (get-config option-name))))

(defun %do-validator-test (option-name valid-inputs invalid-inputs)
  (dolist (input (alexandria:ensure-list valid-inputs))
    (%do-validator-test-valid option-name input))
  (dolist (input (alexandria:ensure-list invalid-inputs))
    (%do-validator-test-invalid option-name input)))

(deftest test-config-port-validator ()
  (let ((valid-inputs '(1 80 443 5000 8080 65535))
        (invalid-inputs '("" "http" 0 65536)))
    (%do-validator-test ':port       valid-inputs invalid-inputs)
    (%do-validator-test ':swank-port valid-inputs invalid-inputs)))

(deftest test-config-non-negative-validator ()
  (let ((valid-inputs `(0 1 42 ,most-positive-fixnum))
        (invalid-inputs `(-1 -12 ,most-negative-fixnum)))
    (%do-validator-test ':qubits       valid-inputs invalid-inputs)
    (%do-validator-test ':num-workers  valid-inputs invalid-inputs)
    (%do-validator-test ':qubit-limit  valid-inputs invalid-inputs)
    (%do-validator-test ':memory-limit valid-inputs invalid-inputs)))

(deftest test-config-log-level-validator ()
  (let* ((log-strings (mapcar #'symbol-name qvm-app-ng::+valid-log-levels+))
         (upcase (mapcar #'string-upcase log-strings))
         (downcase (mapcar #'string-downcase log-strings)))
    (%do-validator-test ':log-level
                        (append upcase downcase)
                        '("" "nope" "not-a-valid-log-level" "no way" "17"))))

(deftest test-config-directory-validator ()
  (let ((valid-inputs
          (mapcar #'namestring
                  (remove-if-not #'uiop:directory-exists-p
                                 (list (uiop:default-temporary-directory)
                                       (user-homedir-pathname)))))
        (invalid-inputs (%ensure-non-existent (namestring (%make-random-tmp-path t)))))
    (%do-validator-test ':safe-include-directory valid-inputs invalid-inputs)))

(deftest test-config-allocation-method-validator ()
  (%do-validator-test ':allocation-method
                      qvm-app-ng::*available-allocation-kinds*
                      '("" "gimme-all-the-memory" "/dev/null" 0)))

(deftest test-config-simulation-method-validator ()
  (%do-validator-test ':simulation-method
                      qvm-app-ng::*available-simulation-methods*
                      '("" "invalid" "purse-state oh no" 12))
  (signals error (load-config '() "(:simulation-method nil)")))

(deftest test-config-invalid-options ()
  (signals error (load-config '("--bogus" "--bogus-with-arg" "hey") nil))
  (signals error (load-config '("--bogus-with-arg" "hey") nil))
  (signals error (load-config '() "(:bogus t)"))
  (signals error (load-config '() "(:bogus-with-arg \"hey\")")))

(deftest test-config-empty-options ()
  (not-signals error (load-config '() nil))
  (not-signals error (load-config '() ""))
  (not-signals error (load-config '() "()")))

(deftest test-config-file-accepts-keywords-or-symbols ()
  (not-signals error (load-config '() "(:qubits 2)"))
  (is (= 2 (get-config ':qubits)))
  (not-signals error (load-config '() "(qubits 3)"))
  (is (= 3 (get-config ':qubits))))

(deftest test-config-file-read-from-path ()
  (uiop:with-temporary-file (:stream stream :pathname path)
    (format stream "(:server t :qubit-limit 8)")
    (force-output stream)
    :CLOSE-STREAM
    (not-signals error (load-config '() path))
    (is (eq t (qvm-app-ng.config:get-config ':server)))
    (is (= 8 (qvm-app-ng.config:get-config ':qubit-limit)))))

(deftest test-config-malformed-config-file ()
  (signals error (load-config '() "("))
  (signals error (load-config '() "(())"))
  (signals error (load-config '() ":verbose"))
  (signals error (load-config '() ":verbose t"))
  ;; Booleans must specify an explicit T or NIL
  (signals error (load-config '() "(:verbose)"))
  (signals error (load-config '() "(:verbose t :host)")))

(deftest test-config-boolean-options ()
  (not-signals error (load-config '("--verbose") nil))
  (is (eq t (qvm-app-ng.config:get-config ':verbose)))
  (not-signals error (load-config '("--verbose=no") nil))
  (is (null (qvm-app-ng.config:get-config ':verbose)))
  (not-signals error (load-config '() "(:verbose t)"))
  (is (eq t (qvm-app-ng.config:get-config ':verbose)))
  (not-signals error (load-config '() "(:verbose nil)"))
  (is (null (get-config ':verbose))))

(deftest test-config-file-missing ()
  "Test that a missing config file raises an error."
  (alexandria:if-let ((no-such-file (%ensure-non-existent (namestring (%make-random-tmp-path)))))
    (signals error (load-config '() no-such-file))))

(deftest test-config-file-type-checks ()
  "Test that options loaded from config files enforce the option spec :TYPE."
  (let ((qvm-app-ng.config::*option-spec* `((("verbose") :type boolean :optional t)
                                            (("integer") :type integer :optional t)
                                            (("string")  :type string :optional t))))
    (not-signals error (load-config '() "(:verbose t)"))
    (not-signals error (load-config '() "(:verbose nil)"))
    (not-signals error (load-config '() "(:integer 42)"))
    (not-signals error (load-config '() "(:string \"hello\")"))
    (signals error (load-config '() "(:verbose :t)"))
    (signals error (load-config '() "(:verbose :nil)"))
    (signals error (load-config '() "(:verbose 4)"))
    (signals error (load-config '() "(:integer t)"))
    (signals error (load-config '() "(:integer \"4\")"))
    (signals error (load-config '() "(:integer 4.0)"))
    (signals error (load-config '() "(:string  5)"))
    (signals error (load-config '() "(:string  t)"))))

(deftest test-config-defaults ()
  (let ((option-spec `((("host")
                        :type string
                        :optional t
                        :initial-value "0.0.0.0"
                        :documentation "some doc")
                       (("simulation-method")
                        :type string
                        :optional t
                        :initial-value "pure-state"
                        :validator ,(lambda (name input)
                                      (declare (ignore name input))))
                       (("check-sdk-version")
                        :type boolean
                        :initial-value nil)
                       (("no-default-int")
                        :type integer)
                       (("default-int")
                        :type integer
                        :initial-value 42))))
    (not-signals error (load-config '() "" option-spec))
    (is (string= "0.0.0.0" (get-config ':host)))
    (is (string= "pure-state" (get-config ':simulation-method)))
    (is (null (get-config ':check-sdk-version)))
    (is (null (get-config ':no-default-int)))
    (is (= 42 (get-config ':default-int)))))

(deftest test-config-overrides ()
  "Test that option priority is command line > config file > defaults."
  ;; Default value
  (not-signals error (load-config '() nil))
  (is (= 5000 (get-config ':port)))
  ;; Command line overrides default
  (not-signals error (load-config '("--port" "6000") nil))
  (is (= 6000 (get-config ':port)))
  ;; Config file overrides default
  (not-signals error (load-config '() "(:port 7000)"))
  (is (= 7000 (get-config ':port)))
  ;; Command line overrides config file
  (not-signals error (load-config '("--port" "8000") "(:port 7000)"))
  (is (= 8000 (get-config ':port))))

(deftest test-config-file-specified-on-command-line ()
  "Test support for passing the config file location as a command-line argument."
  ;; TODO: support passing config file location via --config-file command line option.
  (skip))

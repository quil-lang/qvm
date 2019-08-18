(in-package :qvm-app-ng-tests)

;;; Stolen from CL-QUIL-TESTS
(defun join-strings (strings &key (delimiter #\Newline) prefix-p suffix-p)
  "Join a sequence of STRINGS on the character or string DELIMITER.

If PREFIX-P is non-nil, prefix the returned string with DELIMITER.

If SUFFIX-P is non-nil, suffix the returned string with DELIMITER."
  (check-type strings quil::string-sequence)
  (check-type delimiter (or string character))
  (with-output-to-string (stream)
    (loop :with last := (length strings)
          :with delimiter-string := (string delimiter)
            :initially (and prefix-p (write-string delimiter-string stream))
          :for str :in (coerce strings 'list)
          :for i :upfrom 1
          :do (write-string str stream)
          :when (or (< i last) suffix-p)
            :do (write-string delimiter-string stream))))

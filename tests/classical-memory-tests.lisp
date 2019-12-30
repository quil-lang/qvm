;;;; tests/classical-memory-tests.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-tests)

(deftest test-bit-integer-aliasing ()
  "Test that setting bits of aliased memory affects the integer representation of the number."
  (let* ((memories (qvm::allocate-memory-for-model
                    (qvm:memory-descriptors-to-qvm-memory-model
                     (quil:parsed-program-memory-definitions
                      (quil:parse-quil "
DECLARE ro BIT[64]
DECLARE beta INTEGER SHARING ro")))))
         (ro (gethash "ro" memories))
         (beta (gethash "beta" memories)))
    (setf (qvm::memory-view-ref beta 0) #b101011)
    (is (equal '(1 1 0 1 0 1) (loop :for i :below 6 :collect (qvm::memory-view-ref ro i))))))

(deftest test-load-out-of-range ()
  "Test that we detect an error when a bit is out of range with a LOAD or STORE."
  (let* ((p (quil:parse-quil "
DECLARE idx INTEGER
DECLARE z REAL[4]
DECLARE target REAL

MOVE idx 0
LOAD target z idx   # OK!

MOVE idx 5
LOAD target z idx   # NOT OK!
"))
         (q (make-qvm 1 :classical-memory-model (qvm:memory-descriptors-to-qvm-memory-model
                                                 (quil:parsed-program-memory-definitions p)))))
    (qvm:load-program q p)
    (signals qvm:memory-index-out-of-bounds
      (qvm:run q))))

(deftest test-measure-out-of-range ()
  "Test that we detect an error when a bit is out of range with a MEASURE."
  (let* ((p (quil:parse-quil "
DECLARE ro BIT[1]
X 0
X 1
MEASURE 0 ro[0]
MEASURE 1 ro[1]
"))
         (q (make-qvm 2 :classical-memory-model (qvm:memory-descriptors-to-qvm-memory-model
                                                 (quil:parsed-program-memory-definitions p)))))
    (qvm:load-program q p)
    (signals qvm:memory-index-out-of-bounds
      (qvm:run q))))

(deftest test-bit-offsetting-bit ()
  (let* ((p (quil:parse-quil "
DECLARE mem BIT[1024]     # 128 octets
DECLARE b0 BIT SHARING mem
DECLARE b1 BIT SHARING mem OFFSET 1 BIT
DECLARE b2 BIT SHARING mem OFFSET 2 BIT
DECLARE b3 BIT SHARING mem OFFSET 3 BIT
DECLARE b4 BIT SHARING mem OFFSET 4 BIT
DECLARE b5 BIT SHARING mem OFFSET 5 BIT
DECLARE b6 BIT SHARING mem OFFSET 6 BIT
DECLARE b7 BIT SHARING mem OFFSET 7 BIT
DECLARE b8 BIT SHARING mem OFFSET 8 BIT

DECLARE b80 BIT SHARING mem OFFSET 10 OCTET 1 BIT

MOVE b0 1
MOVE b1 1
MOVE b2 1
MOVE b3 1
MOVE b4 1
MOVE b5 1
MOVE b6 1
MOVE b7 1
MOVE b8 1

MOVE b80 1
"))
         (q (make-qvm 1 :classical-memory-model (qvm:memory-descriptors-to-qvm-memory-model
                                                 (quil:parsed-program-memory-definitions p)))))
    (qvm:load-program q p)
    (qvm:run q)
    (qvm::memory-view-root-classical-memory (gethash "mem" (qvm::classical-memories q)))
    (dotimes (i 9)
      (is (= 1 (qvm::memory-ref q "mem" i)))
      (is (= 1 (qvm::memory-ref q (format nil "b~D" i) 0))))
    (is (= 1 (qvm::memory-ref q "mem" 81)))
    (is (= 1 (qvm::memory-ref q "b80" 0)))))

(deftest test-bit-offsetting-octet ()
  (let* ((p (quil:parse-quil "
DECLARE mem OCTET[128]

DECLARE o0 OCTET SHARING mem
DECLARE o2 OCTET SHARING mem OFFSET 1 OCTET 8 BIT

MOVE o0 255
MOVE o2 255
"))
         (q (make-qvm 1 :classical-memory-model (qvm:memory-descriptors-to-qvm-memory-model
                                                 (quil:parsed-program-memory-definitions p)))))
    (qvm:load-program q p)
    (qvm:run q)
    (is (= 255 (qvm::memory-ref q "mem" 0)))
    (is (= 255 (qvm::memory-ref q "o0" 0)))
    (is (= 255 (qvm::memory-ref q "mem" 2)))
    (is (= 255 (qvm::memory-ref q "o2" 0)))))

(deftest test-quake-style-square-root ()
  "Test the good old-fashioned \"fast inverse square root\", except we compute the square root proper."
  (let ((p (quil:parse-quil "
DECLARE input REAL
DECLARE output REAL
DECLARE output-i INTEGER SHARING output
DECLARE tmp REAL

MOVE output input
DIV output-i 2
NEG output-i
ADD output-i 6910469410427058089

MOVE tmp output
MUL tmp tmp
MUL tmp input
MUL tmp 0.5
NEG tmp
ADD tmp 1.5
MUL tmp output
MOVE output tmp

MOVE tmp 1
DIV tmp output
MOVE output tmp
")))
    (dotimes (i 10)
      (let ((q (make-qvm 1 :classical-memory-model (qvm:memory-descriptors-to-qvm-memory-model
                                                    (quil:parsed-program-memory-definitions p)))))
        (qvm:load-program q p)
        (setf (qvm:memory-ref q "input" 0) (expt (flonum i) 2))
        (qvm:run q)
        (is (= i (floor (qvm:memory-ref q "output" 0))))))))

(deftest test-indirect-comparators ()
  "Test that comparators actually execute and produce at least somewhat reliable results."
  (let ((p (quil:parse-quil "
DECLARE bx BIT[2]
DECLARE rx REAL[2]
DECLARE ix INTEGER[2]
DECLARE ox OCTET[2]

DECLARE lt BIT[4]
DECLARE le BIT[4]
DECLARE eq BIT[4]
DECLARE ge BIT[4]
DECLARE gt BIT[4]

LT lt[0] bx[0] bx[1]
LT lt[1] rx[0] rx[1]
LT lt[2] ix[0] ix[1]
LT lt[3] ox[0] ox[1]

LE le[0] bx[0] bx[1]
LE le[1] rx[0] rx[1]
LE le[2] ix[0] ix[1]
LE le[3] ox[0] ox[1]

EQ eq[0] bx[0] bx[1]
EQ eq[1] rx[0] rx[1]
EQ eq[2] ix[0] ix[1]
EQ eq[3] ox[0] ox[1]

GE ge[0] bx[0] bx[1]
GE ge[1] rx[0] rx[1]
GE ge[2] ix[0] ix[1]
GE ge[3] ox[0] ox[1]

GT gt[0] bx[0] bx[1]
GT gt[1] rx[0] rx[1]
GT gt[2] ix[0] ix[1]
GT gt[3] ox[0] ox[1]
"))
        (q (make-qvm 1)))
    (flet ((init-memorando (bx0 bx1
                            rx0 rx1
                            ix0 ix1
                            ox0 ox1)
             (qvm:load-program q p :supersede-memory-subsystem t)
             (setf (qvm:memory-ref q "bx" 0) bx0
                   (qvm:memory-ref q "bx" 1) bx1
                   (qvm:memory-ref q "rx" 0) rx0
                   (qvm:memory-ref q "rx" 1) rx1
                   (qvm:memory-ref q "ix" 0) ix0
                   (qvm:memory-ref q "ix" 1) ix1
                   (qvm:memory-ref q "ox" 0) ox0
                   (qvm:memory-ref q "ox" 1) ox1)
             nil)
           (check-results (blt ble beq bge bgt
                           rlt rle req rge rgt
                           ilt ile ieq ige igt
                           olt ole oeq oge ogt)
             (qvm:run q)
             ;; Check everything ugh.
             (is (= blt (qvm:memory-ref q "lt" 0)))
             (is (= ble (qvm:memory-ref q "le" 0)))
             (is (= beq (qvm:memory-ref q "eq" 0)))
             (is (= bge (qvm:memory-ref q "ge" 0)))
             (is (= bgt (qvm:memory-ref q "gt" 0)))

             (is (= rlt (qvm:memory-ref q "lt" 1)))
             (is (= rle (qvm:memory-ref q "le" 1)))
             (is (= req (qvm:memory-ref q "eq" 1)))
             (is (= rge (qvm:memory-ref q "ge" 1)))
             (is (= rgt (qvm:memory-ref q "gt" 1)))

             (is (= ilt (qvm:memory-ref q "lt" 2)))
             (is (= ile (qvm:memory-ref q "le" 2)))
             (is (= ieq (qvm:memory-ref q "eq" 2)))
             (is (= ige (qvm:memory-ref q "ge" 2)))
             (is (= igt (qvm:memory-ref q "gt" 2)))

             (is (= olt (qvm:memory-ref q "lt" 3)))
             (is (= ole (qvm:memory-ref q "le" 3)))
             (is (= oeq (qvm:memory-ref q "eq" 3)))
             (is (= oge (qvm:memory-ref q "ge" 3)))
             (is (= ogt (qvm:memory-ref q "gt" 3)))))
      (init-memorando #b0     #b0
                      0.0d0   0.0d0
                      0       0
                      #b0     #b0)
      (check-results 0 1 1 1 0
                     0 1 1 1 0
                     0 1 1 1 0
                     0 1 1 1 0)
      (init-memorando #b0     #b1
                      0.0d0   1.0d0
                      0       1
                      #b0     #b1)
      (check-results 1 1 0 0 0
                     1 1 0 0 0
                     1 1 0 0 0
                     1 1 0 0 0)
      (init-memorando #b1     #b0
                      1.0d0   0.0d0
                      1       0
                      #b1     #b0)
      (check-results 0 0 0 1 1
                     0 0 0 1 1
                     0 0 0 1 1
                     0 0 0 1 1))))

(deftest test-exhange ()
  "Test that EXCHANGE does the job."
  (let* ((p (quil:parse-quil "
DECLARE bx BIT[2]
DECLARE rx REAL[2]
DECLARE ix INTEGER[2]
DECLARE ox OCTET[2]

MOVE bx[0] 0
MOVE bx[1] 1

MOVE rx[0] 0.0
MOVE rx[1] 55.0

MOVE ix[0] 0
MOVE ix[1] 123

MOVE ox[0] 0
MOVE ox[1] 255

EXCHANGE bx[0] bx[1]
EXCHANGE rx[0] rx[1]
EXCHANGE ix[0] ix[1]
EXCHANGE ox[0] ox[1]
"))
         (q (make-qvm 1)))
    (qvm:load-program q p :supersede-memory-subsystem t)
    (qvm:run q)
    (is (zerop (qvm:memory-ref q "bx" 1)))
    (is (zerop (qvm:memory-ref q "rx" 1)))
    (is (zerop (qvm:memory-ref q "ix" 1)))
    (is (zerop (qvm:memory-ref q "ox" 1)))

    (is (= 1      (qvm:memory-ref q "bx" 0)))
    (is (= 55.0d0 (qvm:memory-ref q "rx" 0)))
    (is (= 123    (qvm:memory-ref q "ix" 0)))
    (is (= 255    (qvm:memory-ref q "ox" 0)))))

(deftest test-memory-name-closure-bug ()
  "This was a bug where the reader and writer closures had their NAME and LENGTH bindings mutated in a loop."
  (let ((valid-quil (quil:parse-quil "
DECLARE ro BIT[4]
DECLARE theta REAL[1]
MOVE theta[0] 0.0
PRAGMA EXPECTED_REWIRING \"#(0 1 2 5 3 4)\"
RX(-pi/2) 0
CZ 1 0
RX(pi) 5
RZ(pi) 0
RZ(-pi/2) 1
RX(-pi/2) 1
CZ 2 1
RZ(-pi/2) 2
RX(-pi/2) 2
CZ 5 2
RZ(pi/2) 2
RZ(-pi/2) 5
RX(-pi/2) 5
RZ(pi/2 + theta[0] + -pi/2) 5
RX(pi/2) 5
CZ 5 2
RZ(pi/2) 2
RX(pi/2) 2
CZ 2 1
RX(pi/2) 1
CZ 0 1
RX(-1.5707963267948968) 0
RZ(1.5707963267948974) 1
RZ(pi/2) 2
RX(pi) 2
RZ(pi/2) 5
RX(pi) 5
PRAGMA CURRENT_REWIRING \"#(0 1 2 5 3 4)\"
PRAGMA EXPECTED_REWIRING \"#(0 1 2 5 3 4)\"
MEASURE 5 ro[3]
MEASURE 2 ro[2]
MEASURE 1 ro[1]
MEASURE 0 ro[0]
PRAGMA CURRENT_REWIRING \"#(0 1 2 5 3 4)\"
")))
    (not-signals error (qvm:run-program 6 valid-quil))))

(deftest test-wait-function-poke-memory ()
  (let* ((p (quil:parse-quil "DECLARE hold-up BIT; WAIT"))
         (q (make-instance 'pure-state-qvm
                           :number-of-qubits 1
                           :classical-memory-subsystem
                           (make-instance 'classical-memory-subsystem
                                          :classical-memory-model
                                          (qvm:memory-descriptors-to-qvm-memory-model
                                           (quil:parsed-program-memory-definitions p)))
                           :wait-function
                           (lambda (qvm)
                             (setf (qvm:memory-ref qvm "hold-up" 0) 1)))))
    (is (= 0 (qvm::memory-ref q "hold-up" 0)))
    (qvm:load-program q p)
    (qvm:run q)
    (is (= 1 (qvm::memory-ref q "hold-up" 0)))))

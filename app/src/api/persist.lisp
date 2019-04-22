;;;; persist.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:qvm-app)

(defun |GET-persist| (request)
  ;; Return an array of all of the client's valid persistent keys.
  (yason:with-output-to-string* ()
    (yason:with-array ()
      (mapc #'yason:encode-array-element
            (lookup-persistent-keys-for-client
             (tbnl:real-remote-addr request))))))

(defun |GET-persist/id| (request args)
  ;; Get info about an allocated QVM. (This replaces the old SHM
  ;; socket/server.)
  (let* ((key (cdr (assoc "id" args :test #'string=)))
         (record (lookup-persistent-qvm-for-ip key (tbnl:real-remote-addr request))))
    (format-log :debug "Getting info for ~A" record)
    ;; for shared memory:
    ;;
    ;;     name
    ;;     length
    ;;     offset
    ;;
    ;; * ~ -- ~ TODO ~ -- ~ *
))

(defun shared-allocation-descriptor-p (string)
  (and (stringp string)
       (cl-ppcre:scan "shared:(?:[A-Za-z0-9]|-|_)+" string)))

(defun shared-allocation-descriptor-name (string)
  (subseq string (length "shared:")))

(defun |POST-persist| (request)
  ;; need num-qubits, allocation, qvm-type
  (let ((new-key (generate-persistent-qvm-key))
        (js      (extract-json-payload request)))
    (check-required-fields js "num-qubits" "allocation" "simulation-method")
    (let* ((simulation-method (string-to-simulation-method (gethash "simulation-method" js)))
           (num-qubits        (gethash "num-qubits" js))
           (allocation        (gethash "allocation" js)))
      (check-type num-qubits unsigned-byte)
      ;; We need an actual allocation description.
      (cond
        ;; Not present = default.
        ((null allocation)
         (setf allocation **default-allocation**))

        ;; Either native or foreign.
        ((member allocation *available-allocation-kinds* :test #'string=)
         (setf allocation (allocation-description-maker allocation)))

        ;; Shared memory. Descriptor will be "shared:<name>".
        ((shared-allocation-descriptor-p allocation)
         (let ((name (shared-allocation-descriptor-name allocation)))
           (setf allocation (lambda (length)
                              (make-instance 'qvm:posix-shared-memory-allocation
                                             :length length
                                             :name name)))))

        ;; Something invalid from the user..
        (t
         (error "Invalid allocation type: ~S" allocation)))

      ;; At this point, ALLOCATION will be bound to a function which
      ;; takes a length and returns an allocator description used to
      ;; actually allocate memory. We need to determine the length of
      ;; the allocatiuon now.
      (ecase simulation-method
        ((pure-state pauli-pure-state)
         (setf allocation (funcall allocation (expt 2 num-qubits))))
        ((density)
         (setf allocation (funcall allocation (expt 2 (* 2 num-qubits))))))
      
      ;; At this point, ALLOCATION will be bound to a class describing
      ;; what, where, and how much to allocate.
      ;;
      ;; Now we need to actually allocate the QVM.
      (multiple-value-bind (vec finalizer) (qvm:allocate-vector allocation)
        ;; It just so happens that the pure zero state in the
        ;; density matrix formalism is the same as the pure zero state
        ;; in the state-vector formalism).
        (setf (aref vec 0) (cflonum 1))
        (let ((qvm (ecase simulation-method
                     ;; We are duplicating functionality of MAKE-QVM
                     ;; and its brethren here. Maybe this is a hint
                     ;; that we ought to make MAKE-INSTANCE a nicer
                     ;; interface?
                     (pure-state
                      (make-instance
                       'qvm:pure-state-qvm
                       :number-of-qubits num-qubits
                       :amplitudes vec
                       :classical-memory-subsystem
                       (make-instance 'qvm:classical-memory-subsystem
                                      :classical-memory-model quil:**empty-memory-model**)))
                     (pauli-pure-state
                      (make-instance
                       'qvm:depolarizing-qvm
                       :number-of-qubits num-qubits
                       :amplitudes vec
                       :classical-memory-subsystem
                       (make-instance 'qvm:classical-memory-subsystem
                                      :classical-memory-model quil:**empty-memory-model**)))
                     (stochastic-pure-state
                      (make-instance
                       'qvm:noisy-qvm
                       :number-of-qubits num-qubits
                       :amplitudes vec
                       :classical-memory-subsystem
                       (make-instance 'qvm:classical-memory-subsystem
                                      :classical-memory-model quil:**empty-memory-model**)))
                     (density
                      (make-instance 'qvm:density-qvm
                                     :number-of-qubits num-qubits
                                     :amplitudes vec
                                     :classical-memory-subsystem
                                     (make-instance 'qvm:classical-memory-subsystem
                                                    :classical-memory-model quil:**empty-memory-model**))))))
          ;; Make sure we pin a deallocation finalizer on the QVM.
          (tg:finalize qvm finalizer)
          ;; Now actually store the persistent record.
          (setf (lookup-persistent-qvm new-key)
                (make-persistent-record
                 :client-ip (tbnl:real-remote-addr request)
                 :simulation-method simulation-method
                 :allocation allocation
                 :qvm qvm))))
      (yason-encode-to-string new-key))))

(defun |DELETE-persist/id| (request args)
  ;; Delete a client's ID.
  (let* ((key (cdr (assoc "id" args :test #'string=)))
         (record (lookup-persistent-qvm-for-ip key (tbnl:real-remote-addr request))))
    (format-log :debug "deleting ~S" record)
    (remhash key **persistent-qvms**)
    ;; return the key
    (yason-encode-to-string key)))

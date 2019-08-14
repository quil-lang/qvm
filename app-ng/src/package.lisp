;;;; app-ng/src/package.lisp
;;;;
;;;; Author: appleby

(defpackage #:qvm-app-ng.config
  (:use #:cl #:qvm)
  (:export
   #:handle-config
   #:load-config
   #:get-config
   #:show-option-help))

(defpackage #:qvm-app-ng
  (:use #:cl #:qvm #:qvm-app-ng.config))

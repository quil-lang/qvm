;;;; app-ng/src/http-status.lisp
;;;;
;;;; author: appleby
;;;;
;;;; This file provides a wafer-thin abstraction over HUNCHENTOOT's http-status response codes.
;;;; More importantly, it also makes me feel warm-and-fuzzy for making an attempt to remain
;;;; http-server-backend agnostic.
(in-package #:qvm-app-ng)

(deftype http-status () '(integer 100 599))

(macrolet ((define-tbnl-derived-constant (status-symbol)
             (check-type status-symbol symbol)
             (let* ((tbnl-symbol (find-symbol (symbol-name status-symbol) 'tbnl))
                    (tbnl-documentation (documentation tbnl-symbol 'variable)))
               (assert (not (null tbnl-symbol)))
               `(defconstant ,status-symbol ,tbnl-symbol
                  ,@(alexandria:ensure-list tbnl-documentation)))))

  (define-tbnl-derived-constant +http-ok+)
  (define-tbnl-derived-constant +http-bad-request+)
  (define-tbnl-derived-constant +http-internal-server-error+))

(defun http-status-string (http-status)
  "Return a human-readable status string for HTTP-STATUS."
  (check-type http-status http-status)
  (tbnl:reason-phrase http-status))

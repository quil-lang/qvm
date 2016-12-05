(in-package #:qvm)

#+unix
(progn
  (include "unistd.h")
  (constant ($sc-nprocessors-onln "_SC_NPROCESSORS_ONLN") :optional nil))

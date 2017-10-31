(in-package #:qvm)

#+unix
(progn
  ;; Includes - POSIX
  (include "sys/mman.h")
  (include "sys/stat.h")
  (include "stddef.h")

  ;; Types
  (ctype size_t "size_t")
  (ctype mode_t "mode_t")

  ;; Constants/Enums
  (constant ($map-failed "MAP_FAILED") :optional nil))

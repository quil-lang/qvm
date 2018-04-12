(in-package #:qvm)

#+unix
(progn
  ;; Includes - POSIX
  (include "sys/mman.h")
  (include "sys/stat.h")
  (include "stddef.h")
  (include "fcntl.h")

  ;; Types
  (ctype size_t "size_t")
  (ctype mode_t "mode_t")
  (ctype off_t "off_t")

  ;; Constants/Enums
  (constant ($map-failed "MAP_FAILED") :optional nil)
  (constant ($map-shared "MAP_SHARED") :optional nil)

  (constant ($prot-read "PROT_READ") :optional nil)
  (constant ($prot-write "PROT_WRITE") :optional nil)

  (constant ($o-creat "O_CREAT") :optional nil)
  (constant ($o-excl "O_EXCL") :optional nil)
  (constant ($o-rdwr "O_RDWR") :optional nil)

  ;; Variables
  (cvar ("errno" %errno) :int))

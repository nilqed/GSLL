;; Permissible types
;; Liam Healy 2008-12-31 21:06:34EST types.lisp
;; Time-stamp: <2008-12-31 21:52:33EST types.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Types for CFFI (will eventually be in CFFI)
;;;;****************************************************************************

(defvar *sizet-type* nil
  "The CL type for size_t.")

(case
  (cffi:foreign-type-size :long)
  (8
   (push :int64 *features*)
   (setf *sizet-type* '(unsigned-byte 64))
   (cffi:defctype sizet :uint64))
  (4
   (push :int32 *features*)
   (setf *sizet-type* '(unsigned-byte 32))
   (cffi:defctype sizet :uint32))
  (t (error "Size of :long unrecognized")))

;; cffi-features:no-long-long doesn't work for me, but ought to be checked? 
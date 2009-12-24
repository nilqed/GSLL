;; Number types used by GSL functions, and specification conversion
;; Liam Healy 2008-12-31 21:06:34EST types.lisp
;; Time-stamp: <2009-12-23 23:00:53EST types.lisp>
;; $Id: $

(in-package :gsl)

;;;;****************************************************************************
;;;; Unsigned address types size_t
;;;;****************************************************************************

(case (cffi:foreign-type-size :long)
  (8
   (push :int64 *features*)
   #+fsbv (fsbv:defsynonym sizet :uint64))
  (4
   (push :int32 *features*)
   #+fsbv (fsbv:defsynonym sizet :uint32)))

;;;;****************************************************************************
;;;; Type specification conversion
;;;;****************************************************************************

;;; Functions to perform conversions
;;; CL -> GSL in function #'cl-gsl

;;; Sources of equivalence
;;; Cstd -> GSL in alist *cstd-gsl-mapping*

;;;;****************************************************************************
;;;; Basic definition
;;;;****************************************************************************

;;; Preliminary definitions.  These are not used directly outside this
;;; file; they exist to define the two variables
;;; *cstd-cl-type-mapping* and *cstd-gsl-mapping* which are used by
;;; the conversion functions.

(defparameter *gsl-splice-int-types*
  ;; list | grep -i 'gsl_vector.*_alloc\b'
  '("char" "int" "long" "short" "uchar" "uint" "ulong" "ushort")
  "The list of integer types that can be spliced into function names.")

(defparameter *gsl-splice-fp-types*
  ;; list | grep -i 'gsl_vector.*_alloc\b'
  ;; Ordered by: real shortest to longest, then complex shortest to longest.
  ;; Must match *fp-type-mapping*.
  '("float" "" #+long-double "long_double"
    "complex_float" "complex" #+long-double "complex_long_double")
  "The list of floating point types that can be spliced into function names.")

(defparameter *cstd-gsl-mapping*
  (append
   ;; The integer types 
   (remove-if-not
    (lambda (x) (find (rest x) *gsl-splice-int-types* :test 'string-equal))
    (mapcar
     (lambda (type)
       (cons type
	     (let ((ut
		    (if (string-equal type "uns" :end1 3)
			(string-downcase
			 (concatenate 'string "u" (subseq (string type) 9)))
			(string-downcase type))))
	       (if (and (> (length ut) 8)
			(string-equal
			 (subseq ut (- (length ut) 9))
			 "long-long"))
		   (concatenate 'string (subseq ut 0 (- (length ut) 9)) "llong")
		   ut))))
     c-array:*cstd-integer-types*))
   ;; The floating types are associated by order, so it is important that
   ;; order of *fp-type-mapping* and *gsl-splice-fp-types* match,
   ;; though the latter may be longer.
   (c-array:floating-point-association *gsl-splice-fp-types*))
  "Mapping the C standard types to the GSL splice name.")

(defparameter *blas-splice-fp-types*
  ;; Ordered by: real shortest to longest, then complex shortest to longest.
  ;; Must match *fp-type-mapping*.
  '("s" "d" #+long-double nil
    "c" "z" #+long-double nil)
  "The list of floating point types that can be spliced into BLAS function names.")

(defparameter *cstd-blas-mapping*
  ;; The floating types are associated by order, so it is important that
  ;; order of *fp-type-mapping* and *blas-splice-fp-types* match,
  ;; though the latter may be longer.
  (c-array:floating-point-association *blas-splice-fp-types*)
  "Mapping the C standard types to the BLAS splice name.")

;;;;****************************************************************************
;;;; Conversions
;;;;****************************************************************************

;;; GSL splice name             "uchar"
;;; (cl-gsl '(unsigned-byte 8))
;;; "uchar"
(defun cl-gsl (cl-type &optional prepend-underscore blas)
  "The GSL splice string from the CL type."
  (let ((string
	 (c-array:lookup-type
	  (c-array:lookup-type cl-type c-array:*cstd-cl-type-mapping* t)
	  (if blas *cstd-blas-mapping* *cstd-gsl-mapping*))))
    (if (and prepend-underscore (plusp (length string)))
	(concatenate 'string "_" string)
	string)))

;; Conversion of numbers C->CL
;; Liam Healy, Sun May 28 2006 - 22:04
;; Time-stamp: <2009-12-20 23:12:25EST number-conversion.lisp>
;; $Id$

(in-package :c-array)

(export '(complex-to-cl))

;;;;****************************************************************************
;;;; Built-in numerical types
;;;;****************************************************************************

(export '(dcref))

(defmacro dcref (double &optional (index 0))
  "Reference C double(s)."
  `(cffi:mem-aref ,double :double ,index))

;;;;****************************************************************************
;;;; Complex numbers
;;;;****************************************************************************

;;; GSL complex struct is defined in init/complex-types.lisp.
(defun complex-to-cl
    (gsl-complex &optional (index 0) (complex-type 'complex-double-c))
  "Make a CL complex number from the GSL pointer to a complex struct or
   an array of complex structs and an index into the array." 
  (let ((carr (cffi:foreign-slot-value
	       (cffi:inc-pointer
		gsl-complex
		(* index (cffi:foreign-type-size complex-type)))
	       complex-type 'dat)))
    (complex (dcref carr 0)
	     (dcref carr 1))))
	

;; Conversion of numbers C->CL
;; Liam Healy, Sun May 28 2006 - 22:04
;; Time-stamp: <2009-12-07 22:30:23EST number-conversion.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Built-in numerical types
;;;;****************************************************************************

(export '(dcref scref))

(defmacro dcref (double &optional (index 0))
  "Reference C double(s)."
  `(cffi:mem-aref ,double :double ,index))

(defmacro scref (size &optional (index 0))
  "Reference C size(s)."
  `(cffi:mem-aref ,size 'sizet ,index))

;;;;****************************************************************************
;;;; Complex numbers
;;;;****************************************************************************

;;; GSL complex struct is defined in init/complex-types.lisp.
(defun complex-to-cl
    (gsl-complex &optional (index 0) (complex-type 'c-array:complex-double-c))
  "Make a CL complex number from the GSL pointer to a complex struct or
   an array of complex structs and an index into the array." 
  (let ((carr (cffi:foreign-slot-value
	       (cffi:inc-pointer
		gsl-complex
		(* index (cffi:foreign-type-size complex-type)))
	       complex-type 'c-array::dat)))
    (complex (dcref carr 0)
	     (dcref carr 1))))

;;;;****************************************************************************
;;;; Conversion form
;;;;****************************************************************************

(defun cl-convert-form (decl)
  "Generate a form that calls the appropriate converter from C/GSL to CL."
  (case (st-actual-type decl)
    (sf-result 
     `((val ,(st-symbol decl))
       (err ,(st-symbol decl))))
    (sf-result-e10
     `((val ,(st-symbol decl) 'sf-result-e10)
       (e10 ,(st-symbol decl))
       (err ,(st-symbol decl) 'sf-result-e10)))
    (c-array:complex-double-c
     `((complex-to-cl ,(st-symbol decl) 0 'c-array:complex-double-c)))
    (c-array:complex-float-c
     `((complex-to-cl ,(st-symbol decl) 0 'c-array:complex-float-c)))
    (t `((cffi:mem-aref ,(st-symbol decl) ',(st-actual-type decl))))))	

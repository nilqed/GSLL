;; Get/set array or elements: cl-array, maref
;; Liam Healy 2008-08-27 22:43:10EDT maref.lisp
;; Time-stamp: <2009-12-21 14:10:40EST maref.lisp>
;; $Id: $

(in-package :gsl)
(export 'maref)

;;; Normally we won't need to set or get directly from the GSL
;;; vector/matrix pointer.  However, it is necessary to have access to
;;; elements from the GSL vector/matrix pointers for things like
;;; callback functions in solve-minimize-fit and in #'cl-array method
;;; for pointers.

;;; These functions add the following class arguments for
;;; the array:
;;; - a pointer to a GSL vector or matrix structure

(defmacro maref-function-picker
    (type-symbol category ffrestargs &optional value-symbol)
  "Generate sexp to select on the various gsl_{vector,matrix}*_{get,set} functions."
  (cons 'cond
	(mapcar (lambda (tp)
		  `((equal ,type-symbol ',tp)
		    (cffi:foreign-funcall
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,(if value-symbol "_set" "_get"))
		       category tp)
		     ,@ffrestargs
		     ,@(if value-symbol
			   (list (c-array:cl-cffi tp) value-symbol)
			   (list (c-array:cl-cffi tp))))))
		c-array:*array-element-types*)))

(defgeneric maref (object index &optional index2 type)
  (:documentation
   "An element of the data.  The object may be a foreign-array object, a pointer to
    a GSL vector or matrix, or an ordinary CL array of one or two dimensions.")
  (:method ((object marray) index &optional index2 type)
    (declare (ignore type))
    (if index2
	(grid:gref object index index2)
	(grid:gref object index)))
  (:method ((pointer #.+foreign-pointer-class+) index &optional index2
	    (type 'double-float))
    (if index2
	(maref-function-picker
	 type matrix
	 (:pointer pointer sizet index sizet index2))
	(maref-function-picker
	 type vector
	 (:pointer pointer sizet index)))))

;;; Index the GSL function names to maref
#.(cons 'progn
	(append
	 (mapcar (lambda (tp)
		   `(map-name
		     'maref
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_get")
		       'vector tp)))
		 c-array:*array-element-types*)
	 (mapcar (lambda (tp)
		   `(map-name
		     'maref
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_get")
		       'matrix tp)))
		 c-array:*array-element-types*)))

(defgeneric (setf maref) (value object index &optional index2 type)
  (:documentation
   "Set an element of the data.  The object may be a foreign-array object,
    a pointer to a GSL vector or matrix, or an ordinary CL array
    of one or two dimensions.")
  (:method (value (object marray) index &optional index2 type)
    (declare (ignore type))
    (if index2
	(setf (grid:gref object index index2) value)
	(setf (grid:gref object index) value)))
  (:method (value (pointer #.+foreign-pointer-class+) index &optional index2
	    (type 'double-float))
    (if index2
	(maref-function-picker
	 type matrix
	 (:pointer pointer sizet index sizet index2) value)
	(maref-function-picker
	 type vector
	 (:pointer pointer sizet index) value))))

;;; Index the GSL function names to (setf maref)
#.(cons 'progn
	(append
	 (mapcar (lambda (tp)
		   `(map-name
		     '(setf maref)
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_set")
		       'vector tp)))
		 c-array:*array-element-types*)
	 (mapcar (lambda (tp)
		   `(map-name
		     '(setf maref)
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_set")
		       'matrix tp)))
		 c-array:*array-element-types*)))

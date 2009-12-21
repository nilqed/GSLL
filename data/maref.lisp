;; Get/set array or elements: cl-array, maref
;; Liam Healy 2008-08-27 22:43:10EDT maref.lisp
;; Time-stamp: <2009-12-21 09:42:47EST maref.lisp>
;; $Id: $

(in-package :gsl)

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

(defmethod maref
    ((pointer #.+foreign-pointer-class+) index &optional index2
     (type 'double-float))
  (if index2
      (maref-function-picker
	 type matrix
	 (:pointer pointer sizet index sizet index2))
      (maref-function-picker
	 type vector
	 (:pointer pointer sizet index))))

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

(defmethod (setf maref)
    (value (pointer #.+foreign-pointer-class+) index &optional index2
     (type 'double-float))
  (if index2
      (maref-function-picker
       type matrix
       (:pointer pointer sizet index sizet index2) value)
      (maref-function-picker
       type vector
       (:pointer pointer sizet index) value)))

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

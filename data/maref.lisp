;; Get/set array or elements: cl-array, maref
;; Liam Healy 2008-08-27 22:43:10EDT maref.lisp
;; Time-stamp: <2010-06-27 18:03:27EDT maref.lisp>
;;
;; Copyright 2008, 2009 Liam M. Healy
;; Distributed under the terms of the GNU General Public License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
			   (list (grid:cl-cffi tp) value-symbol)
			   (list (grid:cl-cffi tp))))))
		grid:*array-element-types*)))

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
		 grid:*array-element-types*)
	 (mapcar (lambda (tp)
		   `(map-name
		     'maref
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_get")
		       'matrix tp)))
		 grid:*array-element-types*)))

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
		 grid:*array-element-types*)
	 (mapcar (lambda (tp)
		   `(map-name
		     '(setf maref)
		     ,(actual-gsl-function-name
		       `("gsl_" :category :type ,"_set")
		       'matrix tp)))
		 grid:*array-element-types*)))

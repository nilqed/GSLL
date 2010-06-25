;; A "marray" is an array in both GSL and CL
;; Liam Healy 2008-04-06 21:23:41EDT
;; Time-stamp: <2010-06-24 20:03:33EDT marray.lisp>
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

;;;;****************************************************************************
;;;; The class marray and its construction
;;;;****************************************************************************

(export 'marray)
(defclass marray (mobject c-array:foreign-array)
  ((block-pointer :initarg :block-pointer :reader block-pointer)
   (total-size :reader size))
  (:documentation
   "A superclass for arrays represented in GSL and CL."))

;;; We don't allocate or free the C array data, because that is
;;; handled by c-array:foreign-array.  We can use the GSL functions
;;; *_alloc_from_block because they will allocate only the structure,
;;; but we must use CFFI to allocate the block structure; otherwise
;;; GSL would try to allocate the C array.  We do not use any of the
;;; GSL free functions because they would free the C array data.

(defmethod initialize-instance :after ((object marray) &rest initargs)
  (declare (ignore initargs)) ; required by &rest arg for c-array:foreign-array?
  ;; Need to check that all allocations succeeded.
  ;; Don't do anything if mpointer has been assigned (shouldn't happen)
  ;; or this is a permutation or combination (they have their own methods).
  (unless (and (slot-boundp object 'mpointer) (mpointer object))
    (let ((blockptr (cffi:foreign-alloc 'gsl-block-c)))
      (setf (cffi:foreign-slot-value blockptr 'gsl-block-c 'size)
	    (size object)
	    (slot-value object 'block-pointer)
	    blockptr)
      (when (zerop (total-size object))
	(error "Object ~a has zero total dimension." object))
      (let ((array-struct (alloc-from-block object blockptr)))
	(setf (slot-value object 'mpointer) array-struct)
	#-native (set-struct-array-pointer object)
	(tg:finalize object
		     (lambda ()
		       (cffi:foreign-free blockptr)
		       (cffi:foreign-free array-struct)))))))

(defun set-struct-array-pointer (object)
  "Set the pointer in the 'data slot of the foreign structs to be the
   current c-pointer value.  In non-native implementations this need
   be called only once when the marray is made.  In native implementations
   it is called whenever mpointer is requested because of the possibility
   that a GC moved the pointer."
  (setf (cffi:foreign-slot-value (block-pointer object) 'gsl-block-c 'data)
	(c-pointer object)
	;; alloc-from-block automatically copies over the data pointer
	;; from the block to the vector/matrix; we must do that manually here
	(cffi:foreign-slot-value
	 (slot-value object 'mpointer)
	 (if (typep object 'matrix) 'gsl-matrix-c 'gsl-vector-c)
	 'data)
	(c-pointer object)))

#+native
(defmethod mpointer ((object marray))
  "Compute the c-pointer of the array and place it in the GSL struct
   because the stored version is untrustworthy unless it was computed
   within the same native-pointer-protect form as this mpointer
   extraction."
  (set-struct-array-pointer object)
  (call-next-method))

;;;;****************************************************************************
;;;; Definition of specific data classes
;;;;****************************************************************************

(defparameter *class-element-type* nil
  "The mapping between the class name and the CL element type.")

(defun data-class-name (category element-type)
  "The class name from the type of element."
  ;; e.g. (data-class-name 'vector '(unsigned-byte 8))
  ;; -> VECTOR-UNSIGNED-BYTE-8
  (if (member category '(vector matrix))
      (intern (format nil "~:@(~a-~a~)"
		      category (c-array:cl-single element-type :gsl))
	      :gsl)
      category))

(defun data-defclass (category superclass)
  "Define all the subclasses based on the known element types."
  (cons 'progn
	(mapcan
	 (lambda (element-type-cl)
	   (let* ((class-name (data-class-name category element-type-cl)))
	     ;; Define the class
	     `((defclass ,class-name
		   (,superclass)
		 ((element-type :initform ',element-type-cl
				:allocation :class)))
	       ;; Push mapping onto *class-element-type*
	       (pushnew ',(cons class-name element-type-cl)
			*class-element-type* :test #'equal)
	       (export ',class-name))))
	 c-array:*array-element-types*)))

;;;;****************************************************************************
;;;; Make data from either the dimensions provided or from the initial values
;;;;****************************************************************************

(export 'make-marray)
(defun make-marray
    (class-or-element-type &rest keys &key dimensions initial-contents
     &allow-other-keys)
  "Make a GSLL array with the given element type,
   :dimensions, and :initial-contents, :initial-element or :data.  If
   the :data argument is supplied, it should be a CL array generated
   with #'make-ffa."
  (apply #'make-instance
	 (if (subtypep class-or-element-type 'marray)
	     class-or-element-type
	     (data-class-name
	      (if
	       (or
		(and dimensions (listp dimensions) (eql (length dimensions) 2))
		(and initial-contents (listp (first initial-contents))))
	       'matrix 'vector)
	      class-or-element-type))
	 keys))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew 'marray grid:*grid-data-superclasses*))

(defmethod grid:make-grid-data
    ((type (eql 'marray)) dimensions rest-spec &rest keys
     &key initial-element initial-contents)
  (declare (ignorable initial-element initial-contents))
  (apply #'make-marray (grid:spec-scalar-p rest-spec)
	 :dimensions dimensions
	 keys))

;;;;****************************************************************************
;;;; Copy to and from bare mpointers 
;;;;****************************************************************************

(defgeneric contents-from-pointer (pointer struct-type &optional element-type)
  (:documentation
   "Create a contents list from the GSL object of type struct-type
    referenced by pointer."))

(defmethod c-array:copy-making-destination ((pointer #.+foreign-pointer-class+))
  (foreign-pointer-method
   pointer
   ;; Default assumption when destination isn't given in #'copy is
   ;; that this should make a vector-double-float.
   (c-array:copy-to-destination pointer 'vector-double-float)))

(defmethod c-array:copy-to-destination
    ((pointer #.+foreign-pointer-class+) (class-name symbol))
  (foreign-pointer-method
   pointer
   (make-marray
    class-name
    :initial-contents
    (contents-from-pointer
     pointer
     (if (subtypep class-name 'matrix) 'gsl-matrix-c 'gsl-vector-c)
     (c-array:lookup-type class-name *class-element-type*)))))

;; Some functions in solve-minimize-fit return a pointer to a GSL
;; vector of double-floats.  This function turns that into a
;; foreign-friendly array.  There is no choice but to copy over the
;; data even on native implementations; because GSL is doing the
;; mallocing, the data are not CL-accessible.


;;;;****************************************************************************
;;;; Convenient defaulting of defmfun arguments 
;;;;****************************************************************************

(defun make-marray-or-default
    (default dimensions
     &optional dont-make-if-nil (element-type 'double-float) initial-element)
  "If default is T or dont-make-if-nil, make a marray and
   returned with the dimensions and element-type.  If default is a
   marray, it is returned after being synced for non-native."
  (if (member default (list t dont-make-if-nil))
      (if initial-element
	  (make-marray
	   element-type :dimensions dimensions :initial-element initial-element)
	  (make-marray element-type :dimensions dimensions))
      (progn #-native
	     (when (typep default 'marray)
	       (c-array:copy-cl-to-c default))
	     default)))

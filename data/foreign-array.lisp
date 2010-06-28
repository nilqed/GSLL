;; A grid:foreign-array with added metadata for GSL.
;; Liam Healy 2008-04-06 21:23:41EDT
;; Time-stamp: <2010-06-27 21:49:36EDT foreign-array.lisp>
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
;;;; Make the mpointer, block-pointer, total-size metadata
;;;;****************************************************************************

;;; We don't allocate or free the C array data, because that is
;;; handled by grid:foreign-array.  We can use the GSL functions
;;; *_alloc_from_block because they will allocate only the structure,
;;; but we must use CFFI to allocate the block structure; otherwise
;;; GSL would try to allocate the C array.  We do not use any of the
;;; GSL free functions because they would free the C array data.

(defmacro metadata-slot (object name)
  `(getf (slot-value ,object 'grid:foreign-metadata) ,name))

(defun make-gsl-metadata (object)
  "Make the necessary GSL metadata (mpointer and block-pointer)
   for the given foreign array, and return the mpointer.
   This should only be called by #'mpointer the first time
   it is called on a particular foreign-array."
  ;; Don't do anything if mpointer has already been assigned.
  (unless (metadata-slot object 'mpointer)
    (when (zerop (total-size object))
      (error "Object ~a has zero total dimension." object))
    (let ((blockptr (cffi:foreign-alloc 'gsl-block-c)))
      (setf (cffi:foreign-slot-value blockptr 'gsl-block-c 'size)
	    (grid:total-size object)
	    (cffi:foreign-slot-value blockptr 'gsl-block-c 'data)
	    (foreign-pointer object)
	    (metadata-slot object 'block-pointer) blockptr)
      (let ((array-struct (alloc-from-block object blockptr)))
	(setf
	 (metadata-slot object 'mpointer) array-struct
	 ;; alloc-from-block automatically copies over the data pointer
	 ;; from the block to the vector/matrix; we must do that manually here
	 (cffi:foreign-slot-value
	  array-struct
	  (if (typep object 'matrix) 'gsl-matrix-c 'gsl-vector-c) 'data)
	 (foreign-pointer object))
	(tg:finalize object
		     (lambda ()
		       (cffi:foreign-free blockptr)
		       (cffi:foreign-free array-struct))))
      (metadata-slot object 'mpointer))))

(defmethod mpointer ((object grid:foreign-array))
  (or 
   (metadata-slot object 'mpointer)
   (make-gsl-metadata object)))

;;;;****************************************************************************
;;;; Make data from either the dimensions provided or from the initial values
;;;;****************************************************************************

;;;;;;; There are >1400 calls to make-marray, write an emulation?
;;; This should probably be moved/renamed grid:make-foreign-array.
(export 'make-marray)
(defun make-marray (element-type &rest keys &key dimensions &allow-other-keys)
  "Make a GSLL array with the given element type,
   :dimensions, and :initial-contents, :initial-element or :data."
  (when (subtypep element-type 'grid:foreign-array)
    (error "Can't take a class name here anymore, sorry."))
  (apply
   'grid:make-grid
   `((grid:foreign-array ,@dimensions) element-type)
   keys))

(defun make-marray-or-default
    (default dimensions
     &optional dont-make-if-nil (element-type 'double-float) initial-element)
  "If default is T or dont-make-if-nil, make a marray and
   returned with the dimensions and element-type.  If default is a
   marray, it is returned."
  (if (member default (list t dont-make-if-nil))
      (if initial-element
	  (make-marray
	   element-type :dimensions dimensions :initial-element initial-element)
	  (make-marray element-type :dimensions dimensions))
      default))

;;;;****************************************************************************
;;;; Make from GSL mpointers 
;;;;****************************************************************************

;; Some functions in solve-minimize-fit return a pointer to a GSL
;; vector of double-floats.  This function turns it into a
;; foreign-array.  

(defun make-foreign-array-from-mpointer
    (mpointer &optional (element-type 'double-float) (category :vector))
  "Make the foreign array when a GSL pointer to a
   gsl-vector-c or gsl-matrix-c is given."
  (let* ((cstruct
	  (case category
	    (:vector 'gsl-vector-c)
	    (:matrix 'gsl-matrix-c)
	    (t (error "Unrecognized category ~a" category))))
	 (foreign-pointer (cffi:foreign-slot-value mpointer cstruct 'data)))
    (grid:make-grid
     (case category
       (:vector
	`((foreign-array ,(cffi:foreign-slot-value mpointer cstruct 'size))
	  ,element-type))
       (:matrix
	`((foreign-array
	   ,(cffi:foreign-slot-value mpointer cstruct 'size0)
	   ,(cffi:foreign-slot-value mpointer cstruct 'size1))
	  ,element-type)))
     :foreign-pointer foreign-pointer
     :foreign-metadata mpointer)))

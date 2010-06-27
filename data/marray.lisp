;; A "marray" is an array in both GSL and CL
;; Liam Healy 2008-04-06 21:23:41EDT
;; Time-stamp: <2010-06-26 23:04:25EDT marray.lisp>
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

;;; Slots were mpointer, block-pointer, total-size

;;; We don't allocate or free the C array data, because that is
;;; handled by grid:foreign-array.  We can use the GSL functions
;;; *_alloc_from_block because they will allocate only the structure,
;;; but we must use CFFI to allocate the block structure; otherwise
;;; GSL would try to allocate the C array.  We do not use any of the
;;; GSL free functions because they would free the C array data.

(defmacro metadata-slot (object name)
  `(getf (foreign-metadata ,object) ,name))

(defun make-gsl-metadata (object)
  ;; Need to check that all allocations succeeded.
  ;; Don't do anything if mpointer has been assigned,
  ;; or this is a permutation or combination (they have their own methods).
  (unless (metadata-slot object 'mpointer)
    (when (zerop (total-size object))
      (error "Object ~a has zero total dimension." object))
    (let* ((blockptr (cffi:foreign-alloc 'gsl-block-c))
	   (array-struct (alloc-from-block object blockptr)))
      (setf (cffi:foreign-slot-value blockptr 'gsl-block-c 'size)
	    (grid:total-size object)
	    (cffi:foreign-slot-value blockptr 'gsl-block-c 'data)
	    (foreign-pointer object)
	    (metadata-slot object 'block-pointer) blockptr
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
		     (cffi:foreign-free array-struct))))))

(defmethod mpointer ((object grid:foreign-array))
  (metadata-slot object 'mpointer)
  (make-gsl-metadata object))

;;;;****************************************************************************
;;;; Make data from either the dimensions provided or from the initial values
;;;;****************************************************************************

;;;;;;; There are >1400 calls to make-marray, write an emulation?
;;; This should probably be moved/renamed grid:make-foreign-array.
(export 'make-marray)
(defun make-marray
    (element-type &rest keys &key dimensions initial-contents
     &allow-other-keys)
  "Make a GSLL array with the given element type,
   :dimensions, and :initial-contents, :initial-element or :data."
  (when (subtypep element-type 'grid:foreign-array)
    (error "Can't take a class name here anymore, sorry.")
    (apply
     'grid:make-grid
     `((grid:foreign-array ,@dimensions) element-type)
     keys)))

;;;;****************************************************************************
;;;; Copy to and from bare mpointers 
;;;;****************************************************************************

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

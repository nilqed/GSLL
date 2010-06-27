;; Copy marrays to/from CL arrays
;; Liam Healy 2009-02-11 19:28:44EST copy-cl.lisp
;; Time-stamp: <2010-06-27 18:03:25EDT copy-cl.lisp>
;;
;; Copyright 2009 Liam M. Healy
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

;;; The function #'copy can be used to copy the contents to or from a
;;; CL array.  If the destination isn't literally supplied, it can be
;;; a symbol 'array indicating that the marray is to be copied to a CL
;;; array, or an element type like 'double-float, indicating the
;;; contents of the CL array is to be copied to a new marray with the
;;; given element type.

;;; Contrast this with the function #'cl-array or the initarg
;;; :cl-array to #'make-marray.  The function returns the cl-array
;;; without copying, and so might be faster but also makes the
;;; original marray object vulnerable if an element is changed, which
;;; officially has unpredictable results.  Likewise, the initarg will
;;; use the given CL array directly in the created marray without
;;; copying.

(in-package :gsl)

;;; These make destination but are methods of copy-to-destination so
;;; that the class argument may be supplied.

(defmethod grid:copy-to-destination ((source mvector) (destination array))
  (unless (equal (dimensions source) (array-dimensions destination))
    (error 'grid:array-mismatch))
  (loop for i below (dim0 source)
       do (setf (aref destination i) (maref source i)))
  destination)

(defmethod grid:copy-to-destination ((source matrix) (destination array))
  (unless (equal (dimensions source) (array-dimensions destination))
    (error 'grid:array-mismatch))
  (loop for i below (dim0 source) do
       (loop for j below (dim1 source) do
	    (setf (aref destination i j) (maref source i j))))
  destination)

(defmethod grid:copy-to-destination ((source array) (destination mvector))
  (unless (equal (array-dimensions source) (dimensions destination))
    (error 'grid:array-mismatch))
  (loop for i below (length source)
       do (setf (maref destination i) (aref source i)))
  destination)

(defmethod grid:copy-to-destination ((source array) (destination matrix))
  (unless (equal (array-dimensions source) (dimensions destination))
    (error 'grid:array-mismatch))
  (loop for i below (array-dimension source 0) do
       (loop for j below (array-dimension source 1) do
	    (setf (maref destination i j) (aref source i j))))
  destination)

(defmethod grid:copy-to-destination ((source marray) (destclass (eql 'array)))
  (let ((destination
	 (grid:make-ffa (element-type source) :dimensions (dimensions source))))
    (grid:copy-to-destination source destination)))

;;; Copy to a named marray element-type, where the type is a symbol
(defmethod grid:copy-to-destination ((source array) (destclass symbol))
  (let ((destination
	 (make-marray destclass :dimensions (array-dimensions source))))
    (grid:copy-to-destination source destination)))

;;; Copy to a named marray element-type, where the type is a list
(defmethod grid:copy-to-destination ((source array) (destclass list))
  (let ((destination
	 (make-marray destclass :dimensions (array-dimensions source))))
    (grid:copy-to-destination source destination)))

;;; Examples and tests

(generate-all-array-tests vector-copy-to-cl-and-back t
 (cl-array (copy (copy (array-default 3) 'array) 'default-element-type)))

(generate-all-array-tests matrix-copy-to-cl-and-back t
 (cl-array (copy (copy (array-default '(3 3)) 'array) 'default-element-type)))



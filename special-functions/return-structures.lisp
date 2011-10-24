;; Structures returned by special functions.
;; Liam Healy, Mon Jan  1 2007 - 11:35
;; Time-stamp: <2011-10-23 21:15:46EDT return-structures.lisp>
;;
;; Copyright 2007, 2008, 2009, 2010, 2011 Liam M. Healy
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
;;;; Result from special functions
;;;;****************************************************************************

;;; Define methods for cffi:translate-from-foreign for translate-from-foreign
;;; that return the slots as successive values.
;;; (defmethod cffi:translate-from-foreign (value (type ))); sf-result
;;; (defmethod cffi:translate-from-foreign (value (type ))); sf-result-e10
;;; This should become the cffi:translate-from-foreign method for sf-result-e10
;;;(defun values-e10 (val err e10)
;;;  (values val e10 err))
;;; Note: need to enhance CFFI to either make a default class name for each type, or allow the specification of a class when groveling.

(defun complex-with-error (real-sfr imaginary-sfr)
  "Return two complex numbers, the value and the error."
  (multiple-value-bind (re-val re-err)
      (fsbv:object real-sfr 'sf-result)
    (multiple-value-bind (im-val im-err)
	(fsbv:object imaginary-sfr 'sf-result)
      (values
       (complex re-val im-val)
       (complex re-err im-err)))))

(defun values-with-errors (&rest values-sfr)
  "Return numbers as values and errors."
  (loop for vs in values-sfr
     for (val err) = (multiple-value-list (fsbv:object vs 'sf-result))
     collect val into values
     collect err into errors
     finally (return (values-list (append values errors)))))

;;;;****************************************************************************
;;;; Array construction
;;;;****************************************************************************

(defparameter *default-sf-array-size* 5
  "The default size to make an array returned from a special function.")

(defun vdf (size-or-array)
  "Make or take a vector-double-float."
  (if (integerp size-or-array)
      (grid:make-foreign-array 'double-float :dimensions size-or-array)
      size-or-array))

(defun vdf-size (size-or-array)
  "Make or take a vector-double-float."
  (if (integerp size-or-array)
      size-or-array
      (size size-or-array)))

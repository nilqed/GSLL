;; Additional methods for lisp-unit
;; Liam Healy 2009-04-15 23:23:30EDT augment.lisp
;; Time-stamp: <2010-05-22 10:54:02EDT augment.lisp>
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

(in-package :gsl)

(defmethod lisp-unit:numerical-equal
    ((result1 marray) (result2 marray) &key (test #'lisp-unit:number-equal))
  "Return true if the arrays are numerically equal according to :TEST."
  (when (equal (dimensions result1) (dimensions result2))
    (lisp-unit:numerical-equal (cl-array result1) (cl-array result2)
			       :test test)))

;;; See cdf/test.c
(defconstant +test-tol0+ (* 2 +dbl-epsilon+))
(defconstant +test-tol1+ (* 16 +dbl-epsilon+))
(defconstant +test-tol2+ (* 256 +dbl-epsilon+))
(defconstant +test-tol3+ (* 2048 +dbl-epsilon+))
(defconstant +test-tol4+ (* 16384 +dbl-epsilon+))
(defconstant +test-tol5+ (* 131072 +dbl-epsilon+))
(defconstant +test-tol6+ (* 1048576 +dbl-epsilon+))

;; (assert-to-tolerance (tdist-P 0.0d0 1.0d0) 0.5d0 +test-tol6+)
(defmacro assert-to-tolerance (form expected-value tolerance)
  `(let ((lisp-unit:*epsilon* ,tolerance))
     (lisp-unit::assert-numerical-equal
      ,expected-value
      ,form)))

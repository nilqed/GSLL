;; Additional methods for lisp-unit
;; Liam Healy 2009-04-15 23:23:30EDT augment.lisp
;; Time-stamp: <2010-05-30 12:42:11EDT augment.lisp>
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
(defconstant +test-sqrt-tol0+ (* 2 +sqrt-dbl-epsilon+))

;;; These are 1.0 if not "RELEASED"
(defconstant +test-sigma+ 1.5d0)
(defconstant +test-factor+ 100.0d0)

(defun sf-frac-diff (x1 x2)
  ;; After test_sf_frac_diff in specfunc/test_sf.c.
  (cond ((and (zerop x1) (zerop x2))(exp-err-scaled 1.0d0 +test-sqrt-tol0+)
	 (coerce 0 (type-of x1)))
	((zerop x1)
	 (abs x2))
	((and (<= x1 most-positive-double-float)
	      (<= x2 most-positive-double-float)
	      (not (zerop (+ x1 x2))))
	 (abs (/ (- x1 x2) (+ x1 x2))))
	(t 1.0d0)))

(defun sf-check-pair (result expected-value tolerance &optional error-estimate)
  (or (eql result expected-value) ; catch expected inifinity/nan
      (let ((diff (abs (- result expected-value))))
	(and
	 (<= (sf-frac-diff result expected-value) (* +test-factor+ tolerance))
	 (if error-estimate
	     (and (not (minusp error-estimate)) ; redundant but signalled as separate error in C
		  (finitep error-estimate)
		  (<= diff (* 2 +test-sigma+ error-estimate)))
	     t)))))

(defun sf-check-results (result-list expected-value tolerance)
  ;; After test_sf_check_result in specfunc/test_sf.c.
  (when (atom expected-value)
    (setf expected-value (list expected-value)))
  (when (= (length result-list) (* 2 (length expected-value)))
    ;; Error information is returned
    (loop for ind below (length expected-value)
       always
       (sf-check-pair
	(elt result-list ind)
	(elt expected-value ind)
	tolerance
	(elt result-list (+ ind (length expected-value)))))))

;; (assert-to-tolerance (tdist-P 0.0d0 1.0d0) 0.5d0 +test-tol6+)
;; Probably can remove the binding of lisp-unit:*epsilon*, it doesn't
;; do anything anymore.
(defmacro assert-to-tolerance (form expected-value tolerance)
  `(let ((lisp-unit:*epsilon* ,tolerance))
     (lisp-unit::assert-true
      ,expected-value
      (sf-check-results
       (multiple-value-list ,form) ,expected-value ,tolerance))))

;; lisp-unit::assert-true

(defmacro assert-posinf (form)
  `(lisp-unit::assert-true
    (let ((val ,form))
      (and (infinityp val) (plusp val)))))

(defmacro assert-neginf (form)
  `(lisp-unit::assert-true
    (let ((val ,form))
      (and (infinityp val) (minusp val)))))

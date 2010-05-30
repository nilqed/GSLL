;; Regression test EXPONENTIAL-FUNCTIONS for GSLL
;;
;; Copyright 2009, 2010 Liam M. Healy
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

(defconstant +exp-x+ (* 0.8d0 +log-dbl-max+))
(defconstant +ln2+ 0.69314718055994530941723212146d0)

(defmacro assert-sf-scale (form expected-value result-tol err-tol &optional scale)
  `(lisp-unit::assert-true
    (multiple-value-bind (val scale err)
	,form
      (and
       (sf-check-pair val ,expected-value ,result-tol err)
       (= scale ,scale)))))

;; assert-to-tolerance probably not doing any good here
(LISP-UNIT:DEFINE-TEST EXPONENTIAL-FUNCTIONS
  ;; From specfunc/test_sf.c
  (assert-to-tolerance (gsl-exp -10.0d0) (exp -10.0d0) +test-tol0+)
  (assert-to-tolerance (gsl-exp 10.0d0) (exp 10.0d0) +test-tol0+)
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-scaled 1.0d0)
     (or 
      (> (sf-frac-diff val (exp 1.0d0)) +test-tol0+)
      (> err +test-tol1+)
      (not (zerop scale)))))
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-scaled 2000.0d0)
     (or 
      (> (sf-frac-diff val 3.88118019428363725d0) +test-tol3+)
      (> err +test-tol5+)
      (not (= scale 868)))))
  (assert-to-tolerance (exp-err -10.0d0 +test-tol1+) (exp -10.0d0) +test-tol1+)
  (assert-to-tolerance (exp-err 10.0d0 +test-tol1+) (exp 10.0d0) +test-tol1+)
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-err-scaled 1.0d0 +test-sqrt-tol0+)
     (or 
      (> (sf-frac-diff val (exp 1.0d0)) +test-tol1+)
      (> err (* 32 +test-tol0+))
      (not (zerop scale)))))
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-err-scaled 2000.0d0 1.0d-10)
     (or 
      (> (sf-frac-diff val 3.88118019428363725d0) +test-tol3+)
      (> err 1.0d-7)
      (not (= scale 868)))))
  (assert-to-tolerance (exp-mult -10.0d0 1.0d-06) (* 1.0d-06 (exp -10.0d0)) +test-tol0+)
  (assert-to-tolerance (exp-mult -10.0d0 2.0d) (* 2.0d0 (exp -10.0d0)) +test-tol0+)
  (assert-to-tolerance (exp-mult -10.0d0 -2.0d) (* -2.0d0 (exp -10.0d0)) +test-tol0+)
  (assert-to-tolerance (exp-mult 10.0d0 1.0d-06) (* 1.0d-06 (exp 10.0d0)) +test-tol0+)
  (assert-to-tolerance (exp-mult 10.0d0 -2.0d0) (* -2.0d0 (exp 10.0d0)) +test-tol0+)
  (assert-to-tolerance
   (exp-mult +exp-x+ 1.00001d0) (* 1.00001d0 (exp +exp-x+)) +test-tol3+)
  (assert-to-tolerance
   (exp-mult +exp-x+ 1.000001d0) (* 1.000001d0 (exp +exp-x+)) +test-tol3+)
  (assert-to-tolerance
   (exp-mult +exp-x+ 1.0000001d0) (* 1.0000001d0 (exp +exp-x+)) +test-tol3+)
  (assert-to-tolerance
   (exp-mult +exp-x+ 100.0d0) (* 100.0d0 (exp +exp-x+)) +test-tol3+)
  (assert-to-tolerance
   (exp-mult +exp-x+ 1.0d20) (* 1.0d20 (exp +exp-x+)) +test-tol3+)
  (assert-to-tolerance
   (exp-mult +exp-x+ (* +exp-x+ (exp +ln2+))) 2.0d0 +test-tol4+)
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-mult-scaled 1.0d0 1.0d0)
     (or 
      (> (sf-frac-diff val (exp 1.0d0)) +test-tol0+)
      (> err +test-tol2+)
      (not (zerop scale)))))
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-mult-scaled 1000.0d0 1.0d200)
     (or 
      (> (sf-frac-diff val 1.970071114017046993888879352d0) +test-tol3+)
      (> err 1.0d-11)
      (not (= scale 634)))))
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-mult-err-scaled 1.0d0 +test-tol0+ 1.0d0 +test-tol0+)
     (or 
      (> (sf-frac-diff val (exp 1.0d0)) +test-tol0+)
      (> err +test-tol2+)
      (not (zerop scale)))))
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-mult-err-scaled 1000.0d0 1.0d-12 1.0d200 1.0d190)
     (or 
      (> (sf-frac-diff val 1.9700711140165661d0) +test-tol3+)
      (> err 1.0d-9)
      (not (= scale 634)))))
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-mult-scaled 10000.0d0 1.0d0)
     (and
      (sf-check-pair val 8.806818225662921587261496007d0 +test-tol5+ err)
      (= scale 4342))))
  (lisp-unit::assert-true
   (multiple-value-bind (val scale err)
       (exp-mult-scaled 100.0d0 1.0d0)
     (and
      (sf-check-pair val 2.688117141816135448412625551d43 +test-tol2+ err)
      (zerop scale)))))
  

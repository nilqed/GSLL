;; Regression test CHOLESKY for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST CHOLESKY
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb2-soln*)
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM *HILB2*))))
  (let ((lisp-unit:*epsilon* (* 2 128 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb3-soln*)
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM *HILB3*))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb4-soln*)
     (MULTIPLE-VALUE-LIST (TEST-SV-SOLVE-DIM *hilb4*))))
  (let ((lisp-unit:*epsilon* 0.5d0))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb12-soln*)
     (MULTIPLE-VALUE-LIST (TEST-SV-SOLVE-DIM *hilb12*))))
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb2*)
     (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-DECOMP-DIM *HILB2*))))
  (let ((lisp-unit:*epsilon* (* 2 128 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *HILB3*)
     (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-DECOMP-DIM *HILB3*))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *HILB4*)
     (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-DECOMP-DIM *HILB4*))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *HILB12*)
     (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-DECOMP-DIM *HILB12*)))))


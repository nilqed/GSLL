;; Regression test LAPLACE for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST LAPLACE
  (LISP-UNIT::ASSERT-true (testpdf 'laplace-pdf :laplace :a 2.75d0))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6967346701436833d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-P 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3032653298563167d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-Q 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-PINV 0.6967346701436833d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0d0)
   (MULTIPLE-VALUE-LIST (LAPLACE-QINV 0.3032653298563167d0 2.0d0))))


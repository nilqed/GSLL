;; Regression test LOGISTIC for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST LOGISTIC
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 82.6131993192451d0 -16.367346042668906d0
	  -9.31513272043762d0 28.87020708710654d0
	  -11.989809875784625d0 -0.6012364762000397d0
	  31.142555263622d0 10.684673721048895d0
	  1.6051840954024446d0 10.457241904701199d0
	  11.523714106294097d0))
   (MULTIPLE-VALUE-LIST
    (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (sample rng 'logistic :a 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.2350037122015945d0)
   (MULTIPLE-VALUE-LIST (LOGISTIC-PDF 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6224593312018546d0)
   (MULTIPLE-VALUE-LIST (LOGISTIC-P 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.37754066879814546d0)
   (MULTIPLE-VALUE-LIST (LOGISTIC-Q 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5000000000000001d0)
   (MULTIPLE-VALUE-LIST (LOGISTIC-PINV 0.6224593312018546d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4999999999999998d0)
   (MULTIPLE-VALUE-LIST (LOGISTIC-QINV 0.37754066879814546d0 1.0d0))))


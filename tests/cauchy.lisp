;; Regression test CAUCHY for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST CAUCHY
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST -0.00811319915595434d0 5.617196410586812d0
	  12.292369828923075d0 -1.6741088357812182d0
	  8.909104486260928d0 211.6765861544609d0
	  -1.3439049184367153d0 -10.364363282910663d0
	  -79.0709314248171d0 -10.652071087998578d0
	  -9.393948243493877d0))
   (MULTIPLE-VALUE-LIST
    (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (sample rng :cauchy :a 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.03183098861837907d0)
   (MULTIPLE-VALUE-LIST (CAUCHY-PDF 0.0d0 10.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6475836176504333d0)
   (MULTIPLE-VALUE-LIST (CAUCHY-P 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.35241638234956674d0)
   (MULTIPLE-VALUE-LIST (CAUCHY-Q 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.9999999999999998d0)
   (MULTIPLE-VALUE-LIST
    (CAUCHY-PINV 0.6475836176504333d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.0000000000000002d0)
   (MULTIPLE-VALUE-LIST
    (CAUCHY-QINV 0.35241638234956674d0 2.0d0))))


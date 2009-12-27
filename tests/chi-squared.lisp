;; Regression test CHI-SQUARED for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST CHI-SQUARED
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 13.043328884186328d0 11.427227829236712d0
	  16.55811815484942d0 7.128795406995407d0
	  5.120266499239882d0 10.464572605669142d0
	  5.8126929867006405d0 8.784940866479005d0
	  7.559275305609187d0 8.35181083950897d0
	  4.140798004825149d0))
   (MULTIPLE-VALUE-LIST
    (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (sample rng 'chi-squared :nu 10.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.43939128946772227d0)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-PDF 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5204998778130462d0)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-P 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.4795001221869538d0)
   (MULTIPLE-VALUE-LIST (CHI-SQUARED-Q 0.5d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5000000000000003d0)
   (MULTIPLE-VALUE-LIST
    (CHI-SQUARED-PINV 0.5204998778130463d0 1.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.5000000000000003d0)
   (MULTIPLE-VALUE-LIST
    (CHI-SQUARED-QINV 0.4795001221869537d0 1.0d0))))


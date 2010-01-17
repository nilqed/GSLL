;; Regression test GUMBEL1 for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST GUMBEL1
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 8.954596257487015d0 0.0973051899750762d0
	  0.45913506233088003d0 3.6074124224293223d0
	  0.31300027468174807d0 1.0165796949651174d0
	  3.8292081936610396d0 1.912897393181305d0
	  1.17748457894919d0 1.893232107970416d0
	  1.9859118616847695d0))
   (MULTIPLE-VALUE-LIST
    (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (sample rng :gumbel1 :a 1.0d0 :b 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.29625708964974956d0)
   (MULTIPLE-VALUE-LIST (GUMBEL1-PDF 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1637073598773166d0)
   (MULTIPLE-VALUE-LIST (GUMBEL1-P 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8362926401226833d0)
   (MULTIPLE-VALUE-LIST (GUMBEL1-Q 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.10000000000000007d0)
   (MULTIPLE-VALUE-LIST
    (GUMBEL1-PINV 0.1637073598773166d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.10000000000000028d0)
   (MULTIPLE-VALUE-LIST
    (GUMBEL1-QINV 0.8362926401226833d0 1.0d0 2.0d0))))


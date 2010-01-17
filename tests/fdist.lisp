;; Regression test FDIST for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST FDIST
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 18.411308904958695d0 5.120881381831058d0
	  18.104535265974707d0 0.15934280606960227d0
	  0.06272171507636773d0 2.2809441726456456d0
	  0.5259458753395939d0 1.8256109001076744d0
	  0.845346894458977d0 2.5212086970057763d0
	  0.5212415547032052d0))
   (MULTIPLE-VALUE-LIST
    (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (sample rng :fdist :nu1 1.0d0 :nu2 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.1594719884624466d0)
   (MULTIPLE-VALUE-LIST (FDIST-PDF 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.6123724356957948d0)
   (MULTIPLE-VALUE-LIST (FDIST-P 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.3876275643042052d0)
   (MULTIPLE-VALUE-LIST (FDIST-Q 1.2d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.2d0)
				     (MULTIPLE-VALUE-LIST
				      (FDIST-PINV
				       0.612372435695795d0
				       1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.1999999999999995d0)
   (MULTIPLE-VALUE-LIST
    (FDIST-QINV 0.38762756430420503d0 1.0d0 2.0d0))))


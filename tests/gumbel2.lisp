;; Regression test GUMBEL2 for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST GUMBEL2
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 7743.400858519516d0 1.102196701680339d0
	  1.5827044520998628d0 36.87052393317972d0
	  1.3675219066608615d0 2.7637257945633085d0
	  46.026080060263446d0 6.772683525074477d0
	  3.2461983686562204d0 6.640797807286079d0
	  7.285687897019733d0))
   (MULTIPLE-VALUE-LIST
    (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (sample rng 'gumbel2 :a 1.0d0 :b 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.053625603682851145d0)
   (MULTIPLE-VALUE-LIST (GUMBEL2-PDF 5.0d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8187307530779818d0)
   (MULTIPLE-VALUE-LIST (GUMBEL2-P 10.0d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.18126924692201815d0)
   (MULTIPLE-VALUE-LIST (GUMBEL2-Q 10.0d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 9.999999999999998d0)
   (MULTIPLE-VALUE-LIST
    (GUMBEL2-PINV 0.8187307530779818d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 10.0d0)
   (MULTIPLE-VALUE-LIST (GUMBEL2-QINV 0.18126924692201815d0 1.0d0 2.0d0))))


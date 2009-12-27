;; Regression test BETA for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST BETA
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.3935474359990073d0 0.7063621551518341d0
	  0.044515648447265056d0 0.09286083229785232d0
	  0.210544366728104d0 0.010114317425185686d0
	  0.4595767375719009d0 0.1515157002550483d0
	  0.1731331145031117d0 0.4270743075655188d0
	  0.3353314142479658d0))
   (MULTIPLE-VALUE-LIST
    (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (sample rng 'beta :a 1.0d0 :b 2.0d0)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 1.8000000000000016d0)
   (MULTIPLE-VALUE-LIST (BETA-PDF 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.19000000000000017d0)
   (MULTIPLE-VALUE-LIST (BETA-P 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.8099999999999998d0)
   (MULTIPLE-VALUE-LIST (BETA-Q 0.1d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.09999999999999992d0)
   (MULTIPLE-VALUE-LIST (BETA-PINV 0.19d0 1.0d0 2.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.09999999999999988d0)
   (MULTIPLE-VALUE-LIST (BETA-QINV 0.81d0 1.0d0 2.0d0))))


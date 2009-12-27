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
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 0.005166356198580803d0 -3.942577717493807d0
                               -8.329510281601332d0 1.1159975704649974d0
                               -6.2234038148786555d0 -35.04800398421181d0
                               0.8888158320028845d0 7.161892491969776d0
                               25.24637780914261d0 7.341651048064451d0
                               6.54142651602034d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (sample rng 'laplace :a 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.05d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LAPLACE-PDF 0.0d0
                                                                        10.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.6967346701436833d0)
                        (MULTIPLE-VALUE-LIST (LAPLACE-P 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.3032653298563167d0)
                        (MULTIPLE-VALUE-LIST (LAPLACE-Q 1.0d0 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LAPLACE-PINV
                                                            0.6967346701436833d0
                                                            2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LAPLACE-QINV
                                                            0.3032653298563167d0
                                                            2.0d0))))


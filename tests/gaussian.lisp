;; Regression test GAUSSIAN for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST GAUSSIAN
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 1.3391860811867589d0 -0.8810099183143839d0
                               16.744084062537738d0 7.336411072925795d0
                               9.975246316020124d0 -12.775020810027664d0
                               -23.967152827332075d0 -6.79280164729211d0
                               -0.3909131843358723d0 8.935555455208181d0
                               -0.17647794589783283d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (sample rng :gaussian :sigma 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.039894228040143274d0)
                        (MULTIPLE-VALUE-LIST (GAUSSIAN-PDF 0.0d0 10.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 7.648734260124924d0 -11.307312946196086d0
                               4.523361654215173d0 -4.5181725947577185d0
                               13.64676540828912d0 3.553650119391929d0
                               -5.567035948404032d0 10.097727863788887d0
                               2.519120940798607d0 -9.024585521868676d0
                               -9.463283369593537d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((RNG (MAKE-RANDOM-NUMBER-GENERATOR +MT19937+ 0)))
                           (LOOP FOR I FROM 0 TO 10 COLLECT
                                 (sample rng :gaussian-ziggurat :sigma 10.0d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.9772498680518208d0)
                        (MULTIPLE-VALUE-LIST (UGAUSSIAN-P 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.022750131948179212d0)
                        (MULTIPLE-VALUE-LIST (UGAUSSIAN-Q 2.0d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 2.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (UGAUSSIAN-PINV
                                                            0.9772498680518208d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 2.0d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (UGAUSSIAN-QINV
                                                            0.02275013194817921d0))))


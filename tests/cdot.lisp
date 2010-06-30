;; Regression test CDOT for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST CDOT
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-1.1684039224427728d28 0.0d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (GRID:MAKE-FOREIGN-ARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5 8.24 3.29 -8.93 34.12
                                               -6.15 49.27 -13.49 32.5 42.73
                                               -17.24 43.31 -16.12 -8.25 21.44
                                               -49.08)))
                               (V2
                                (GRID:MAKE-FOREIGN-ARRAY '(COMPLEX SINGLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(32.5 42.73 -17.24 43.31 -16.12
                                               -8.25 21.44 -49.08 -39.66 -49.46
                                               19.68 -5.55 -8.82 25.37 -30.58
                                               31.67))))
                           (CDOT V1 V2))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST #C(-6252.624d0 -6236.050300000001d0))
                        (MULTIPLE-VALUE-LIST
                         (LET ((V1
                                (GRID:MAKE-FOREIGN-ARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(-34.5d0 8.24d0 3.29d0 -8.93d0
                                               34.12d0 -6.15d0 49.27d0 -13.49d0
                                               32.5d0 42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0
                                               -49.08d0)))
                               (V2
                                (GRID:MAKE-FOREIGN-ARRAY '(COMPLEX DOUBLE-FLOAT)
                                             :INITIAL-CONTENTS
                                             '(32.5d0 42.73d0 -17.24d0 43.31d0
                                               -16.12d0 -8.25d0 21.44d0
                                               -49.08d0 -39.66d0 -49.46d0
                                               19.68d0 -5.55d0 -8.82d0 25.37d0
                                               -30.58d0 31.67d0))))
                           (CDOT V1 V2)))))


;; Regression test ELLIPTIC-FUNCTIONS for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST ELLIPTIC-FUNCTIONS
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST 0.19762082367187703d0 0.9802785369736752d0
                              0.9840560289645665d0 0.0d0 0.0d0 0.0d0)
                        (MULTIPLE-VALUE-LIST
                         (JACOBIAN-ELLIPTIC-FUNCTIONS 0.2d0 0.81d0)))
                       (LISP-UNIT:ASSERT-ERROR 'INPUT-DOMAIN
                                               (JACOBIAN-ELLIPTIC-FUNCTIONS
                                                0.61802d0 1.5d0)))


;; Additional methods for lisp-unit
;; Liam Healy 2009-04-15 23:23:30EDT augment.lisp
;; Time-stamp: <2009-12-27 10:12:13EST augment.lisp>
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

(defmethod lisp-unit:numerical-equal
    ((result1 marray) (result2 marray) &key (test #'lisp-unit:number-equal))
  "Return true if the arrays are numerically equal according to :TEST."
  (when (equal (dimensions result1) (dimensions result2))
    (lisp-unit:numerical-equal (cl-array result1) (cl-array result2)
			       :test test)))

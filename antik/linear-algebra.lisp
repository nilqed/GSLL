;; Define generic operators for GSL functions
;; Liam Healy 2011-01-01 09:51:47EST linear-algebra.lisp
;; Time-stamp: <2011-01-01 11:48:49EST linear-algebra.lisp>

;; Copyright 2011 Liam M. Healy
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

(in-package :antik)

(defmethod *i ((a grid:foreign-array) (b grid:foreign-array))
  (gsl:matrix-product a b))

(defmethod *i ((a grid:foreign-array) (b number))
  (gsl:scale b a))

(defmethod *i ((a number) (b grid:foreign-array))
  (gsl:scale a b))

(defmethod /i ((a grid:foreign-array) (b number))
  (gsl:scale (cl:/ b) a))

(defmethod +i ((a grid:foreign-array) (b grid:foreign-array))
  (gsl:elt+ a b))

(defmethod +i ((a grid:foreign-array) (b number))
  (gsl:elt+ a b))

(defmethod +i ((a number) (b grid:foreign-array))
  (gsl:elt+ b a))

(defmethod -i ((a grid:foreign-array) (b grid:foreign-array))
  (gsl:elt- a b))

(defmethod -i ((a grid:foreign-array) (b number))
  (gsl:elt+ a (cl:- b)))

(defmethod -i ((a number) (b grid:foreign-array))
  (gsl:elt+ (- b) a))

(defmethod expt ((a grid:matrix) (b integer))
  (cond ((= 1 b) a)
	((plusp b)		   ; use addition-chain exponentiation
	 (* a (expt a (1- b))))
	((minusp b)
	 (expt (invert-matrix a) (- b)))
	;; zero exponent; need to figure out non-square matrix
	(t (grid:identity-matrix (grid:dim0 a)))))

;;; Invert a matrix using LU
(export 'invert-matrix)
(defun invert-matrix (mat)
  "Invert the matrix."
  (let* ((dim (grid:dim0 mat))
	 (per (gsl:make-permutation dim))
	 (inv (grid:make-foreign-array 'double-float :dimensions (list dim dim))))
    (gsl:lu-decomposition mat per)
    (gsl:lu-invert mat per inv)))

#|
;;; Examples and unit test
;;; These are direct tests of matrix inversion
(save-test
 lu
 (grid:copy-to
  (invert-matrix
   (grid:make-foreign-array 'double-float
		:dimensions  '(2 2)
		:initial-contents '(1.0d0 2.0d0 3.0d0 4.0d0)))))

  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((-1.9999999999999998d0 1.0d0)
	(1.4999999999999998d0 -0.49999999999999994d0)))
   (MULTIPLE-VALUE-LIST
    (GRID:COPY-TO
     (INVERT-MATRIX
      (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :DIMENSIONS '(2 2)
			       :INITIAL-CONTENTS
			       '((1.0d0 2.0d0) (3.0d0 4.0d0)))))))
|#


;; Define generic operators for GSL functions
;; Liam Healy 2011-01-01 09:51:47EST linear-algebra.lisp
;; Time-stamp: <2011-01-01 10:58:18EST linear-algebra.lisp>
;; $Id: $

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
	 (expt (gsl:invert-matrix a) (- b)))
	;; zero exponent; need to figure out non-square matrix
	(t (grid:identity-matrix (grid:dim0 a)))))

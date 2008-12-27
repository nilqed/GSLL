;; Series acceleration.
;; Liam Healy, Wed Nov 21 2007 - 18:41
;; Time-stamp: <2008-12-26 18:22:40EST series-acceleration.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_sum.h

;;;;****************************************************************************
;;;; Creation and calculation of Levin series acceleration
;;;;****************************************************************************

(cffi:defcstruct levin-c
  "The definition of Levin series acceleration for GSL."
  (size sizet)
  (position-in-array sizet)
  (terms-used sizet)
  (sum-plain :double)
  (q-num :pointer)
  (q-den :pointer)
  (dq-num :pointer)
  (dq-den :pointer)
  (dsum :pointer))

(defmobject levin "gsl_sum_levin_u"
  ((order sizet))
  "Levin u-transform"			; FDL
  "Make a workspace for a Levin u-transform of n
   terms.  The size of the workspace is O(2n^2 + 3n).")

(defmfun accelerate (array levin)
  "gsl_sum_levin_u_accel"
  (((c-pointer array) :pointer) ((dim0 array) sizet) ((mpointer levin) :pointer)
   (accelerated-sum :double) (absolute-error :double))
  :documentation			; FDL
  "From the terms of a series in array, compute the extrapolated
   limit of the series using a Levin u-transform.  Additional
   working space must be provided in levin.  The extrapolated sum is
   returned with an estimate of the absolute error.  The actual
   term-by-term sum is returned in 
   w->sum_plain. The algorithm calculates the truncation error
   (the difference between two successive extrapolations) and round-off
   error (propagated from the individual terms) to choose an optimal
   number of terms for the extrapolation.")

;;;;****************************************************************************
;;;; Acceleration with error estimation from truncation
;;;;****************************************************************************

(defmobject levin-truncated "gsl_sum_levin_utrunc"
  ((order sizet))
  "truncated Levin u-transform"			; FDL
  "Make a workspace for a Levin u-transform of n
   terms, without error estimation.  The size of the workspace is
   O(3n).")

(defmfun accelerate-truncated (array levin)
  "gsl_sum_levin_utrunc_accel"
  (((c-pointer array) :pointer) ((dim0 array) sizet) ((mpointer levin) :pointer)
   (accelerated-sum :double) (absolute-error :double))
  :documentation			; FDL
  "From the terms of a series in array, compute the extrapolated
   limit of the series using a Levin u-transform.  Additional
   working space must be provided in levin.  The extrapolated sum is
   returned with an estimate of the absolute error.  The actual
   term-by-term sum is returned in w->sum_plain. The algorithm
   terminates when the difference between two successive extrapolations
   reaches a minimum or is sufficiently small. The difference between
   these two values is used as estimate of the error and is stored in
   abserr_trunc.  To improve the reliability of the algorithm the
   extrapolated values are replaced by moving averages when calculating
   the truncation error, smoothing out any fluctuations.")

;;;;****************************************************************************
;;;; Example
;;;;****************************************************************************

;;; From Sec. 29.3 in the GSL manual.

(defun acceleration-example ()
  (let ((maxterms 20)
	(sum 0.0d0)
	(zeta2 (/ (expt pi 2) 6)))
    (let ((levin (make-levin maxterms))
	  (array (make-marray 'double-float :dimensions maxterms)))
      (dotimes (n maxterms)
	(setf (maref array n) (coerce (/ (expt (1+ n) 2)) 'double-float))
	(incf sum (maref array n)))
      (multiple-value-bind (accelerated-sum error)
	  (accelerate array levin)
	(format t "~&term-by-term sum =~f using ~d terms" sum maxterms)
	(format t "~&term-by-term sum =~f using ~d terms"
		(cffi:foreign-slot-value levin 'levin-c 'sum-plain)
		(cffi:foreign-slot-value levin 'levin-c 'terms-used))
	(format t "~&exact value     = ~f" zeta2)
	(format t "~&accelerated sum = ~f using ~d terms"
		accelerated-sum
		(cffi:foreign-slot-value levin 'levin-c 'terms-used))
	(format t "~&estimated error = ~f" error)
	(format t "~&actual error = ~f" (- accelerated-sum zeta2))))))

;; term-by-term sum =1.5961632439130233 using 20 terms
;; term-by-term sum =1.5759958390005426 using 13 terms
;; exact value     = 1.6449340668482264
;; accelerated sum = 1.6449340669228176 using 13 terms
;; estimated error = 0.00000000008883604962761638
;; actual error = 0.00000000007459122208786084

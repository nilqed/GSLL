;; Example spline
;; Liam Healy, Sat Nov 10 2007 - 21:18
;; Time-stamp: <2009-02-16 09:46:57EST spline-example.lisp>
;; $Id$

(in-package :gsl)

(defun spline-example (&optional (step 0.01d0))
  "The first example in Sec. 26.7 of the GSL manual."
  (let* ((acc (make-acceleration))
	 (xarr
	  (make-marray
	   'double-float
	   :initial-contents
	   (loop for i from 0.0d0 below 10.0d0
	      collect (+ i (* 0.5d0 (sin i))))))
	 (yarr
	  (make-marray
	   'double-float
	   :initial-contents
	   (loop for i from 0.0d0 below 10.0d0
	      collect (+ i (cos (expt i 2))))))
	 (spline (make-spline +cubic-spline-interpolation+ xarr yarr)))
    (loop for xi from (maref xarr 0) below (maref xarr 9) by step
       collect (list xi (evaluate spline xi :acceleration acc)))))

(save-test interpolation (spline-example 0.1d0))

(defun evaluate-integral-example (&optional (intervals 4))
  "Evaluate integral of sin(x) in interval 0-pi.  sin(x) is tabulated
over a 0-2pi interval and interpolated with
+periodic-cubic-spline-interpolation+"
  (let* ((nodes (1+ intervals))
	 (max-node (1- nodes))
	 (xarr 
	  (loop 
	     with step = (/ (* 2.0 pi) intervals)
	     for i from 0 upto max-node
	     collect (* i step)))
	 (xmarr (make-marray 'double-float :initial-contents xarr))
	 ;; cannot use (loop for x on (cl-array xmarr)...) -- c function gives error
	 (ymarr 
	  (make-marray 'double-float :initial-contents
		       (loop for x in xarr
			  collect (sin x))))
	 (acc (make-acceleration))
	 (spline (make-spline +periodic-cubic-spline-interpolation+ xmarr ymarr)))
  (evaluate-integral spline 0d0 (coerce pi 'double-float) :acceleration acc)))

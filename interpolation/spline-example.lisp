;; Example spline
;; Liam Healy, Sat Nov 10 2007 - 21:18
;; Time-stamp: <2009-11-11 23:19:51EST spline-example.lisp>

(in-package :gsl)

(defun spline-example (&optional (step 0.01d0))
  "The first example in Sec. 26.7 of the GSL manual."
  (let* ((xarr
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
       collect (list xi (evaluate spline xi)))))

(defun evaluate-integral-example (&optional (intervals 4))
  "Evaluate integral of sin(x) in interval 0-pi.  sin(x) is tabulated
   over a 0-2pi interval and interpolated with
   +periodic-cubic-spline-interpolation+"
  (let* ((xarr 
	  (loop with step = (/ (* 2.0 pi) intervals)
	     for i from 0 upto intervals
	     collect (* i step)))
	 (xmarr (make-marray 'double-float :initial-contents xarr))
	 (ymarr
	  (make-marray 'double-float :initial-contents (mapcar 'sin xarr)))
	 (spline
	  (make-spline +periodic-cubic-spline-interpolation+ xmarr ymarr)))
    (evaluate-integral spline 0d0
		       #+clisp (coerce pi 'double-float) ;; pi=3.14..L0 on clisp
		       #-clisp pi)))

(save-test interpolation
 (spline-example 0.1d0)
 (evaluate-integral-example 100))

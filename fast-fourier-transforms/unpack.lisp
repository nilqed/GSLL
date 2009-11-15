;; Unpack functions for FFT vectors.
;; Sumant Oemrawsingh, Sun Oct 25 2009 - 16:35
;; Time-stamp: <2009-11-08 21:10:27EST unpack.lisp>

(in-package :gsl)

;; /usr/include/gsl/gsl_fft_real.h
;; /usr/include/gsl/gsl_fft_real_float.h
;; /usr/include/gsl/gsl_fft_complex.h
;; /usr/include/gsl/gsl_fft_complex_float.h

(defmfun fft-real-unpack
    ((vector vector)
     &key (stride 1)
     (output
	 (eltcase single-float
		  (make-marray '(complex single-float) :dimensions (size vector))
		  t
		  (make-marray '(complex double-float) :dimensions (size vector)))))
  ("gsl_fft_real" :type "_unpack")
  (((c-pointer vector) :pointer)
   ((c-pointer output) :pointer)
   (stride sizet) ((floor (size vector) stride) sizet))
  :definition :generic
  :element-types :float
  :inputs (vector output)
  :outputs (output)
  :return (output)
  :export nil
  :index unpack
  :documentation
  "This function converts a single real array into an equivalent complex
  array (with imaginary part set to zero), suitable for fft-complex
  routines.")

(defmfun fft-half-complex-radix2-unpack
    ((vector vector)
     &key (stride 1)
     (output
	 (eltcase single-float
		  (make-marray '(complex single-float) :dimensions (size vector))
		  t
		  (make-marray '(complex double-float) :dimensions (size vector)))))
  ("gsl_fft_halfcomplex" :type "_radix2_unpack")
  (((c-pointer vector) :pointer)
   ((c-pointer output) :pointer)
   (stride sizet) ((floor (size vector) stride) sizet))
  :definition :generic
  :element-types :float
  :inputs (vector output)
  :outputs (output)
  :return (output)
  :export nil
  :index unpack
  :documentation
  "Convert an array of half-complex coefficients as returned by
  real-fft-radix2-transform, into an ordinary complex array.")

(defmfun fft-half-complex-unpack
    ((vector vector)
     &key (stride 1)
     (output
	 (eltcase single-float
		  (make-marray '(complex single-float) :dimensions (size vector))
		  t
		  (make-marray '(complex double-float) :dimensions (size vector)))))
  ("gsl_fft_halfcomplex" :type "_unpack")
  (((c-pointer vector) :pointer)
   ((c-pointer output) :pointer)
   (stride sizet) ((floor (size vector) stride) sizet))
  :definition :generic
  :element-types :float
  :inputs (vector output)
  :outputs (output)
  :return (output)
  :export nil
  :index unpack
  :documentation
  "This function converts an array of half-complex coefficients as
  returned by fft-real-transform, into an ordinary complex array. It
  fills in the complex array using the symmetry z_k = z_{n-k}^* to
  reconstruct the redundant elements.")

(export 'unpack)
(defun unpack
    (vector &rest args &key (stride 1) (unpack-type 'real) &allow-other-keys)
  (let ((pass-on-args (copy-list args)))
    (remf pass-on-args :unpack-type)
    (if (eq unpack-type 'complex)
	(if (power-of-2-p (floor (size vector) stride))
	    (apply 'fft-half-complex-radix2-unpack vector pass-on-args)
	    (apply 'fft-half-complex-unpack vector pass-on-args))
	(apply 'fft-real-unpack vector pass-on-args))))

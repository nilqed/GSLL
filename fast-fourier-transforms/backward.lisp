;; Backward FFT
;; Sumant Oemrawsingh, Sat Oct 24 2009 - 12:55
;; Time-stamp: <2009-11-09 16:30:05EST backward.lisp>

(in-package :gsl)

;; /usr/include/gsl/gsl_fft_complex.h
;; /usr/include/gsl/gsl_fft_complex_float.h
;; /usr/include/gsl/gsl_fft_halfcomplex.h
;; /usr/include/gsl/gsl_fft_halfcomplex_float.h

;;;;****************************************************************************
;;;; Backward transformation
;;;;****************************************************************************

;;; Backward transformations where the size is a power of 2
(defmfun backward-fourier-transform-radix2
  ((vector vector) &key (stride 1))
  ("gsl_fft" :type "_radix2_backward")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index backward-fourier-transform
  :documentation
  "Backward FFT on a vector for which (floor length stride) is a power of 2.")

(defmfun backward-fourier-transform-nonradix2
    ((vector vector) &key (stride 1)
     (wavetable (make-fft-wavetable element-type (floor (size vector) stride)))
     (workspace (make-fft-workspace element-type (floor (size vector) stride))))
  ("gsl_fft" :type "_backward")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index backward-fourier-transform
  :documentation
  "Backward FFT on a complex vector for which (floor length stride) is
  not a power of 2.")

;;;;****************************************************************************
;;;; Half-complex  
;;;;****************************************************************************

(defmfun backward-fourier-transform-halfcomplex-radix2
    ((vector vector) &key (stride 1))
  ("gsl_fft_halfcomplex" :type "_radix2_backward")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index backward-fourier-transform
  :documentation
  "Backward FFT on a vector for which (floor length stride) is a power of 2,
   in half complex form.")

(defmfun backward-fourier-transform-halfcomplex-nonradix2
    ((vector vector) &key (stride 1)
     (wavetable (make-fft-wavetable element-type (floor (size vector) stride) t))
     (workspace (make-fft-workspace element-type (floor (size vector) stride))))
  ("gsl_fft_halfcomplex" :type "_backward")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index backward-fourier-transform
  :documentation
  "Backward FFT on a vector for which (floor length stride) is not a power of 2,
   in half complex form.")

;;;;****************************************************************************
;;;; Decimation-in-frequency backward FFT
;;;;****************************************************************************

(defmfun backward-fourier-transform-dif-radix2
  ((vector vector) &key (stride 1))
  ("gsl_fft" :type "_radix2_dif_backward")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index backward-fourier-transform
  :documentation
  "Backward decimation-in-frequency FFT on a vector for which (floor
  length stride) is a power of 2.")

;;;;****************************************************************************
;;;; Unified function backward-fourier-transform
;;;;****************************************************************************

;;; This selects among the 10 backward FFT functions that GSL defines.
(export 'backward-fourier-transform)
(defun backward-fourier-transform
    (vector &rest args
     &key decimation-in-frequency (stride 1) &allow-other-keys)
  "Perform a backward fast Fourier transform on the given vector. If
  the length of the vector is not a power of 2, and the user has a
  suitable wavetable and/or workspace, these can be supplied as
  keyword arguments.  If the vector is real, it is assumed to be
  in half-complex form."
  (let ((pass-on-args (copy-list args)))
    (remf pass-on-args :half-complex)
    (remf pass-on-args :decimation-in-frequency)
    (if (power-of-2-p (floor (size vector) stride))
	(if (subtypep (element-type vector) 'real)
	    (apply 'backward-fourier-transform-halfcomplex-radix2
		   vector pass-on-args)
	    (if decimation-in-frequency
		(apply 'backward-fourier-transform-dif-radix2 vector pass-on-args)
		(apply 'backward-fourier-transform-radix2 vector pass-on-args)))
	(if (subtypep (element-type vector) 'real)
	    (apply 'backward-fourier-transform-halfcomplex-nonradix2
		   vector pass-on-args)
	    (apply 'backward-fourier-transform-nonradix2 vector pass-on-args)))))

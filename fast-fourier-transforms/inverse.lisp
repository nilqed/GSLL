;; Inverse FFT
;; Sumant Oemrawsingh, Sat Oct 24 2009 - 12:55
;; Time-stamp: <2009-11-09 16:31:22EST inverse.lisp>

(in-package :gsl)

;; /usr/include/gsl/gsl_fft_complex.h
;; /usr/include/gsl/gsl_fft_complex_float.h
;; /usr/include/gsl/gsl_fft_halfcomplex.h
;; /usr/include/gsl/gsl_fft_halfcomplex_float.h

;;;;****************************************************************************
;;;; Inverse transformation
;;;;****************************************************************************

;;; Inverse transformations where the size is a power of 2
(defmfun inverse-fourier-transform-radix2
  ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index inverse-fourier-transform
  :documentation
  "Inverse FFT on a vector for which (floor length stride) is a power of 2.")

(defmfun inverse-fourier-transform-nonradix2
    ((vector vector) &key (stride 1) (n (size vector))
     (wavetable (make-fft-wavetable element-type (size vector)))
     (workspace (make-fft-workspace element-type (size vector))))
  ("gsl_fft" :type "_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index inverse-fourier-transform
  :documentation
  "Inverse FFT on a complex vector for which (floor length stride) is
  not a power of 2.")

;;;;****************************************************************************
;;;; Half-complex  
;;;;****************************************************************************

(defmfun inverse-fourier-transform-halfcomplex-radix2
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft_halfcomplex" :type "_radix2_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index inverse-fourier-transform
  :documentation
  "Inverse FFT on a vector for which (floor length stride) is a power of 2,
   in half complex form.")

(defmfun inverse-fourier-transform-halfcomplex-nonradix2
    ((vector vector) &key (stride 1) (n (size vector))
     (wavetable (make-fft-wavetable element-type (size vector) t))
     (workspace (make-fft-workspace element-type (size vector))))
  ("gsl_fft_halfcomplex" :type "_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index inverse-fourier-transform
  :documentation
  "Inverse FFT on a vector for which (floor length stride) is not a power of 2,
   in half complex form.")

;;;;****************************************************************************
;;;; Decimation-in-frequency inverse FFT
;;;;****************************************************************************

(defmfun inverse-fourier-transform-dif-radix2
  ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_dif_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index inverse-fourier-transform
  :documentation
  "Inverse decimation-in-frequency FFT on a vector for which (floor
  length stride) is a power of 2.")

;;;;****************************************************************************
;;;; Unified function inverse-fourier-transform
;;;;****************************************************************************

;;; This selects among the 10 inverse FFT functions that GSL defines.
(export 'inverse-fourier-transform)
(defun inverse-fourier-transform
    (vector &rest args
     &key decimation-in-frequency (stride 1) &allow-other-keys)
  "Perform a inverse fast Fourier transform on the given vector. If
  the length of the vector is not a power of 2, and the user has a
  suitable wavetable and/or workspace, these can be supplied as
  keyword arguments.  If the vector is real, it is assumed to be
  in half-complex form."
  (let ((pass-on-args (copy-list args)))
    (remf pass-on-args :half-complex)
    (remf pass-on-args :decimation-in-frequency)
    (if (power-of-2-p (floor (size vector) stride))
	(if (subtypep (element-type vector) 'real)
	    (apply 'inverse-fourier-transform-halfcomplex-radix2
		   vector pass-on-args)
	    (if decimation-in-frequency
		(apply 'inverse-fourier-transform-dif-radix2 vector pass-on-args)
		(apply 'inverse-fourier-transform-radix2 vector pass-on-args)))
	(if (subtypep (element-type vector) 'real)
	    (apply 'inverse-fourier-transform-halfcomplex-nonradix2
		   vector pass-on-args)
	    (apply 'inverse-fourier-transform-nonradix2 vector pass-on-args)))))

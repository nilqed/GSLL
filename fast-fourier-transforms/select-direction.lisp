;; FFT where direction is selected.
;; Sumant Oemrawsingh, Sat Oct 24 2009 - 12:55
;; Time-stamp: <2009-11-07 10:22:21EST select-direction.lisp>

;; /usr/include/gsl/gsl_fft_complex.h
;; /usr/include/gsl/gsl_fft_complex_float.h

(in-package :gsl)

(defmfun fourier-transform-radix2
    ((vector vector) direction &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_transform")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet) (direction fft-direction))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index fourier-transform
  :documentation
  "FFT in the given direction for a complex radix-2 vector")

(defmfun fourier-transform-dif-radix2
    ((vector vector) direction &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_dif_transform")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet) (direction fft-direction))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :export nil
  :index fourier-transform
  :documentation
  "Decimation-in-frequency version of the FFT in the given direction for a
  complex radix-2 vector")

(export 'fourier-transform)
(defun fourier-transform
    (vector direction &rest args
     &key decimation-in-frequency (stride 1) &allow-other-keys)
  "Perform a fast Fourier transform on the given vector in the
   selected direction.  The direction argument is one of
   :backward or :forward."
  (let ((pass-on-args (copy-list args)))
    (remf pass-on-args :half-complex)
    (remf pass-on-args :decimation-in-frequency)
    (if (power-of-2-p (floor (size vector) stride))
	(if decimation-in-frequency
	    (apply 'fourier-transform-dif-radix2 vector direction pass-on-args)
	    (apply 'fourier-transform-radix2 vector direction pass-on-args))
	(error "Unable to do direction-switchable Fourier transform on ~
	vector that is not a power of 2 in length; use one of ~
	forward-fourier-transform, backward-fourier-transform, ~
	or inverse-fourier-transform."))))

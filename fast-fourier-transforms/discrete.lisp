;; Discrete Fourier Transforms
;; Liam Healy 2009-11-07 14:24:07EST
;; Time-stamp: <2009-11-07 17:02:19EST discrete.lisp>

;;; These functions are not documented in GSL and appear to be present
;;; only to provide a check on the FFT routines.

(in-package :gsl)

;; /usr/include/gsl/gsl_dft_complex.h
;; /usr/include/gsl/gsl_dft_complex_float.h

(defmfun forward-discrete-fourier-transform
    ((vector vector)
     &key (stride 1)
     (result (make-marray element-type :dimensions (dimensions vector))))
  ("gsl_dft" :type "_forward")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet)
   ((c-pointer result) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (result)
  :return (result)
  :documentation
  "Forward discrete Fourier transform provided to check the FFT
  routines.")

(defmfun backward-discrete-fourier-transform
    ((vector vector)
     &key (stride 1)
     (result (make-marray element-type :dimensions (dimensions vector))))
  ("gsl_dft" :type "_backward")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet)
   ((c-pointer result) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (result)
  :return (result)
  :documentation
  "Backward discrete Fourier transform provided to check the FFT
  routines.")

(defmfun inverse-discrete-fourier-transform
    ((vector vector)
     &key (stride 1)
     (result (make-marray element-type :dimensions (dimensions vector))))
  ("gsl_dft" :type "_inverse")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet)
   ((c-pointer result) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (result)
  :return (result)
  :documentation
  "Inverse discrete Fourier transform provided to check the FFT
  routines.")

(defmfun discrete-fourier-transform
    ((vector vector)
     &key (stride 1)
     (result (make-marray element-type :dimensions (dimensions vector))))
  ("gsl_dft" :type "_transform")
  (((c-pointer vector) :pointer) (stride sizet) ((floor (size vector) stride) sizet)
   ((c-pointer result) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (result)
  :return (result)
  :documentation
  "Discrete Fourier transform in selectable direction provided to
  check the FFT routines.")

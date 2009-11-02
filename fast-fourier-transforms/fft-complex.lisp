;; Functions for fast fourier transforms on complex data.
;; Sumant Oemrawsingh, Sat Oct 24 2009 - 12:55
;; Time-stamp: <2009-11-01 22:46:09EST fft-complex.lisp>

;; /usr/include/gsl/gsl_fft_complex.h
;; /usr/include/gsl/gsl_fft_complex_float.h

(in-package :gsl)

;; Power of 2 functions

(defmfun fft-complex-radix2-backward
  ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_backward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Backward FFT for a complex radix-2 vector")

(defmfun fft-complex-radix2-inverse
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Inverse FFT for a complex radix-2 vector")

(defmfun fft-complex-radix2-transform
    ((vector vector) direction &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_transform")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet) (direction fft-direction))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "FFT in the given direction for a complex radix-2 vector")

;; These are decimation-in-frequency versions of the radix-2 FFT functions.
;; I don't know what they are and how they are supposed to work, so I don't
;; know if they work.

(defmfun fft-complex-radix2-dif-forward
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_dif_forward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Decimation-in-frequency version of the forward FFT for a complex radix-2 vector")

(defmfun fft-complex-radix2-dif-backward
  ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_dif_backward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Decimation-in-frequency version of the backward FFT for a complex radix-2 vector")

(defmfun fft-complex-radix2-dif-inverse
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Decimation-in-frequency version of the inverse FFT for a complex radix-2 vector")

(defmfun fft-complex-radix2-dif-transform
    ((vector vector) direction &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_dif_transform")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet) (direction fft-direction))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Decimation-in-frequency version of the FFT in the given direction for a
  complex radix-2 vector")

;; Mixed Radix general-N functions

(defmobject fft-complex-wavetable-double-float
    "gsl_fft_complex_wavetable" ((n sizet))
  "structure that holds the factorization and trigonometric lookup tables for
  the mixed radix complex fft algorithm"
  :documentation
  "This function prepares a trigonometric lookup table for a complex FFT of
  length n. The function returns a pointer to the newly allocated
  gsl_fft_complex_wavetable if no errors were detected, and a null pointer in
  the case of error. The length n is factorized into a product of
  subtransforms, and the factors and their trigonometric coefficients are
  stored in the wavetable. The trigonometric coefficients are computed using
  direct calls to sin and cos, for accuracy. Recursion relations could be used
  to compute the lookup table faster, but if an application performs many FFTs
  of the same length then this computation is a one-off overhead which does
  not affect the final throughput.
  
  The wavetable structure can be used repeatedly for any transform of the same
  length. The table is not modified by calls to any of the other FFT
  functions. The same wavetable can be used for both forward and backward (or
  inverse) transforms of a given length.")

(defmobject fft-complex-wavetable-single-float
    "gsl_fft_complex_wavetable_float" ((n sizet))
  "structure that holds the factorization and trigonometric lookup tables for
  the mixed radix complex float fft algorithm"
  :documentation
  "This function prepares a trigonometric lookup table for a complex float FFT
  of length n. The function returns a pointer to the newly allocated
  gsl_fft_complex_wavetable if no errors were detected, and a null pointer in
  the case of error. The length n is factorized into a product of
  subtransforms, and the factors and their trigonometric coefficients are
  stored in the wavetable. The trigonometric coefficients are computed using
  direct calls to sin and cos, for accuracy. Recursion relations could be used
  to compute the lookup table faster, but if an application performs many FFTs
  of the same length then this computation is a one-off overhead which does
  not affect the final throughput.
  
  The wavetable structure can be used repeatedly for any transform of the same
  length. The table is not modified by calls to any of the other FFT
  functions. The same wavetable can be used for both forward and backward (or
  inverse) transforms of a given length.")

(defmobject fft-complex-workspace-double-float
    "gsl_fft_complex_workspace" ((n sizet))
  "Structure that holds the additional working space required for the
  intermediate steps of the mixed radix complex fft algoritms"
  :documentation
  "This function allocates a workspace for a complex transform of length n.")

(defmobject fft-complex-workspace-single-float
    "gsl_fft_complex_workspace_float" ((n sizet))
  "Structure that holds the additional working space required for the
  intermediate steps of the mixed radix complex float fft algoritms"
  :documentation
  "This function allocates a workspace for a complex float transform of length
  n.")

(defmfun fft-complex-backward
    ((vector vector) &key (stride 1) (n (size vector))
     (wavetable
      (eltcase (complex single-float) (make-fft-complex-wavetable-single-float (size vector))
	       (complex double-float) (make-fft-complex-wavetable-double-float (size vector))))
     (workspace
      (eltcase (complex single-float) (make-fft-complex-workspace-single-float (size vector))
	       (complex double-float) (make-fft-complex-workspace-double-float (size vector)))))
  ("gsl_fft" :type "_backward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Backward FFT for a complex vector")

(defmfun fft-complex-inverse
    ((vector vector) &key (stride 1) (n (size vector))
                     (wavetable (eltcase (complex single-float) (make-fft-complex-wavetable-single-float (size vector))
                                         (complex double-float) (make-fft-complex-wavetable-double-float (size vector))))
                     (workspace (eltcase (complex single-float) (make-fft-complex-workspace-single-float (size vector))
                                         (complex double-float) (make-fft-complex-workspace-double-float (size vector)))))
  ("gsl_fft" :type "_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Inverse FFT for a complex vector")

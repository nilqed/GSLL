;; Functions for fast fourier transforms on real data.
;; Sumant Oemrawsingh, Sat Oct 31 2009 - 20:12
;; Time-stamp: <2009-11-01 23:04:46EST fft-half-complex.lisp>

;; /usr/include/gsl/gsl_fft_halfcomplex.h
;; /usr/include/gsl/gsl_fft_halfcomplex_float.h

(in-package :gsl)

;; Power of 2 functions

(defmfun fft-half-complex-radix2-backward
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft_halfcomplex" :type "_radix2_backward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Backward FFT for a half-complex radix-2 vector")

(defmfun fft-half-complex-radix2-inverse
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft_halfcomplex" :type "_radix2_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Inverse FFT for a half-complex radix-2 vector")

(defmfun fft-half-complex-radix2-unpack
         ((h-complex vector)
          &key (stride 1) (n (expt 2 (floor (log (size h-complex) 2))))
          (complex-output (eltcase single-float (make-marray '(complex single-float) :dimensions n)
                                        t (make-marray '(complex double-float) :dimensions n))))
  ("gsl_fft_halfcomplex" :type "_radix2_unpack")
  (((c-pointer h-complex) :pointer) ((c-pointer complex-output) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :float
  :inputs (h-complex complex-output)
  :outputs (complex-output)
  :return (complex-output)
  :documentation
  "Convert an array of half-complex coefficients as returned by
  real-fft-radix2-transform, into an ordinary complex array.")

;; Mixed Radix general-N functions

(defmobject fft-half-complex-wavetable-double-float
    "gsl_fft_halfcomplex_wavetable" ((n sizet))
  "structure that holds the factorization and trigonometric lookup tables for
  the mixed radix halfcomplex fft algorithm"
  :documentation
  "These functions prepare trigonometric lookup tables for an FFT of size n
  real elements. The functions return a pointer to the newly allocated struct
  if no errors were detected, and a null pointer in the case of error. The
  length n is factorized into a product of subtransforms, and the factors and
  their trigonometric coefficients are stored in the wavetable. The
  trigonometric coefficients are computed using direct calls to sin and cos,
  for accuracy. Recursion relations could be used to compute the lookup table
  faster, but if an application performs many FFTs of the same length then
  computing the wavetable is a one-off overhead which does not affect the
  final throughput.

  The wavetable structure can be used repeatedly for any transform of the same
  length. The table is not modified by calls to any of the other FFT
  functions. The appropriate type of wavetable must be used for forward real
  or inverse half-complex transforms.")

(defmobject fft-half-complex-wavetable-single-float
    "gsl_fft_halfcomplex_wavetable_float" ((n sizet))
  "structure that holds the factorization and trigonometric lookup tables for
  the mixed radix real float fft algorithm"
  :documentation
  "These functions prepare trigonometric lookup tables for an FFT of size n
  real float elements. The functions return a pointer to the newly allocated
  struct if no errors were detected, and a null pointer in the case of error.
  The length n is factorized into a product of subtransforms, and the factors
  and their trigonometric coefficients are stored in the wavetable. The
  trigonometric coefficients are computed using direct calls to sin and cos,
  for accuracy. Recursion relations could be used to compute the lookup table
  faster, but if an application performs many FFTs of the same length then
  computing the wavetable is a one-off overhead which does not affect the
  final throughput.

  The wavetable structure can be used repeatedly for any transform of the same
  length. The table is not modified by calls to any of the other FFT
  functions. The appropriate type of wavetable must be used for forward real
  or inverse half-complex transforms.")

(defmfun fft-half-complex-backward
    ((vector vector) &key (stride 1) (n (size vector))
     (wavetable
      (eltcase single-float (make-half-complex-wavetable-single-float (size vector))
	       double-float (make-half-complex-wavetable-double-float (size vector))))
     (workspace
      (eltcase single-float (make-real-workspace-single-float (size vector))
	       double-float (make-real-workspace-double-float (size vector)))))
  ("gsl_fft_halfcomplex" :type "_backward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Forward FFT for a half-complex vector")

(defmfun fft-half-complex-inverse
    ((vector vector) &key (stride 1) (n (size vector))
     (wavetable
      (eltcase single-float (make-half-complex-wavetable-single-float (size vector))
	       double-float (make-half-complex-wavetable-double-float (size vector))))
     (workspace
      (eltcase single-float (make-real-workspace-single-float (size vector))
	       double-float (make-real-workspace-double-float (size vector)))))
  ("gsl_fft_halfcomplex" :type "_inverse")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Forward FFT for a half-complex vector")

(defmfun fft-half-complex-unpack
         ((h-complex vector)
          &key (stride 1) (n (size h-complex))
          (complex-output (eltcase single-float (make-marray '(complex single-float) :dimensions n)
                                        t (make-marray '(complex double-float) :dimensions n))))
  ("gsl_fft_halfcomplex" :type "_unpack")
  (((c-pointer h-complex) :pointer) ((c-pointer complex-output) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :float
  :inputs (h-complex complex-output)
  :outputs (complex-output)
  :return (complex-output)
  :documentation
  "This function converts an array of half-complex
  coefficients as returned by fft-real-transform, into an ordinary complex
  array. It fills in the complex array using the symmetry z_k = z_{n-k}^* to
  reconstruct the redundant elements.")

;; Functions for fast fourier transforms on real data.
;; Sumant Oemrawsingh, Sun Oct 25 2009 - 16:35

;; /usr/include/gsl/gsl_fft_real.h
;; /usr/include/gsl/gsl_fft_real_float.h

(in-package :gsl)

;; Power of 2 functions

(defmfun fft-real-radix2-transform
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft_real" :type "_radix2_transform")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Forward FFT for a real radix-2 vector")

;; Mixed Radix general-N functions

(defmobject fft-real-wavetable
    "gsl_fft_real_wavetable" ((n sizet))
  "structure that holds the factorization and trigonometric lookup tables for
  the mixed radix real fft algorithm"
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

(defmobject fft-real-wavetable-float
    "gsl_fft_real_wavetable_float" ((n sizet))
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

(defmobject fft-real-workspace
    "gsl_fft_real_workspace" ((n sizet))
  "Structure that holds the additional working space required for the
  intermediate steps of the mixed radix real fft algoritms"
  :documentation
  "This function allocates a workspace for a real transform of length n.")

(defmobject fft-real-workspace-float
    "gsl_fft_real_workspace_float" ((n sizet))
  "Structure that holds the additional working space required for the
  intermediate steps of the mixed radix real float fft algoritms"
  :documentation
  "This function allocates a workspace for a real float transform of length
  n.")

(defmfun fft-real-transform
    ((vector vector) &key (stride 1) (n (size vector))
                     (wavetable (eltcase single-float (make-fft-real-wavetable-float (size vector))
                                         double-float (make-fft-real-wavetable (size vector))))
                     (workspace (eltcase single-float (make-fft-real-workspace-float (size vector))
                                         double-float (make-fft-real-workspace (size vector)))))
  ("gsl_fft_real" :type "_transform")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Forward FFT for a real vector")

(defmfun fft-real-unpack
         ((vector vector)
          &key (stride 1) (n (size vector))
          (complex-output (eltcase single-float (make-marray '(complex single-float) :dimensions n)
                                        t (make-marray '(complex double-float) :dimensions n))))
  ("gsl_fft_real" :type "_unpack")
  (((c-pointer vector) :pointer) ((c-pointer complex-output) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :float
  :inputs (vector complex-output)
  :outputs (complex-output)
  :return (complex-output)
  :documentation
  "This function converts a single real array into an equivalent complex
  array (with imaginary part set to zero), suitable for fft-complex
  routines.")

;; Functions for fast fourier transforms on real data.
;; Sumant Oemrawsingh, Sun Oct 25 2009 - 16:35
;; Time-stamp: <2009-11-03 23:02:17EST fft-real.lisp>

;; /usr/include/gsl/gsl_fft_real.h
;; /usr/include/gsl/gsl_fft_real_float.h

(in-package :gsl)

;; Mixed Radix general-N functions

(defmobject fft-real-wavetable-double-float
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

(defmobject fft-real-wavetable-single-float
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

(defmobject fft-real-workspace-double-float
    "gsl_fft_real_workspace" ((n sizet))
  "Structure that holds the additional working space required for the
  intermediate steps of the mixed radix real fft algoritms"
  :documentation
  "This function allocates a workspace for a real transform of length n.")

(defmobject fft-real-workspace-single-float
    "gsl_fft_real_workspace_float" ((n sizet))
  "Structure that holds the additional working space required for the
  intermediate steps of the mixed radix real float fft algoritms"
  :documentation
  "This function allocates a workspace for a real float transform of length
  n.")

;; Functions for fast fourier transforms on complex data.
;; Sumant Oemrawsingh, Sat Oct 24 2009 - 12:55

;; /usr/include/gsl/gsl_fft_complex.h
;; /usr/include/gsl/gsl_fft_complex_float.h

(in-package :gsl)

;; Power of 2 functions

(defmfun fft-complex-radix2-forward
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  ("gsl_fft" :type "_radix2_forward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Forward FFT for a complex radix-2 vector")

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
;; TODO: Get it to work with complex_float. GSL sucks, because the complex
;; wavetables are named gsl_fft_complex_wavetable and
;; gsl_fft_complex_wavetable_float. This is not the same as the naming
;; convention used elsewhere, where gsl_fft_complex_float_wavetable would be
;; expected.
;; I could define a new object with superclass fft-wavetable, but I have to
;; figure out if this is the cleanest way to do it in gsll. Probably an
;; approach such as with make-marray element-type ... is nicest.

(defclass fft-wavetable (mobject)
  ()
  (:documentation
   "Factorization and trigonometric lookup tables for mixed radix fft algorithms"))

(defmobject complex-wavetable
    "gsl_fft_complex_wavetable"
  ((n sizet))
  "structure that holds the factorization and trigonometric lookup tables for
  the mixed radix complex fft algorithm"
  :superclasses (fft-wavetable)
  :allocator "gsl_fft_complex_wavetable_alloc"
  :allocate-inputs ((n sizet))
  :freer "gsl_fft_complex_wavetable_free"
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


(defclass fft-workspace (mobject)
  ()
  (:documentation
   "Additional working space for the mixed radix fft algorithms to hold the
   intermediate steps of the transform."))

(defmobject complex-workspace
    "gsl_fft_complex_workspace"
  ((n sizet))
  "Structure that holds the additional working space required for the
  intermediate steps of the mixed radix fft algoritms"
  :superclasses (fft-workspace)
  :allocator "gsl_fft_complex_workspace_alloc"
  :allocate-inputs ((n sizet))
  :freer "gsl_fft_complex_workspace_free"
  :documentation
  "This function allocates a workspace for a complex transform of length n.")


(defmfun fft-complex-forward
    ((vector vector) &key (stride 1) (n (size vector))
                     (wavetable (make-complex-wavetable (size vector)))
                     (workspace (make-complex-workspace (size vector))))
  ("gsl_fft" :type "_forward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Forward FFT for a complex vector")

(defmfun fft-complex-backward
    ((vector vector) &key (stride 1) (n (size vector))
                     (wavetable (make-complex-wavetable (size vector)))
                     (workspace (make-complex-workspace (size vector))))
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
                     (wavetable (make-complex-wavetable (size vector)))
                     (workspace (make-complex-workspace (size vector))))
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

(defmfun fft-complex-transform
    ((vector vector) direction &key (stride 1) (n (size vector))
                     (wavetable (make-complex-wavetable (size vector)))
                     (workspace (make-complex-workspace (size vector))))
  ("gsl_fft" :type "_transform")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer)
   (direction fft-direction))
  :definition :generic
  :element-types :complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "FFT in the given direction for a complex vector")

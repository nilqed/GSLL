;; Generators of random numbers.
;; Liam Healy, Sat Jul 15 2006 - 14:43
;; Time-stamp: <2008-02-17 12:15:46EST generators.lisp>
;; $Id: $

(in-package :gsl)

;;; Would it be useful to have a make-data like macro?

(export '(make-random-number-generator))

(defgeneric generator (object)
  (:method ((object t))
    (if (cffi:pointerp object)
	object
	(call-next-method)))
  (:documentation "The foreign pointer to the GSL generator function."))

(defclass gsl-random ()
  ((type :initarg :type :reader rng-type)
   (generator :initarg :generator :accessor generator)))

(defclass random-number-generator (gsl-random)
  ()
  (:documentation "A generator of random numbers."))

(defmethod print-object ((object gsl-random) stream)
  (print-unreadable-object (object stream :type t :identity t) 
    (format stream "~a" (when (generator object) (rng-name object)))))

(defun make-random-number-generator
    (&optional (type *default-type*) (generator t))
  "Make a random number generator; by default it is allocated on creation."
  (let ((instance
	 (make-instance
	  'random-number-generator
	  :type type
	  :generator generator)))
    (if (eq generator t) (alloc instance) instance)))

(defgo-s (random-number-generator type value)
	 make-random-number-generator free rng-set 1)

;;;;****************************************************************************
;;;; Initialization
;;;;****************************************************************************

(defmfun alloc ((generator random-number-generator))
  "gsl_rng_alloc" (((rng-type generator) :pointer))
  :type :method
  :c-return (ptr :pointer)
  :return ((progn (setf (generator generator) ptr) generator))
  :documentation			; FDL
  "Instatiate a random number generator of specified type.
   For example, create an instance of the Tausworthe
   generator: (rng-alloc *taus*).
   The generator is automatically initialized with the default seed,
   *default-seed*.  This is zero by default but can be changed
   either directly or by using the environment variable GSL_RNG_SEED.")

(defmfun free ((generator random-number-generator))
  "gsl_rng_free" (((generator generator) :pointer))
  :type :method
  :c-return :void
  :after ((setf (generator generator) nil))
  :documentation			; FDL
  "Free all the memory associated with the generator.")

;;;;****************************************************************************
;;;; Seed
;;;;****************************************************************************

(cffi:defcvar ("gsl_rng_default_seed" *default-seed*) :ulong)
(setf (documentation '*default-seed* 'variable)
      "The default seed for random number generators.")
(map-name '*default-seed* "gsl_rng_cmrg")
(export '*default-seed*)

(defmfun rng-set (rng-instance value)
  "gsl_rng_set" (((generator rng-instance) :pointer) (value :ulong))
  :c-return :void
  :documentation			; FDL
  "Initialize (or `seeds') the random number generator.  If
   the generator is seeded with the same value of s on two different
   runs, the same stream of random numbers will be generated by successive
   calls to the routines below.  If different values of s are
   supplied, then the generated streams of random numbers should be
   completely different.  If the seed s is zero then the standard seed
   from the original implementation is used instead.  For example, the
   original Fortran source code for the *ranlux* generator used a seed
   of 314159265, and so choosing s equal to zero reproduces this when
   using *ranlux*.")

;;;;****************************************************************************
;;;; Sampling
;;;;****************************************************************************

(defmfun get-random-number (generator)
  "gsl_rng_get" (((generator generator) :pointer))
  :c-return :ulong
  :documentation			; FDL
  "Generate a random integer.  The
   minimum and maximum values depend on the algorithm used, but all
   integers in the range [min, max] are equally likely.  The
   values of min and max can determined using the auxiliary
   functions #'rng-max and #'rng-min.")

(defmfun uniform (generator)
  "gsl_rng_uniform" (((generator generator) :pointer))
  :c-return :double
  :documentation			; FDL
  "A double precision floating point number uniformly
   distributed in the range [0,1).  The range includes 0.0 but excludes 1.0.
   The value is typically obtained by dividing the result of
   #'rng-get by (+ (rng-max generator) 1.0) in double
   precision.  Some generators compute this ratio internally so that they
   can provide floating point numbers with more than 32 bits of randomness
   (the maximum number of bits that can be portably represented in a single
   :ulong.")

(defmfun uniform>0 (generator)
  "gsl_rng_uniform_pos" (((generator generator) :pointer))
  :c-return :double
  :documentation			; FDL
  "Return a positive double precision floating point number
   uniformly distributed in the range (0,1), excluding both 0.0 and 1.0.
   The number is obtained by sampling the generator with the algorithm of
   #'uniform until a non-zero value is obtained.  You can use
   this function if you need to avoid a singularity at 0.0.")

(defmfun uniform-fixnum (generator upperbound)
  "gsl_rng_uniform_int" (((generator generator) :pointer) (upperbound :ulong))
  :c-return :ulong
  :documentation			; FDL
  "Generate a random integer from 0 to upperbound-1 inclusive.
   All integers in the range are equally likely, regardless
   of the generator used.  An offset correction is applied so that zero is
   always returned with the correct probability, for any minimum value of
   the underlying generator.  If upperbound is larger than the range
   of the generator then the function signals an error.")

;;;;****************************************************************************
;;;; Information functions about instances
;;;;****************************************************************************

(export 'rng-name)
(defgeneric rng-name (rng-instance)
  (:documentation			; FDL
   "The name of the random number generator."))

(defmfun rng-name ((rng-instance random-number-generator))
  "gsl_rng_name" (((generator rng-instance) :pointer))
  :type :method
  :c-return :string)

(defmfun rng-max (rng-instance)
  "gsl_rng_max" (((generator rng-instance) :pointer))
  :c-return :unsigned-long
  :documentation "The largest value that #'get-random-number
   can return.")

(defmfun rng-min (rng-instance)
  "gsl_rng_min" (((generator rng-instance) :pointer))
  :c-return :unsigned-long
  :documentation			; FDL
  "The smallest value that #'get-random-number
   can return.  Usually this value is zero.  There are some generators with
   algorithms that cannot return zero, and for these generators the minimum
   value is 1.")

(export 'rng-state)
(defgeneric rng-state (rng-instance)
  (:documentation			; FDL
   "A pointer to the state of generator."))

(defmfun rng-state ((rng-instance random-number-generator))
  "gsl_rng_state" (((generator rng-instance) :pointer))
  :c-return :pointer
  :type :method
  :index gsl-random-state)

(export 'rng-size)
(defgeneric rng-size (rng-instance)
  (:documentation			; FDL
   "The size of the generator."))

(defmfun rng-size ((rng-instance random-number-generator))
  "gsl_rng_size" (((generator rng-instance) :pointer))
  :c-return size
  :type :method
  :index gsl-random-state)

(export 'gsl-random-state)
(defun gsl-random-state (rng-instance)
  "The complete state of a given random number generator, specified
   as a vector of bytes."
  (let* ((gen rng-instance)
	 (ans
	  (make-array (rng-size gen)
		      :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length ans)
       do
       (setf (aref ans i)
	     (mem-aref (rng-state gen) :uint8 i)))
    ans))

;;;;****************************************************************************
;;;; Copying state
;;;;****************************************************************************

(defmfun copy
    ((destination random-number-generator) (source random-number-generator))
  "gsl_rng_memcpy"
  (((generator destination) :pointer) ((generator source) :pointer))
  :type :method
  :documentation			; FDL
  "Copy the random number generator source into the
   pre-existing generator destination,
   making destination into an exact copy
   of source.  The two generators must be of the same type.")

(export 'clone-generator)
(defgeneric clone-generator (instance)
  (:documentation			; FDL
   "Create a new generator which is an exact copy of the original.
   Don't use; use #'make-random-number-generator, #'copy instead."))

(defmfun clone-generator ((instance random-number-generator))
  "gsl_rng_clone" (((generator instance) :pointer))
  :c-return :pointer
  :type :method)

(defmfun write-binary
    ((object random-number-generator) stream)
  "gsl_rng_fwrite"
  ((stream :pointer) ((generator object) :pointer))
  :type :method)

(defmfun read-binary
    ((object random-number-generator) stream)
  "gsl_block_fread"
  ((stream :pointer) ((pointer object) :pointer))
  :type :method)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests random-number-generators
 (letm ((rng (random-number-generator *mt19937* 0)))
   (loop for i from 0 to 10
	 collect
	 (uniform-fixnum rng 1000)))
 (letm ((rng (random-number-generator *cmrg* 0)))
   (loop for i from 0 to 10 collect (uniform rng))))
|#

(LISP-UNIT:DEFINE-TEST RANDOM-NUMBER-GENERATORS
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 999 162 282 947 231 484 957 744 540 739 759))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (UNIFORM-FIXNUM RNG 1000)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST 0.11177622997750353d0 0.9591667949963206d0
	  0.8415268011584537d0 0.9254037136795947d0
	  0.27540698474059205d0 0.7093040573919677d0
	  0.5541333041871588d0 0.8806957769583426d0
	  0.597139396982798d0 0.7518741133398722d0
	  0.9311084621265104d0))
   (MULTIPLE-VALUE-LIST
    (LETM ((RNG (RANDOM-NUMBER-GENERATOR *CMRG* 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	    (UNIFORM RNG))))))

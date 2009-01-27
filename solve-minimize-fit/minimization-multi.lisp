;; Multivariate minimization.
;; Liam Healy  <Tue Jan  8 2008 - 21:28>
;; Time-stamp: <2009-01-26 21:55:26EST minimization-multi.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_multimin.h

;;; In the parabaloid example, I notice that the consruct 
;;; (min-test-gradient (mfdfminimizer-gradient minimizer) 1.0d-3)
;;; is constructing a CL vector-double-float (in mfdfminimizer-gradient) and
;;; then immediately pulling out the pointer (in min-test-gradient).  It
;;; is easy enough to eliminate this, but then mfdfminimizer-gradient
;;; would not be useful to a CL user.

;;;;****************************************************************************
;;;; Function definition
;;;;****************************************************************************

;;; The structures gsl-mfunction and gsl-mfunction-fdf are, from the
;;; CFFI point of view, equally valid for gsl_multimin_function and
;;; gsl_multimin_function_fdf defined in
;;; /usr/include/gsl/gsl_multimin.h as they are for
;;; gsl_multiroot_function and gsl_multiroot_function_fdf defined in
;;; /usr/include/gsl/gsl_multiroots.h.  As far as CFFI is concerned, a
;;; pointer is a pointer, even though the C definition the functions
;;; they point to have different signatures.

(export 'make-minimization-functions)
(defmacro make-minimization-functions (function dimensions &optional df fdf array)
  "Setup functions for multivariate minimization.
   The CL functions name and derivative should be defined previously
   with defuns."
  (let ((vdec (if array :pointer `((:double ,dimensions))))
	(svdec (if array :pointer `(:set :double ,dimensions))))
    (with-unique-names (solverfn solverdf solverfdf)
      `(progn
	 (defmcallback ,solverfn :double ,vdec nil t ,function)
	 ,@(when
	    df
	    `((defmcallback ,solverdf :void ,vdec (,svdec) t ,df)
	      (defmcallback
		  ,solverfdf :void ,vdec ((:set :double -1) ,svdec) t ,fdf)))
	 ,(if df
	      `(defcbstruct (,solverfn function ,solverdf df ,solverfdf fdf)
		   gsl-mfunction-fdf
		 ((dimensions ,dimensions)))
	      `(defcbstruct (,solverfn function)
		   gsl-mfunction
		 ((dimensions ,dimensions))))))))

;;;;****************************************************************************
;;;; Initialization
;;;;****************************************************************************

(defmobject multi-dimensional-minimizer-f
    "gsl_multimin_fminimizer"
  ((type :pointer) (dimension sizet))
  "multi-dimensional minimizer with function only"
  :documentation			; FDL
  "Make an instance of a minimizer of the given for an function of the
   given dimensions.  Optionally initialize the minimizer to minimize
   the function starting from the initial point.  The size of the
   initial trial steps is given in vector step-size. The precise
   meaning of this parameter depends on the method used."
  :initialize-suffix "set"
  :initialize-args ;; Could have one fewer argument: dimension=(dim0 initial)
  ((function :pointer) ((mpointer initial) :pointer)
   ((mpointer step-size) :pointer)))

(defmobject multi-dimensional-minimizer-fdf
    "gsl_multimin_fdfminimizer"
  ((type :pointer) (dimension sizet))
  "multi-dimensional minimizer with function and derivative"
  :documentation			; FDL
  "Make an instance of a derivative-based minimizer of the given for
   an function of the given dimensions.  Optionally initialize the
   minimizer to minimize the function starting from the initial point.
   The size of the first trial step is given by step-size.  The
   accuracy of the line minimization is specified by tolernace.  The
   precise meaning of this parameter depends on the method used.
   Typically the line minimization is considered successful if the
   gradient of the function g is orthogonal to the current search
   direction p to a relative accuracy of tolerance, where dot(p,g) <
   tol |p| |g|."
  :initialize-suffix "set"
  :initialize-args
  ;; Could have one fewer argument: dimension=(dim0 initial)
  ((function-derivative :pointer) ((mpointer initial) :pointer)
   (step-size :double) (tolerance :double)))

(defmfun name ((minimizer multi-dimensional-minimizer-f))
  "gsl_multimin_fminimizer_name"
  (((mpointer minimizer) :pointer))
  :definition :method
  :c-return :string
  :documentation			; FDL
  "The name of the minimizer.")

(defmfun name ((minimizer multi-dimensional-minimizer-fdf))
  "gsl_multimin_fdfminimizer_name"
  (((mpointer minimizer) :pointer))
  :definition :method
  :c-return :string
  :documentation			; FDL
  "The name of the minimizer.")

;;;;****************************************************************************
;;;; Iteration
;;;;****************************************************************************

(defmfun iterate ((minimizer multi-dimensional-minimizer-f))
  "gsl_multimin_fminimizer_iterate"
  (((mpointer minimizer) :pointer))
  :definition :method
  :documentation			; FDL
  "Perform a single iteration of the minimizer.  If the iteration
   encounters an unexpected problem then an error code will be
   returned.")

(defmfun iterate ((minimizer multi-dimensional-minimizer-fdf))
  "gsl_multimin_fdfminimizer_iterate"
  (((mpointer minimizer) :pointer))
  :definition :method
  :documentation			; FDL
  "Perform a single iteration of the minimizer.  If the iteration
   encounters an unexpected problem then an error code will be
   returned.")

(defmfun solution ((minimizer multi-dimensional-minimizer-f))
  "gsl_multimin_fminimizer_x"
  (((mpointer minimizer) :pointer))
  :definition :method
  :c-return (crtn :pointer)
  :return ((make-marray 'double-float :from-pointer crtn))
  :documentation			; FDL
  "The current best estimate of the location of the minimum.")

(defmfun solution ((minimizer multi-dimensional-minimizer-fdf))
  "gsl_multimin_fdfminimizer_x"
  (((mpointer minimizer) :pointer))
  :definition :method
  :c-return (crtn :pointer)
  :return ((make-marray 'double-float :from-pointer crtn))
  :documentation			; FDL
  "The current best estimate of the location of the minimum.")

(defmfun function-value ((minimizer multi-dimensional-minimizer-f))
  "gsl_multimin_fminimizer_minimum"
  (((mpointer minimizer) :pointer))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "The current best estimate of the value of the minimum.")

(defmfun function-value ((minimizer multi-dimensional-minimizer-fdf))
  "gsl_multimin_fdfminimizer_minimum"
  (((mpointer minimizer) :pointer))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "The current best estimate of the value of the minimum.")

(defmfun mfminimizer-size (minimizer)
  "gsl_multimin_fminimizer_size"
  (((mpointer minimizer) :pointer))
  :c-return :double
  :documentation			; FDL
  "A minimizer-specific characteristic size for the minimizer.")

(defmfun mfdfminimizer-gradient (minimizer)
  "gsl_multimin_fdfminimizer_gradient"
  (((mpointer minimizer) :pointer))
  :c-return (crtn :pointer)
  :return ((make-marray 'double-float :from-pointer crtn))
  :documentation			; FDL
  "The current best estimate of the gradient for the minimizer.")

(defmfun mfdfminimizer-restart (minimizer)
  "gsl_multimin_fdfminimizer_restart"
  (((mpointer minimizer) :pointer))
  :documentation			; FDL
  "Reset the minimizer to use the current point as a
   new starting point.")

;;;;****************************************************************************
;;;; Stopping criteria
;;;;****************************************************************************

(defmfun min-test-gradient (gradient absolute-error)
  "gsl_multimin_test_gradient"
  (((mpointer gradient) :pointer) (absolute-error :double))
  :inputs (gradient)
  :c-return :success-continue
  :documentation			; FDL
  "Test the norm of the gradient against the
   absolute tolerance absolute-error.  The gradient of a multidimensional
   function goes to zero at a minimum. The test returns T
   if |g| < epsabs is achieved, and NIL otherwise.  A suitable choice of
   absolute-error can be made from the desired accuracy in the function for
   small variations in x.  The relationship between these quantities
   \delta f = g \delta x.")

(defmfun min-test-size (size absolute-error)
  "gsl_multimin_test_size"
  ((size :double) (absolute-error :double))
  :c-return :success-continue
  :documentation			; FDL
  "Test the minimizer specific characteristic size (if applicable to
   the used minimizer) against absolute tolerance absolute-error.  The test
   returns T if the size is smaller than tolerance, and NIL otherwise.")

;;;;****************************************************************************
;;;; Algorithms
;;;;****************************************************************************

(defmpar *conjugate-fletcher-reeves*
    "gsl_multimin_fdfminimizer_conjugate_fr"
  ;; FDL
  "The Fletcher-Reeves conjugate gradient algorithm. The conjugate
   gradient algorithm proceeds as a succession of line minimizations. The
   sequence of search directions is used to build up an approximation to the
   curvature of the function in the neighborhood of the minimum.  

   An initial search direction p is chosen using the gradient, and
   line minimization is carried out in that direction.  The accuracy of
   the line minimization is specified by the parameter tol.  The minimum
   along this line occurs when the function gradient g and the search
   direction p are orthogonal.  The line minimization terminates when
   dot(p,g) < tol |p| |g|.  The search direction is updated using the
   Fletcher-Reeves formula p' = g' - \beta g where \beta=-|g'|^2/|g|^2,
   and the line minimization is then repeated for the new search
   direction.")

(defmpar *conjugate-polak-ribiere*
    "gsl_multimin_fdfminimizer_conjugate_pr"
  ;; FDL
  "The Polak-Ribiere conjugate gradient algorithm.  It is similar to
   the Fletcher-Reeves method, differing only in the choice of the
   coefficient \beta. Both methods work well when the evaluation point is
   close enough to the minimum of the objective function that it is well
   approximated by a quadratic hypersurface.")

(defmpar *vector-bfgs*
    "gsl_multimin_fdfminimizer_vector_bfgs"
  ;; FDL
  "The vector Broyden-Fletcher-Goldfarb-Shanno (BFGS) conjugate
   gradient algorithm.  It is a quasi-Newton method which builds up an
   approximation to the second derivatives of the function using the
   difference between successive gradient vectors.  By combining the
   first and second derivatives the algorithm is able to take Newton-type
   steps towards the function minimum, assuming quadratic behavior in
   that region.")

(defmpar *simplex-nelder-mead*
    "gsl_multimin_fminimizer_nmsimplex"
  ;; FDL
  "The Simplex algorithm of Nelder and Mead. It constructs 
   n vectors p_i from the
   starting vector initial and the vector step-size as follows:
   p_0 = (x_0, x_1, ... , x_n) 
   p_1 = (x_0 + step_size_0, x_1, ... , x_n) 
   p_2 = (x_0, x_1 + step_size_1, ... , x_n) 
   ... = ...
   p_n = (x_0, x_1, ... , x_n+step_size_n)
   These vectors form the n+1 vertices of a simplex in n
   dimensions.  On each iteration the algorithm tries to improve
   the parameter vector p_i corresponding to the highest
   function value by simple geometrical transformations.  These
   are reflection, reflection followed by expansion, contraction and multiple
   contraction. Using these transformations the simplex moves through 
   the parameter space towards the minimum, where it contracts itself.  

   After each iteration, the best vertex is returned.  Note, that due to
   the nature of the algorithm not every step improves the current
   best parameter vector.  Usually several iterations are required.

   The routine calculates the minimizer specific characteristic size as the
   average distance from the geometrical center of the simplex to all its
   vertices.  This size can be used as a stopping criteria, as the simplex
   contracts itself near the minimum. The size is returned by the function
   #'mfminimizer-size.")

;;;;****************************************************************************
;;;; Examples
;;;;****************************************************************************

;;; Examples from Sec. 35.8.

;;; Example using derivatives, taking a vector argument.

(defparameter *parabaloid-center* #(1.0d0 2.0d0))

(defun parabaloid-vector (gsl-vector)
  "A parabaloid function of two arguments, given in GSL manual Sec. 35.4.
   This version takes a vector-double-float argument."
  (let ((x (maref gsl-vector 0))
	(y (maref gsl-vector 1))
	(dp0 (aref *parabaloid-center* 0))
	(dp1 (aref *parabaloid-center* 1)))
    (+ (* 10 (expt (- x dp0) 2))
       (* 20 (expt (- y dp1) 2))
       30)))

(defun parabaloid-derivative
    (arguments-gv-pointer derivative-gv-pointer)
  (let ((x (maref arguments-gv-pointer 0))
	(y (maref arguments-gv-pointer 1))
	(dp0 (aref *parabaloid-center* 0))
	(dp1 (aref *parabaloid-center* 1)))
    (setf (maref derivative-gv-pointer 0)
	  (* 20 (- x dp0))
	  (maref derivative-gv-pointer 1)
	  (* 40 (- y dp1)))))

(defun parabaloid-and-derivative (arguments-gv-pointer derivative-gv-pointer)
  (prog1
      (parabaloid-vector arguments-gv-pointer)
    (parabaloid-derivative
     arguments-gv-pointer derivative-gv-pointer)))

#+callback-toplevel-only
(defparameter *minmultidf-cb*
  (make-minimization-functions
   parabaloid-vector 2
   parabaloid-derivative parabaloid-and-derivative t))

(defun multimin-example-derivative
    (&optional (method *conjugate-fletcher-reeves*) (print-steps t))
  (let* ((initial #m(5.0d0 7.0d0))
	 (minimizer
	  (make-multi-dimensional-minimizer-fdf
	   method 2
	   #-callback-toplevel-only
	   (make-minimization-functions
	    parabaloid-vector 2
	    parabaloid-derivative parabaloid-and-derivative t)
	   #+callback-toplevel-only *minmultidf-cb*
	   initial 0.01d0 1.0d-4)))
    (loop with status = T
       for iter from 0 below 100
       while status
       do
       (iterate minimizer)
       (setf status
	     (not (min-test-gradient
		   (mfdfminimizer-gradient minimizer)
		   1.0d-3)))
       (when print-steps
	 (let ((x (solution minimizer)))
	   (format t "~d~6t~10,6f~18t~10,6f~28t~12,9f~&"
		   iter (maref x 0) (maref x 1)
		   (function-value minimizer))))
       finally
       (return
	 (let ((x (solution minimizer)))
	   (values (maref x 0) (maref x 1) (function-value minimizer)))))))

;;; Example without derivatives, same function but now defined using
;;; scalars.

(defun parabaloid-scalar (x y)
  "A parabaloid function of two arguments, given in GSL manual Sec. 35.4.
   This version takes scalar arguments."
  (let ((dp0 (aref *parabaloid-center* 0))
	(dp1 (aref *parabaloid-center* 1)))
    (+ (* 10 (expt (- x dp0) 2))
       (* 20 (expt (- y dp1) 2))
       30)))

#+callback-toplevel-only
(defparameter *minmulti-cb*
  (make-minimization-functions parabaloid-scalar 2))

(defun multimin-example-no-derivative
    (&optional (method *simplex-nelder-mead*) (print-steps t))
  (let ((step-size (make-marray 'double-float :dimensions 2)))
    (set-all step-size 1.0d0)
    (let ((minimizer
	   (make-multi-dimensional-minimizer-f
	    method 2
	    #-callback-toplevel-only
	    (make-minimization-functions parabaloid-scalar 2)
	    #+callback-toplevel-only *minmulti-cb*
	    #m(5.0d0 7.0d0) step-size)))
      (loop with status = T and size
	 for iter from 0 below 100
	 while status
	 do (iterate minimizer)
	 (setf size
	       (mfminimizer-size minimizer)
	       status
	       (not (min-test-size size 1.0d-2)))
	 (when print-steps
	   (let ((x (solution minimizer)))
	     (format t "~d~6t~10,6f~18t~10,6f~28t~12,9f~40t~8,3f~&"
		     iter (maref x 0) (maref x 1)
		     (function-value minimizer)
		     size)))
	 finally
	 (return
	   (let ((x (solution minimizer)))
	     (values (maref x 0) (maref x 1) (function-value minimizer))))))))

(save-test minimization-multi
 (multimin-example-no-derivative *simplex-nelder-mead* nil)
 (multimin-example-derivative *conjugate-fletcher-reeves* nil)
 (multimin-example-derivative *conjugate-polak-ribiere* nil)
 (multimin-example-derivative *vector-bfgs* nil))

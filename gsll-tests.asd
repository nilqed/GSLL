;; Definition of GSLL system 
;; Liam Healy
;; Time-stamp: <2009-09-26 12:46:47EDT gsll-tests.asd>

(asdf:defsystem "gsll-tests"
  :name "gsll-tests"
  :description "Regression (unit) tests for GNU Scientific Library for Lisp."
  :version "0"
  :author "Liam M. Healy"
  :licence "LLGPL v3, FDL"
  :depends-on (gsll lisp-unit)
  :components
  ((:module test-unit
	    :components
	    ((:file "augment")))
   (:module tests
	    :components
	    ((:file "absolute-deviation")
	     (:file "absolute-sum")
	     (:file "airy")
	     (:file "autocorrelation")
	     (:file "axpy")
	     (:file "basis-spline")
	     (:file "bernoulli")
	     (:file "bessel")
	     (:file "beta")
	     (:file "binomial")
	     (:file "blas-copy")
	     (:file "blas-swap")
	     (:file "cauchy")
	     (:file "cdot")
	     (:file "chebyshev")
	     (:file "cholesky")
	     (:file "clausen")
	     (:file "column")
	     (:file "combination")
	     (:file "coulomb")
	     (:file "coupling")
	     (:file "correlation")
	     (:file "covariance")
	     (:file "dawson")
	     (:file "debye")
	     (:file "dilogarithm")
	     (:file "dirichlet")
	     (:file "discrete")
	     (:file "dot")
	     (:file "eigensystems")
	     (:file "elementary")
	     (:file "elliptic-functions")
	     (:file "elliptic-integrals")
	     (:file "error-functions")
	     (:file "euclidean-norm")
	     (:file "exponential-functions")
	     (:file "exponential-integrals")
	     (:file "exponential")
	     (:file "exponential-power")
	     (:file "fdist")
	     (:file "fermi-dirac")
	     (:file "flat")
	     (:file "gamma")
	     (:file "gamma-randist")
	     (:file "gaussian-bivariate")
	     (:file "gaussian")
	     (:file "gaussian-tail")
	     (:file "gegenbauer")
	     (:file "geometric")
	     (:file "givens")
	     (:file "gumbel1")
	     (:file "gumbel2")
	     (:file "hankel")
	     (:file "higher-moments")
	     (:file "histogram")
	     (:file "householder")
	     (:file "hypergeometric")
	     (:file "hypergeometric-randist")
	     (:file "index-max")
	     (:file "interpolation")
	     (:file "inverse-matrix-product")
	     (:file "laguerre")
	     (:file "lambert")
	     (:file "landau")
	     (:file "laplace")
	     (:file "legendre")
	     (:file "levy")
	     (:file "linear-least-squares")
	     (:file "logarithmic")
	     (:file "logarithm")
	     (:file "logistic")
	     (:file "lognormal")
	     (:file "lu")
	     (:file "mathematical")
	     (:file "mathieu")
	     (:file "matrix-copy")
	     (:file "matrix-copy-to-cl-and-back")
	     (:file "matrix-div")
	     (:file "matrix-max-index")
	     (:file "matrix-max")
	     (:file "matrix-mean")
	     (:file "matrix-min")
	     (:file "matrix-min-index")
	     (:file "matrix-minmax-index")
	     (:file "matrix-minmax")
	     (:file "matrix-sub")
	     (:file "matrix-add")
	     (:file "matrix-mult")
	     #+fsbv (:file "matrix-product-hermitian")
	     (:file "matrix-product")
	     (:file "matrix-product-nonsquare")
	     (:file "matrix-product-symmetric")
	     (:file "matrix-product-triangular")
	     (:file "matrix-set-all")
	     (:file "matrix-set-zero")
	     (:file "matrix-standard-deviation")
	     (:file "matrix-standard-deviation-with-fixed-mean")
	     (:file "matrix-standard-deviation-with-mean")
	     (:file "matrix-swap")
	     (:file "matrix-transpose-copy")
	     (:file "matrix-transpose")
	     (:file "matrix-variance")
	     (:file "matrix-variance-with-fixed-mean")
	     (:file "matrix-variance-with-mean")
	     (:file "median-percentile")
	     (:file "minimization-one")
	     (:file "minimization-multi")
	     (:file "monte-carlo")
	     (:file "multinomial")
	     (:file "negative-binomial")
	     (:file "nonlinear-least-squares")
	     (:file "ntuple")
	     (:file "numerical-differentiation")
	     (:file "numerical-integration")
	     (:file "ode")
	     (:file "pareto")
	     (:file "permutation")
	     (:file "poisson")
	     (:file "polynomial")
	     (:file "power")
	     (:file "psi")
	     (:file "qr")
	     (:file "quasi-random-number-generators")
	     (:file "random-number-generators")
	     (:file "rayleigh")
	     (:file "rayleigh-tail")
	     (:file "roots-multi")
	     (:file "roots-one")
	     (:file "row")
	     (:file "scale")
	     (:file "series-acceleration")
	     (:file "set-basis")
	     (:file "setf-column")
	     (:file "setf-row")
	     (:file "set-identity")
	     (:file "shuffling-sampling")
	     (:file "sort-matrix-largest")
	     (:file "sort-matrix")
	     (:file "sort-matrix-smallest")
	     (:file "sort-vector-index")
	     (:file "sort-vector-largest-index")
	     (:file "sort-vector-largest")
	     (:file "sort-vector")
	     (:file "sort-vector-smallest-index")
	     (:file "sort-vector-smallest")
	     (:file "spherical-vector")
	     (:file "svd")
	     (:file "swap-columns")
	     (:file "swap-elements")
	     (:file "swap-row-column")
	     (:file "swap-rows")
	     (:file "synchrotron")
	     (:file "tdist")
	     (:file "transport")
	     (:file "trigonometry")
	     (:file "vector-copy")
	     (:file "vector-copy-to-cl-and-back")
	     (:file "vector-div")
	     (:file "vector-max-index")
	     (:file "vector-max")
	     (:file "vector-mean")
	     (:file "vector-min")
	     (:file "vector-min-index")
	     (:file "vector-minmax-index")
	     (:file "vector-minmax")
	     (:file "vector-sub")
	     (:file "vector-add")
	     (:file "vector-mult")
	     (:file "vector-reverse")
	     (:file "vector-set-all")
	     (:file "vector-set-zero")
	     (:file "vector-standard-deviation")
	     (:file "vector-standard-deviation-with-fixed-mean")
	     (:file "vector-standard-deviation-with-mean")
	     (:file "vector-swap")
	     (:file "vector-variance")
	     (:file "vector-variance-with-fixed-mean")
	     (:file "vector-variance-with-mean")
	     (:file "weibull")
	     (:file "zeta")))))

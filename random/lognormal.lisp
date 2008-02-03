;; Lognormal distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2008-02-03 11:08:34EST lognormal.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl lognormal (generator zeta sigma)
  "gsl_ran_lognormal"
  (((generator generator) :pointer) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the lognormal distribution.
   The distribution function is
   p(x) dx = {1 \over x \sqrt{2 \pi \sigma^2}} \exp(-(\ln(x) - \zeta)^2/2 \sigma^2) dx
   for x > 0.")

(defun-gsl lognormal-pdf (x zeta sigma)
  "gsl_ran_lognormal_pdf" ((x :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at X
   for a lognormal distribution with parameters zeta and sigma,
   using the formula given in #'lognormal.")

(defun-gsl lognormal-P (x zeta sigma)
  "gsl_cdf_lognormal_P" ((x :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the lognormal distribution with parameters zeta and sigma.")

(defun-gsl lognormal-Q (x zeta sigma)
  "gsl_cdf_lognormal_Q" ((x :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the lognormal distribution with parameters
  zeta and sigma.")

(defun-gsl lognormal-Pinv (P zeta sigma)
  "gsl_cdf_lognormal_Pinv" ((P :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the lognormal distribution with parameters
  zeta and sigma.")

(defun-gsl lognormal-Qinv (Q zeta sigma)
  "gsl_cdf_lognormal_Qinv" ((Q :double) (zeta :double) (sigma :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the lognormal distribution with parameters
   zeta and sigma.")

;;; Examples and unit test
(lisp-unit:define-test lognormal
  (lisp-unit:assert-equal
   '("0.238644706813d+01" "0.116876021674d+00" "0.475337457880d+01"
     "0.300933937758d+02" "0.811958437576d+00" "0.316342105516d+01"
     "0.914620656772d+00" "0.727307901066d+00" "0.218018485218d+01"
     "0.389088566169d+01" "0.182184697889d+03")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (loop for i from 0 to 10
	    collect
	    (lognormal rng 1.0d0 2.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.152898339657d+00"
   (lognormal-pdf 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.341328827235d+00"
   (lognormal-P 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.658671172765d+00"
   (lognormal-Q 1.2d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+01"
   (lognormal-Pinv 0.3413288272347352d0 1.0d0 2.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.120000000000d+01"
   (lognormal-Qinv 0.6586711727652649d0 1.0d0 2.0d0)))

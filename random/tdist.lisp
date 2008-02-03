;; Tdist distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2008-02-03 13:27:31EST tdist.lisp>
;; $Id: $

(in-package :gsl)

(defun-gsl tdist (generator nu)
  "gsl_ran_tdist"
  (((generator generator) :pointer) (nu :double))
  :c-return :double
  :documentation			; FDL
  "A random variate from the Student t-distribution.  The
   distribution function is,
   p(x) dx = {\Gamma((\nu + 1)/2) \over \sqrt{\pi \nu} \Gamma(\nu/2)}
   (1 + x^2/\nu)^{-(\nu + 1)/2} dx
   for -\infty < x < +\infty.")

(defun-gsl tdist-pdf (x nu)
  "gsl_ran_tdist_pdf" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a t-distribution with nu degrees of freedom, using the formula
   given in #'tdist.")

(defun-gsl tdist-P (x nu)
  "gsl_cdf_tdist_P" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the tdist distribution with nu degrees of freedom.")

(defun-gsl tdist-Q (x nu)
  "gsl_cdf_tdist_Q" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(x) for the tdist distribution with nu degrees of freedom.")

(defun-gsl tdist-Pinv (P nu)
  "gsl_cdf_tdist_Pinv" ((P :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   P(x) for the tdist distribution with nu degrees of freedom.")

(defun-gsl tdist-Qinv (Q nu)
  "gsl_cdf_tdist_Qinv" ((Q :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the tdist distribution with nu degrees of freedom.")

;;; Examples and unit test
(lisp-unit:define-test tdist
  (lisp-unit:assert-equal
   '("0.149893663745d+00" "0.679414287929d+00" "-0.161583395111d+01"
     "-0.160088628258d+01" "-0.170109355058d+01" "-0.437095974981d-01"
     "0.127611592766d+00" "-0.197312182555d-01" "-0.653466611720d+00"
     "0.203577132452d+00" "0.177650300478d+01")
   (lisp-unit:fp-sequence
    (letm ((rng (random-number-generator *mt19937* 0)))
      (rng-set rng 0)
      (loop for i from 0 to 10
	    collect
	    (tdist rng 10.0d0)))))
  (lisp-unit:assert-first-fp-equal
   "0.254647908947d+00"
   (tdist-pdf 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.647583617650d+00"
   (tdist-P 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.352416382350d+00"
   (tdist-Q 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (tdist-Pinv 0.6475836176504334d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.500000000000d+00"
   (tdist-Qinv 0.3524163823495667d0 1.0d0)))

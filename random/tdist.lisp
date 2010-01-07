;; Tdist distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2009-12-27 10:03:49EST tdist.lisp>
;;
;; Copyright 2006, 2007, 2008, 2009 Liam M. Healy
;; Distributed under the terms of the GNU General Public License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :gsl)

;;; /usr/include/gsl/gsl_randist.h

(export 'tdist)
(defmfun sample
    ((generator random-number-generator) (type (eql 'tdist)) &key nu)
  "gsl_ran_tdist"
  (((mpointer generator) :pointer) (nu :double))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "A random variate from the Student t-distribution.  The
   distribution function is,
   p(x) dx = {\Gamma((\nu + 1)/2) \over \sqrt{\pi \nu} \Gamma(\nu/2)}
   (1 + x^2/\nu)^{-(\nu + 1)/2} dx
   for -\infty < x < +\infty.")

(defmfun tdist-pdf (x nu)
  "gsl_ran_tdist_pdf" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a t-distribution with nu degrees of freedom, using the formula
   given in #'tdist.")

(defmfun tdist-P (x nu)
  "gsl_cdf_tdist_P" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Q (x nu)
  "gsl_cdf_tdist_Q" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
   Q(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Pinv (P nu)
  "gsl_cdf_tdist_Pinv" ((P :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   P(x) for the tdist distribution with nu degrees of freedom.")

(defmfun tdist-Qinv (Q nu)
  "gsl_cdf_tdist_Qinv" ((Q :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the tdist distribution with nu degrees of freedom.")

;;; Examples and unit test
(save-test tdist
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect
	    (sample rng 'tdist :nu 10.0d0)))
  (tdist-pdf 0.5d0 1.0d0)
  (tdist-P 0.5d0 1.0d0)
  (tdist-Q 0.5d0 1.0d0)
  (tdist-Pinv 0.6475836176504334d0 1.0d0)
  (tdist-Qinv 0.3524163823495667d0 1.0d0))

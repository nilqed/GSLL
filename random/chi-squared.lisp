;; Chi-squared distribution
;; Liam Healy, Sat Oct  7 2006 - 16:13
;; Time-stamp: <2009-12-27 10:00:03EST chi-squared.lisp>
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
;;; /usr/include/gsl/gsl_cdf.h

(export 'chi-squared)
(defmfun sample
    ((generator random-number-generator) (type (eql 'chi-squared)) &key nu)
  "gsl_ran_chisq"
  (((mpointer generator) :pointer) (nu :double))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "A random variate from the chi-squared distribution
  with nu degrees of freedom. The distribution function is
  p(x) dx = {1 \over 2 \Gamma(\nu/2) } (x/2)^{\nu/2 - 1} \exp(-x/2) dx
  x >= 0. ")

(defmfun chi-squared-pdf (x nu)
  "gsl_ran_chisq_pdf" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for a chi-squared distribution with nu degrees of freedom, using
   the formula given in #'chi-squared.")

(defmfun chi-squared-P (x nu)
  "gsl_cdf_chisq_P" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  P(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Q (x nu)
  "gsl_cdf_chisq_Q" ((x :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions
  Q(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Pinv (P nu)
  "gsl_cdf_chisq_Pinv" ((P :double) (nu :double))
  :c-return :double
  :documentation			; FDL
  "The inverse cumulative distribution functions
  P(x) for the chi-squared distribution with nu degrees of freedom.")

(defmfun chi-squared-Qinv (Q nu)
  "gsl_cdf_chisq_Qinv" ((Q :double) (nu :double))
  :c-return :double
  :documentation 			; FDL
  "The inverse cumulative distribution functions
   Q(x) for the chi-squared distribution with nu degrees of freedom.")

;;; Examples and unit test
(save-test chi-squared
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect
	    (sample rng 'chi-squared :nu 10.0d0)))
  (chi-squared-pdf 0.5d0 1.0d0)
  (chi-squared-P 0.5d0 1.0d0)
  (chi-squared-Q 0.5d0 1.0d0)
  (chi-squared-Pinv 0.5204998778130463d0 1.0d0)
  (chi-squared-Qinv 0.4795001221869537d0 1.0d0))




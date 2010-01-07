;; Exponential power distribution
;; Liam Healy, Sat Sep 30 2006
;; Time-stamp: <2009-12-27 10:00:02EST exponential-power.lisp>
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

(export 'exponential-power)
(defmfun sample
    ((generator random-number-generator) (type (eql 'exponential-power))
     &key a b)
  "gsl_ran_exppow"
  (((mpointer generator) :pointer) (a :double) (b :double))
  :definition :method
  :c-return :double
  :documentation			; FDL
  "A random variate from the exponential power distribution
   with scale parameter a and exponent b.  The distribution is
   p(x) dx = {1 \over 2 a \Gamma(1+1/b)} \exp(-|x/a|^b) dx
   for x >= 0.  For b = 1 this reduces to the Laplace
   distribution.  For b = 2 it has the same form as a gaussian
   distribution, but with a = \sqrt{2} \sigma.")

(defmfun exponential-power-pdf (x a b)
  "gsl_ran_exppow_pdf" 
  ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The probability density p(x) at x
   for an exponential power distribution with scale parameter a
   and exponent b, using the formula given for #'exponential-power.")

(defmfun exponential-power-P (x a b)
  "gsl_cdf_exppow_P" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution function
  P(x), for the exponential power distribution with
  parameters a and b.")

(defmfun exponential-power-Q (x a b)
  "gsl_cdf_exppow_Q" ((x :double) (a :double) (b :double))
  :c-return :double
  :documentation			; FDL
  "The cumulative distribution functions Q(x)
  for the exponential power distribution with
  parameters a and b.")

;;; Examples and unit test
(save-test exponential-power
  (let ((rng (make-random-number-generator +mt19937+ 0)))
      (loop for i from 0 to 10
	    collect
	    (sample rng 'exponential-power :a 1.0d0 :b 2.0d0)))
  (exponential-power-pdf 0.0d0 1.0d0 2.0d0)
  (exponential-power-P 1.0d0 1.0d0 2.0d0)
  (exponential-power-Q 1.0d0 1.0d0 2.0d0))

;********************************************************
; file:        laguerre.lisp                           
; description: Laguerre polynomials                    
; date:        Fri Apr 28 2006 - 20:40                   
; author:      Liam M. Healy                             
; modified:    Sat Apr 29 2006 - 18:32
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl laguerre-1 ((a :double) (x :double))
  "gsl_sf_laguerre_1_e"
  :return (sf-result)
  :documentation "The generalized Laguerre polynomial
   @math{L^a_1(x)} using explicit representations.")

(defun-gsl laguerre-2 ((a :double) (x :double))
  "gsl_sf_laguerre_2_e"
  :return (sf-result)
  :documentation  "The generalized Laguerre polynomial
   @math{L^a_2(x)} using explicit representations.")

(defun-gsl laguerre-3 ((a :double) (x :double))
  "gsl_sf_laguerre_3_e"
  :return (sf-result)
  :documentation "The generalized Laguerre polynomial
   @math{L^a_3(x)} using explicit representations.")

(defun-gsl laguerre ((n :int) (a :double) (x :double))
  "gsl_sf_laguerre_n_e"
  :return (sf-result)
  :documentatiOn "The generalized Laguerre polynomials
  @math{L^a_n(x)} for @math{a > -1}, @math{n >= 0}.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test laguerre
  (lisp-unit:assert-first-fp-equal
   "-0.100000000000d+01"
   (laguerre-1 1.0d0 3.0d0))
  (lisp-unit:assert-first-fp-equal
   "-0.150000000000d+01"
   (laguerre-2 1.0d0 3.0d0))
  (lisp-unit:assert-first-fp-equal
   "-0.500000000000d+00"
   (laguerre-3 1.0d0 3.0d0)))

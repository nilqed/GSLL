;********************************************************
; file:        hypergeometric.lisp                       
; description: Hypergeometric function                   
; date:        Fri Apr 28 2006 - 23:00                   
; author:      Liam M. Healy                             
; modified:    Sat Apr 29 2006 - 18:35
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun-gsl hypergeometric-0F1 ((c :double) (x :double))
  "gsl_sf_hyperg_0F1_e"
  :return (sf-result)
  :documentation "The hypergeometric function @math{0F1(c,x)}.")

(defun-gsl hypergeometric-1F1-int ((m :int) (n :int) (x :double))
  "gsl_sf_hyperg_1F1_int_e"
  :return (sf-result)
  :documentation "The confluent hypergeometric function
   @math{1F1(m,n,x) = M(m,n,x)} for integer parameters @var{m}, @var{n}.")

(defun-gsl hypergeometric-1F1 ((a :double) (b :double) (x :double))
  "gsl_sf_hyperg_1F1_e"
  :return (sf-result)
  :documentation "Compute the confluent hypergeometric function
  @math{1F1(a,b,x) = M(a,b,x)} for general parameters @var{a}, @var{b}.")

(defun-gsl hypergeometric-U-int ((m :int) (n :int) (x :double))
  "gsl_sf_hyperg_U_int_e"
  :return (sf-result)
  :documentation "The confluent hypergeometric function
   @math{U(m,n,x)} for integer parameters @var{m}, @var{n}.")

(defun-gsl hypergeometric-U-int-e10 ((m :int) (n :int) (x :double))
  "gsl_sf_hyperg_U_int_e10_e"
  :return (sf-result-e10)
  :documentation "The confluent hypergeometric function
  @math{U(m,n,x)} for integer parameters @var{m}, @var{n} using the
  @code{gsl_sf_result_e10} type to return a result with extended range.")

(defun-gsl hypergeometric-U ((a :double) (b :double) (x :double))
  "gsl_sf_hyperg_U_e"
  :return (sf-result)
  :documentation "The confluent hypergeometric function @math{U(a,b,x)}.")

(defun-gsl hypergeometric-U-e10 ((a :double) (b :double) (x :double))
  "gsl_sf_hyperg_U_e10_e"
  :return (sf-result-e10)
  :documentation "The confluent hypergeometric function
  @math{U(a,b,x)} using the @code{gsl_sf_result_e10} type to return a
  result with extended range.")

(defun-gsl hypergeometric-2F1
    ((a :double) (b :double) (c :double) (x :double))
  "gsl_sf_hyperg_2F1_e"
  :return (sf-result)
  :documentation "The Gauss hypergeometric function
  @math{2F1(a,b,c,x)} for @math{|x| < 1}. If the arguments
  @math{(a,b,c,x)} are too close to a singularity then the function can
  return the error code :EMAXITER when the series
  approximation converges too slowly.  This occurs in the region of
  @math{x=1}, @math{c - a - b = m} for integer m.")

(defun-gsl hypergeometric-2F1-conj
    (((realpart a) :double) ((imagpart a) :double) (c :double) (x :double))
  "gsl_sf_hyperg_2F1_conj_e"
  :function (a c x)
  :return (sf-result)
  :documentation "The Gauss hypergeometric function
  @math{2F1(a, a*, c, x)} with complex parameters 
  for @math{|x| < 1}.")

(defun-gsl hypergeometric-renorm
    ((a :double) (b :double) (c :double) (x :double))
  "gsl_sf_hyperg_2F1_renorm_e"
  :return (sf-result)
  :documentation "The renormalized Gauss hypergeometric function
  @math{2F1(a,b,c,x) / \Gamma(c)} for @math{|x| < 1}.")

(defun-gsl hypergeometric-conj-renorm
    (((realpart a) :double) ((imagpart a) :double) (c :double) (x :double))
  "gsl_sf_hyperg_2F1_conj_renorm_e"
  :function (a b c x)
  :return (sf-result)
  :documentation "The renormalized Gauss hypergeometric function
  @math{2F1(a, a*, c, x) / \Gamma(c)} for @math{|x| < 1}.")

(defun-gsl hypergeometric-2F0
    ((a :double) (b :double) (x :double))
  "gsl_sf_hyperg_2F0_e"
  :return (sf-result)
  :documentation "The hypergeometric function 
  @math{2F0(a,b,x)}.  The series representation
  is a divergent hypergeometric series.  However, for @math{x < 0} we
  have @math{2F0(a,b,x) = (-1/x)^a U(a,1+a-b,-1/x)}")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

(lisp-unit:define-test hypergeometric
  (lisp-unit:assert-first-fp-equal
   "0.376219569108d+01"
   (hypergeometric-0f1 0.5d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.543656365692d+01"
   (hypergeometric-1F1-int 2 1 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.543656365692d+01"
   (hypergeometric-1F1 2.0d0 1.0d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.192694724646d+00"
   (hypergeometric-U 2.0d0 1.0d0 1.0d0))
  (lisp-unit:assert-first-fp-equal
   "0.662959493455d+01"
   (hypergeometric-2F1-conj #c(1.0d0 0.5d0) 0.5d0 0.6d0))
  (lisp-unit:assert-first-fp-equal
   "0.435139241256d-01"
   (hypergeometric-2F0 1.0d0 2.0d0 -20.0d0)))
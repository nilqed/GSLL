;; Dilogarithm
;; Liam Healy, Fri Mar 17 2006 - 18:44
;; Time-stamp: <2009-11-14 10:27:52EST dilogarithm.lisp>

(in-package :gsl)

;;; dilog merge complex and real
(export 'dilogarithm)
(defgeneric dilogarithm (x)
  (:documentation			; FDL
   "The dilogarithm."))

(defmfun dilogarithm ((x float))
  "gsl_sf_dilog_e" ((x :double) (ret sf-result))
  :definition :method)

(defmfun dilogarithm ((x complex))
  "gsl_sf_complex_dilog_e"
  (((abs x) :double) ((phase x) :double) (re sf-result) (im sf-result))
  :definition :method
  :return ((complex (val re) (val im)) (complex (err re) (err im))))

(save-test dilogarithm
	   (dilogarithm 1.0d0)
	   (dilogarithm #c(0.0d0 1.0d0)))



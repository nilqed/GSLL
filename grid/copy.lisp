;; Copy objects
;; Liam Healy 2009-12-21 10:16:04EST copy.lisp
;; Time-stamp: <2009-12-21 10:24:07EST copy.lisp>
;; $Id: $

(in-package :c-array)

(export '(copy-to-destination array-mismatch))

(defgeneric copy-to-destination (object destination)
  ;; This copies values into an existing object.  Methods are defined
  ;; for non-array GSL objects that have _memcpy functions defined.
  ;; Defined for
  ;; random-number-generator, quasi-random-number-generator,
  ;; histogram, histogram2d, permutation, combination.
  (:documentation "Copy contents into existing object."))

(define-condition array-mismatch (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Arrays must be of equal size for copying.")))
  (:documentation
   "An error indicating that the two arrays do not have 
   the same dimensions."))

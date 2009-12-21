;; Copy objects
;; Liam Healy 2009-12-21 10:16:04EST copy.lisp
;; Time-stamp: <2009-12-21 10:46:41EST copy.lisp>
;; $Id: $

(in-package :c-array)

(export
 '(copy clone copy-to-destination copy-making-destination array-mismatch))

(defun copy (object &optional destination)
  "Create a duplicate object."
  (if destination
      (copy-to-destination object destination)
      (copy-making-destination object)))

;;; Could be part of copy when the second argument is optional?
(defgeneric clone (object)
  (:documentation "Create a duplicate object."))

(defgeneric copy-making-destination (object)
  (:documentation "Create new duplicate object."))

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

;; Methods for grid functions
;; Liam Healy 2009-12-21 11:19:00EST methods.lisp
;; Time-stamp: <2009-12-21 21:56:38EST methods.lisp>

(in-package :c-array)

(defmethod grid:grid-rank ((object foreign-array))
  (length (dimensions object)))

(defmethod grid:grid-dimensions ((object foreign-array))
  (dimensions object))

(defmethod grid:make-grid-data
    ((type (eql 'foreign-array)) dimensions rest-spec
     &key (initial-element nil initial-element-p))
  (make-instance 'foreign-array
		 :dimensions dimensions
		 :element-type rest-spec
		 :initial-element initial-element))

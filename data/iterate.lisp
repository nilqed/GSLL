;; Iterate
;; Norman Werner 2009-05-26 22:23:40EDT iterate.lisp
;; Time-stamp: <2009-12-27 09:42:05EST iterate.lisp>
;;
;; Copyright 2009 Norman Werner, Liam M. Healy
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

(in-package :iter)

#|
As an example try:
(defparameter m1 #m(1 2 3 ^ 0 6 8))

(iter:iter (iter:for e :matrix-element m1) (princ e) (princ " "))

(iter:iter (iter:for (row column) :matrix-element-index m1)
	   (princ (gsll:maref m1 row column)) (princ " "))
|#


(defclause-sequence matrix-row matrix-row-index
  :access-fn
  (lambda (marray index)
    (check-type marray gsl:matrix)
    (gsll:row marray index))
  :size-fn
  (lambda (marray)
    (check-type marray gsl:matrix)
    (c-array:dim0 marray))
  :element-type t :sequence-type t
  :element-doc-string "(copied) rows of a matrix"
  :index-doc-string "index of the rows in a matrix")

(defclause-sequence matrix-column matrix-column-index
  :access-fn
  (lambda (marray index)
    (check-type marray gsl:matrix)
    (gsll:column marray index))
  :size-fn
  (lambda (marray)
    (check-type marray gsl:matrix)
    (c-array:dim1 marray))
  :element-type t :sequence-type t
  :element-doc-string "(copied) columns of a matrix"
  :index-doc-string "index of the columns in a matrix")

(defclause-sequence vector-element vector-element-index
  :access-fn
  (lambda (vector index)
    (check-type vector gsl:mvector)
    (gsll:maref vector index))
  :size-fn
  (lambda (vector)
    (check-type vector gsl:mvector)
    (c-array:dim0 vector))
  :element-type t :sequence-type t
  :element-doc-string "(copied) elements of a vector"
  :index-doc-string "index of elements in a vector")

(defmacro-driver (FOR element matrix-element matrix)
  "Iterates over all (copied) elements in matrix"
  (cl-utilities:with-unique-names (row-index col-index row-size col-size m)
      (when generate
	(error "Not yet implemented a generate clause for matrix-element."))
      `(progn
	 (with ,m = ,matrix)
	 (with ,row-index = 0)
	 (with ,col-index = 0)
	 (with ,row-size = (c-array:dim0 ,m))
	 (with ,col-size = (c-array:dim1 ,m))
	 (for ,element
	      :next
	      (if (>= ,row-index ,row-size)
		  (terminate)
		  (if (>= ,col-index ,col-size)
		      (progn
			(setf ,col-index 0)
			(incf ,row-index)
			(if (>= ,row-index ,row-size)
			    (terminate)
			    (gsl:maref ,m ,row-index ,col-index)))
		      (prog1
			  (gsl:maref ,m ,row-index ,col-index)
			(incf ,col-index))))))))

(defmacro-driver (FOR indexes matrix-element-index
		      matrix)
  "Iterates over the indexes in matrix. indexes is a list like (row-index-name
   column-index-name) "
  (cl-utilities:with-unique-names (row-index col-index row-size col-size m)
    (when generate
      (error "Not yet implemented a generate clause for matrix-element-index."))
    `(progn
       (with ,m = ,matrix)
       (with ,row-index = 0)
       (with ,col-index = 0)
       (with ,row-size =  (c-array:dim0 ,m))
       (with ,col-size =  (c-array:dim1 ,m))
       (for ,indexes next (if (>= ,row-index ,row-size)
			      (terminate)
			      (if (>= ,col-index ,col-size)
				  (progn
				    (setf ,col-index 0)
				    (incf ,row-index)
				    (if (>= ,row-index ,row-size)
					(terminate)
					(list ,row-index ,col-index)))
				  (prog1
				      (list ,row-index ,col-index)
				    (incf ,col-index))))))))

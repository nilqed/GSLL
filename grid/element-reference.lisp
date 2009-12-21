;; Get/set array or elements: cl-array, maref
;; Liam Healy 2009-12-21 09:40:27EST element-reference.lisp
;; Time-stamp: <2009-12-21 09:47:31EST element-reference.lisp>

(in-package :c-array)

(export '(cl-array maref))

;;; These functions handle the details of converting between the C
;;; representation of arrays and Common Lisp arrays.  The function
;;; #'maref can be treated like #'aref for reading and setting
;;; elements, except that it is limited to two indices (vectors or
;;; matrices only) and requires a final argument with the element
;;; type when taking a C pointer as the first argument (only needed
;;; in the case of solve-minimize-fit for callbacks and some return
;;; values).

;;; Both these functions take one of the following class arguments for
;;; the array:
;;; - a foreign-array
;;; - a Common Lisp array

;;;;****************************************************************************
;;;; Get the entire array:  cl-array
;;;;****************************************************************************

(defgeneric cl-array (object &optional array-rank element-type)
  (:documentation
   "The array as a CL native array.  The object may be a foreign-array object,
    or an ordinary CL array of one or two dimensions.  Optional
    arguments array-rank and element-type are used only for
    pointers.")
  (:method ((object foreign-array) &optional array-rank element-type)
    (declare (ignore array-rank element-type))
    #-native (copy-c-to-cl object)
    (slot-value object 'cl-array))
  (:method ((object array) &optional array-rank element-type)
    ;; For compatibility, work on CL arrays as well.
    (declare (ignore array-rank element-type))
    object))

;;;;****************************************************************************
;;;; Get or set elements of the array:  maref, (setf maref)
;;;;****************************************************************************

(defgeneric maref (object index &optional index2 type)
  (:documentation
   "An element of the data.  The object may be a foreign-array
   object or an ordinary CL array of one or two dimensions.")
  (:method ((object foreign-array) index &optional index2 type)
    (declare (ignore type))
    #-native (copy-c-to-cl object)
    (if index2
	(aref (cl-array object) index index2)
	(aref (cl-array object) index)))
  (:method ((object array) index &optional index2 type)
    ;; For compatibility, work on CL arrays as well.
    (declare (ignore type))
    (if index2
	(aref object index index2)
	(aref object index))))

;;; Alternative to complete copy is to mark which elements have
;;; changed and just copy them.  Is it worth it?

(defgeneric (setf maref) (value object index &optional index2 type)
  (:documentation
   "Set an element of the data.  The object may be a
    foreign-array object or an ordinary CL array of one or two
    dimensions.")
  (:method (value (object foreign-array) index &optional index2 type)
    (declare (ignore type))
    (if index2
	(setf (aref (slot-value object 'cl-array) index index2) value)
	(setf (aref (slot-value object 'cl-array) index) value))
    #-native (setf (c-invalid object) t))
  (:method (value (object array) index &optional index2 type)
    ;; For compatibility, work on CL arrays as well.
    (declare (ignore type))
    (if index2
	(setf (aref object index index2) value)
	(setf (aref object index) value))))


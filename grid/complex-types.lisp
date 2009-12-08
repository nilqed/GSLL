;; Complex number types
;; Liam Healy 2009-01-13 21:24:05EST complex-types.lisp
;; Time-stamp: <2009-12-06 22:05:59EST complex-types.lisp>

(in-package :c-array)

(export '(complex-double-c complex-float-c component-float-type component-type))

;;;;****************************************************************************
;;;; Complex types
;;;;****************************************************************************

#+fsbv
(fsbv:defcstruct
    (complex-float-c :constructor complex :deconstructor (realpart imagpart))
  (dat :float :count 2))

#-fsbv
(cffi:defcstruct complex-float-c (dat :float :count 2))

#+fsbv
(fsbv:defcstruct
    (complex-double-c :constructor complex :deconstructor (realpart imagpart))
  (dat :double :count 2))

#-fsbv
(cffi:defcstruct complex-double-c (dat :double :count 2))

#+long-double
(cffi:defcstruct complex-long-double-c
  (dat :long-double :count 2))

(defun clean-type (type)
  ;; SBCL at least will specify limits on the type, e.g.
  ;; (type-of #C(1.0 2.0))
  ;; (COMPLEX (DOUBLE-FLOAT 1.0 2.0))
  ;; This cleans that up to make
  ;; (clean-type (type-of #C(1.0 2.0)))
  ;; (COMPLEX DOUBLE-FLOAT)
  (if (and (subtypep type 'complex) (listp (second type)))
      (list (first type) (first (second type)))
      type))

(defun component-float-type (eltype)
  "The type of the component of this type (complex)."
  (if (subtypep eltype 'complex)
      ;; complex: use the component type
      (second eltype)
      eltype))

(defun component-type (eltype)
  (cl-cffi (component-float-type eltype)))


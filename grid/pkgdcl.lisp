;; Declaration of package
;; Liam Healy 2009-12-06 19:41:09EST pkgdcl.lisp
;; Time-stamp: <2009-12-21 08:56:53EST pkgdcl.lisp>

(in-package :cl-user)

(defpackage c-array
  (:use :common-lisp :cffi))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :native *features*))

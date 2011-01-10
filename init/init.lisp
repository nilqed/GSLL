;; Load GSL
;; Liam Healy Sat Mar  4 2006 - 18:53
;; Time-stamp: <2011-01-10 18:28:14EST init.lisp>
;;
;; Copyright 2006, 2007, 2008, 2009, 2010, 2011 Liam M. Healy
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

(defpackage gsll
  (:nicknames :gsl)
  (:use :common-lisp :cffi)
  (:import-from
   :grid #:dimensions
    #:element-type #:foreign-array #:matrix #:dim0 #:dim1 #:^ #:copy)
  (:export #:element-type #:dim0 #:dim1  #:copy))

;;; Where there is a symbol conflict, take the other one.
(shadow '#:row :antik-user) ; conflict with grid:row; they are equivalent
(shadow '#:column :antik-user) ; conflict with grid:column; they are equivalent
(shadow '#:sum :antik-user) ; conflict of histogram function with iterate:sum, both pretty obscure
(shadow '#:multiply :antik-user) ; conflict if GSL's duplicate '* with iterate:multiply
(shadow '#:si :antik-user) ; si units symbol-macro vs. GSLL's sine integral; technically not a conflict
(shadow '#:polar-to-rectangular :antik-user) ; GSLL's doesn't use vectors
(shadow '#:rectangular-to-polar :antik-user) ; GSLL's doesn't use vectors
;;; Where there is a symbol conflict, take GSLL's
(shadowing-import 'gsl::iterate :antik-user) ; conflict with iterate:iterate, but iterate:iter is a synonym

(use-package :gsll :antik-user)

(in-package :gsl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gsl-config (arg)
    "A wrapper for tool `gsl-config'."
    (with-input-from-string
        (s (with-output-to-string (asdf::*verbose-out*)
             (asdf:run-shell-command "gsl-config ~s" arg)))
      (read-line s)
      (read-line s)))
  #+unix
  (defun gsl-config-pathname (pn)
    (merge-pathnames pn (pathname (format nil "~a/" (gsl-config "--prefix"))))))

(cffi:define-foreign-library libgslcblas
  (:darwin #.(gsl-config-pathname "lib/libgslcblas.dylib"))
  (:cygwin "cyggslcblas-0.dll")
  (:unix (:or "libgslcblas.so.0" "libgslcblas.so"))
  (t (:default "libgslcblas")))
   
(cffi:use-foreign-library libgslcblas)

;; When calling libgsl from emacs built for windows and slime, and
;; using clisp built for cygwin, we have to load lapack/cygblas.dll
;; before loading cyggsl-0.dll
#+(and clisp cygwin)
(cffi:load-foreign-library "/lib/lapack/cygblas.dll")

(cffi:define-foreign-library libgsl
  (:darwin #. (gsl-config-pathname "lib/libgsl.dylib"))
  (:cygwin "cyggsl-0.dll")
  (:unix (:or "libgsl.so.0" "libgsl.so"))
  (t (:default "libgsl")))
   
(cffi:use-foreign-library libgsl)

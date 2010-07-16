;; Load GSL
;; Liam Healy Sat Mar  4 2006 - 18:53
;; Time-stamp: <2010-07-15 22:26:30EDT init.lisp>
;;
;; Copyright 2006, 2007, 2008, 2009 Liam M. Healy
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
   :grid
   #:cl-array #:dimensions #:element-type
   #:foreign-array #:matrix #:dim0 #:dim1 #:^
   #:copy)
  (:shadowing-import-from :grid #:foreign-pointer)
  (:export
   #:cl-array #:dimensions #:element-type #:dim0 #:dim1
   #:copy))

(cffi:define-foreign-library libgslcblas
    (:darwin
     (:or "/opt/local/lib/libgslcblas.dylib" "/sw/lib/libgslcblas.dylib"
	  "/usr/local/lib/libgslcblas.dylib"))
  (:cygwin (:or "/bin/cyggslcblas-0.dll"))
  (:unix (:or "/usr/lib/libgslcblas.so.0" "/usr/lib/libgslcblas.so"
	      "/usr/lib64/libgslcblas.so.0" "/usr/lib64/libgslcblas.so"))
  (t (:default "libgslcblas")))
   
(cffi:use-foreign-library libgslcblas)

;; When calling libgsl from emacs built for windows and slime, and
;; using clisp built for cygwin, we have to load lapack/cygblas.dll
;; before loading cyggsl-0.dll
#+(and clisp cygwin)
(cffi:load-foreign-library "/lib/lapack/cygblas.dll")

(cffi:define-foreign-library libgsl
    (:darwin
     (:or "/opt/local/lib/libgsl.dylib" "/sw/lib/libgsl.dylib"
	  "/usr/local/lib/libgsl.dylib"))
  (:cygwin (:or "/bin/cyggsl-0.dll"))
  (:unix (:or "/usr/lib/libgsl.so.0" "/usr/lib/libgsl.so"
	      "/usr/lib64/libgsl.so.0" "/usr/lib64/libgsl.so"))
  (t (:default "libgsl")))
   
(cffi:use-foreign-library libgsl)


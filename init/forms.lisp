;; Lisp forms
;; Liam Healy 2009-03-07 15:49:25EST forms.lisp
;; Time-stamp: <2009-12-27 09:50:31EST forms.lisp>
;;
;; Copyright 2009 Liam M. Healy
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

(in-package :gsl)

;;;;****************************************************************************
;;;; Arglists
;;;;****************************************************************************

(defparameter *defmfun-llk* '(&optional &key &aux)
  "Possible lambda-list keywords.")

(defparameter *defmfun-optk* '(&optional &key)
  "Possible optional-argument keywords.")

(defun arglist-plain-and-categories
    (arglist &optional (include-llk t))
  "Get arglist without classes and a list of categories."
  (loop for arg in arglist
     with getting-categories = t and categories
     do
     (when (and getting-categories (member arg *defmfun-llk*))
       (setf getting-categories nil))
     (when (and getting-categories (listp arg))
       ;; Collect categories (classes), but not default values to
       ;; optional arugments.
       (pushnew (second arg) categories))
     when (or (not (member arg *defmfun-llk*)) include-llk)
     collect
     (if (listp arg) (first arg) arg)
     into noclass-arglist
     finally (return (values noclass-arglist categories))))

(defun category-for-argument (arglist symbol)
  "Find the category (class) for the given argument."
  (multiple-value-bind (plain cats)
      (arglist-plain-and-categories arglist)
    (let ((pos (position symbol plain)))
      (when pos
	(nth pos cats)))))

(defun after-llk (arglist)
  "The portion of the arglist from the first llk on."
  (loop for elt in arglist
       with seen = nil
       when (or seen (member elt *defmfun-llk*))
       do (setf seen t)
       when seen collect elt))

#|
;;; Oddly, this gives a warning in SBCL but works the same.
(defun after-llk (arglist)
  "The portion of the arglist from the first llk on."
  (when (intersection *defmfun-llk* arglist)
    (subseq arglist
	    (some (lambda (itm) (position itm arglist)) *defmfun-llk*))))
|#


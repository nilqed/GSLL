;; GSL library version
;; Liam Healy 2016-08-06 10:37:52EDT gsl-version.lisp
;; Time-stamp: <2016-08-06 10:48:20EDT gsl-version.lisp>
;;
;; Copyright 2016 Liam M. Healy
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

(cffi:defcvar ("gsl_version" *gsl-version* :read-only t) :string
          "The version of the GSL library being used.")
(export '*gsl-version*)

(defun have-at-least-gsl-version (major-minor)
  "The GSL version currently running is at least the specified major/minor version."
  (or (null major-minor)
      (let* ((sep-pos (position #\. *gsl-version*))
	     (my-major
	      (read-from-string *gsl-version* nil nil :end sep-pos))
	     (my-minor
	      (read-from-string *gsl-version* nil nil :start (1+ sep-pos))))
	(and (>= my-major (first major-minor))
	     (>= my-minor (second major-minor))))))

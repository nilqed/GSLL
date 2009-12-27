;; Jacobian elliptic functions
;; Liam Healy, Mon Mar 20 2006 - 22:21
;; Time-stamp: <2009-12-27 10:10:05EST elliptic-functions.lisp>
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

(in-package :gsl)

(defmfun jacobian-elliptic-functions (u m)
  "gsl_sf_elljac_e"
  ((u :double) (m :double) (sn sf-result) (cn sf-result) (dn sf-result))
  :return ((val sn) (val cn) (val dn) (err sn) (err cn) (err dn))
  :documentation			; FDL
  "The Jacobian elliptic functions sn(u|m),
  cn(u|m), dn(u|m) computed by descending Landen transformations.")

;;; > (jacobian-elliptic-functions 0.61802d0 0.5d0)
;;; 0.564575752943391
;;; 0.8253812568676386
;;; 0.916857191493965
;;; > (jacobian-elliptic-functions 0.2d0 0.81d0)
;;; 0.19762082367187697
;;; 0.9802785369736752
;;; 0.9840560289645665
;;; > (jacobian-elliptic-functions 0.61802d0 1.5d0)
;;; ;;;error

(save-test elliptic-functions
	   (jacobian-elliptic-functions 0.2d0 0.81d0)
	   (jacobian-elliptic-functions 0.61802d0 1.5d0))

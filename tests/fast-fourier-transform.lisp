;; Fast fourier transform tests
;; Liam Healy 2010-08-14 11:58:26EDT fast-fourier-transform.lisp
;; Time-stamp: <2010-08-14 18:32:54EDT fast-fourier-transform.lisp>
;;
;; Copyright 2010 Liam M. Healy
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

(defvar *allowed-ticks* 1000000)

(defmacro fft-complex-result-check (form)
  "T if all FFT tests pass."
  `(multiple-value-bind
	 (dft fft original fft-roundtrip fft-reverse)
       (reset-urand)
     ,form
     (let ((lisp-unit:*epsilon* (* *allowed-ticks* +dbl-epsilon+))
	   (lisp-unit:*measure* :infinity))
       (lisp-unit:assert-norm-equal dft fft)
       (lisp-unit:assert-norm-equal original fft-roundtrip)
       (lisp-unit:assert-norm-equal original fft-reverse))))

(defmacro fft-real-result-check (form)
  "T if all FFT tests pass."
  `(multiple-value-bind
	 (dft fft original fft-roundtrip)
       (reset-urand)
     ,form
     (let ((lisp-unit:*epsilon* (* *allowed-ticks* +dbl-epsilon+))
	   (lisp-unit:*measure* :infinity))
       (lisp-unit:assert-norm-equal dft fft)
       (lisp-unit:assert-norm-equal original fft-roundtrip))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fft-test-forms (size stride)
    `((fft-real-result-check
       (test-fft-noise 'double-float ,size :stride ,stride))
      (fft-real-result-check
       (test-fft-noise 'single-float ,size :stride ,stride))
      (fft-complex-result-check
       (test-fft-noise '(complex double-float) ,size :stride ,stride))
      (fft-complex-result-check
       (test-fft-noise '(complex single-float) ,size :stride ,stride)))))

(defmacro all-fft-test-forms (size-max stride-max &optional additional-single-stride)
  `(lisp-unit:define-test fast-fourier-transform
     ,@(loop for size from 1 to size-max
	  append
	  (loop for stride from 1 to stride-max
	     append (fft-test-forms size stride)))
     ,@(when additional-single-stride
	     (loop for size in additional-single-stride
		append
		(fft-test-forms size 1)))))

(all-fft-test-forms 9 3 (64 99))

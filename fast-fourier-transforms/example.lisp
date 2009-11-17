;; Example FFT: transform a pulse (using the "clean" fft interface)
;; Sumant Oemrawsingh, Sat Oct 31 2009 - 00:24
;; Time-stamp: <2009-11-15 23:22:45EST example.lisp>

(in-package :gsl)

;;;;****************************************************************************
;;;; Pulse test
;;;;****************************************************************************

;;; Here is an example program modelled after the example given in Section
;;; 15.3 of the GSL Manual, which computes the FFT of a short pulse. To make
;;; the resulting fourier transform real the pulse is defined for equal
;;; positive and negative times (-10 ... 10), where the negative times wrap
;;; around the end of the array.
;;; 
;;; The output array from the example in Section 15.3 of the GSL Manual can be
;;; reproduced with:
;;; (fft-pulse-test '(complex double-float) 128)
;;;
;;; This example program also yields the same output array as the example
;;; program in Section 15.4 of the GSL Manual:
;;; (fft-pulse-test '(complex double-float) 630)

(defun fft-pulse-test (element-type dimension)
  (assert (and (integerp dimension) (> dimension 20)))
  (let ((pulse (make-marray element-type :dimensions dimension))
        (init-value (coerce 1 element-type)))
    (setf (maref pulse 0) init-value)
    (loop for i from 1 to 10
          do (setf (maref pulse i) init-value
                   (maref pulse (- dimension i)) init-value))
    (forward-fourier-transform pulse)))

(save-test
  fft
  (fft-pulse-test '(complex single-float) 128)
  (fft-pulse-test '(complex single-float) 630)
  (fft-pulse-test '(complex double-float) 128)
  (fft-pulse-test '(complex double-float) 630)
  (fft-pulse-test 'single-float 128)
  (fft-pulse-test 'single-float 630)
  (fft-pulse-test 'double-float 128)
  (fft-pulse-test 'double-float 630))

;;;;****************************************************************************
;;;; Random vector transformations, from the GSL tests
;;;;****************************************************************************

;; From gsl-1.11/fft/urand.c
(let ((urand-seed 1))
  (defun urand ()
    "Generate a random number.  See fft/urand.c."
    (setf urand-seed (logand #x7fffffff (+ 12345 (* urand-seed 1103515245))))
    (/ urand-seed 2147483648.d0))
  (defun reset-urand ()
    (setf urand-seed 1)
    (values)))

;; (make-urand-vector '(complex double-float) 5)
(defun make-urand-vector (element-type dimension &key (stride 1))
  "Make a vector with random elements."
  (let ((vec (make-marray `(complex ,(component-float-type element-type))
			  :dimensions (list (* stride dimension)))))
    (loop for i from 0 below (* stride dimension) by stride
       do
       (setf (maref vec i)
	     (if (subtypep element-type 'complex)
		 (coerce (complex (urand) (urand)) element-type)
		 (complex (coerce (urand) element-type)))))
    vec))

(defun realpart-vector (complex-vector)
  "The real vector consisting of the real part of the complex vector."
  (let ((real-vector
	 (make-marray
	  (component-float-type (element-type complex-vector))
	  :dimensions (dimensions complex-vector))))
    (loop for i below (total-size complex-vector) do
	 (setf (maref real-vector i)
	       (realpart (maref complex-vector i))))
    real-vector))

(defun test-fft-noise (element-type size &key (stride 1))
  "Test for FFT with random data (noise); returns the DFT answer and the computed FFT answer.
   See test_real_radix2 etc. in fft/test.mc."
  (let* ((random-vector (make-urand-vector element-type size :stride stride))
	 (forward
	  (forward-fourier-transform
	   (if (subtypep element-type 'complex)
               (copy-making-destination random-vector)
	       (realpart-vector random-vector))
	   :stride stride)))
    (values
     (forward-discrete-fourier-transform random-vector :stride stride)
     (if (subtypep element-type 'complex)
	 forward
	 (unpack forward :unpack-type 'complex :stride stride)))))

;;(test-fft-noise 'double-float 10 :stride 5)
;;(test-fft-noise '(complex double-float) 10 :stride 5)

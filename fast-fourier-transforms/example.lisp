;; Example FFT: transform a pulse (using the "clean" fft interface)
;; Sumant Oemrawsingh, Sat Oct 31 2009 - 00:24
;; Time-stamp: <2009-11-08 22:39:41EST example.lisp>

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

(in-package :gsl)

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
(defun make-urand-vector (element-type dimension)
  "Make a vector with random elements."
  (let ((vec (make-marray element-type :dimensions (list dimension))))
    (loop for i from 0 below dimension
       do
       (setf (maref vec i) (complex (urand))))
    vec))

(defun fft-signal-real-noise (element-type size)
  "From generated random data, return the data and DFT.
   See fft_signal_real_noise in fft/signals.mc."
  (let ((data (make-urand-vector (list 'complex element-type) size)))
    (values data (forward-discrete-fourier-transform data))))

(defun test-real-radix2 (element-type size)
  "Test for FFT; returns the DFT answer and the computed FFT answer.
   See test_real_radix2 in fft/test.mc."
  (multiple-value-bind (complex-vector dft-answer)
      (fft-signal-real-noise element-type size) ; results are complex
    (let ((real-vector (make-marray element-type :dimensions (list size))))
      (loop for i below size do
	   (setf (maref real-vector i)
		 (realpart (maref complex-vector i))))
      (let ((forward-fft (forward-fourier-transform real-vector)))
	(values dft-answer (unpack forward-fft :unpack-type 'complex))))))

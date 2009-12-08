;; Example FFT: transform a pulse (using the "clean" fft interface)
;; Sumant Oemrawsingh, Sat Oct 31 2009 - 00:24
;; Time-stamp: <2009-12-06 22:05:01EST example.lisp>

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
  fft-pulse
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
  (let ((vec (make-marray `(complex ,(c-array:component-float-type element-type))
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
	  (c-array:component-float-type (element-type complex-vector))
	  :dimensions (dimensions complex-vector))))
    (loop for i below (total-size complex-vector) do
	 (setf (maref real-vector i)
	       (realpart (maref complex-vector i))))
    real-vector))

(defun size-vector-real (vector &key (stride 1))
  "Return the size of a vector while taking the stride into account."
  (coerce (floor (size vector) stride) (element-type vector)))

(defun vector/length (vector stride)
  (elt/ vector (size-vector-real vector :stride stride)))

(defun test-real-fft-noise (vector &key (stride 1))
  "Test forward and inverse FFT for a real vector, and return both results in unpacked form."
  (let* ((forward
	  (forward-fourier-transform (realpart-vector vector) :stride stride))
         (inverse
	  (forward-fourier-transform (copy forward) :half-complex t :stride stride)))
    (values (unpack forward :unpack-type 'complex :stride stride)
            (unpack (vector/length inverse stride) :stride stride))))

(defun test-complex-fft-noise (vector &key (stride 1))
  "Test forward, inverse and backward FFT for a complex vector and return all three results."
  (let ((forward (forward-fourier-transform (copy vector) :stride stride)))
    (values forward
	    (inverse-fourier-transform (copy forward) :stride stride)
	    (backward-fourier-transform (copy forward) :stride stride))))

(defun test-fft-noise (element-type size &key (stride 1))
  "A test of real forward and complex forward, revese, and inverse FFT
   on random noise.  Returns the result of the DFT forward Fourier transformation
   and the forward FFT, which should be the same, and the original vector and the
   inverse FFT, which should also be the same.  In addition,
   the backward Fourier transform is returned for complex vectors, which
   should be the same as the last two."
  (let* ((random-vector (make-urand-vector element-type size :stride stride))
	 (dft-random-vector
	  (forward-discrete-fourier-transform random-vector :stride stride)))
    (if (subtypep element-type 'complex)
	(multiple-value-bind (forward inverse backward)
	    (test-complex-fft-noise random-vector :stride stride)
	  (values
	   dft-random-vector
	   forward
	   random-vector
	   inverse
	   (if (and (have-at-least-gsl-version '(1 12)) #+fsbv t #-fsbv nil)
	       (elt/ (copy backward) (size-vector-real backward :stride stride))
	       ;; Hack for old GSL version without complex vector math
	       (make-marray
		(element-type backward)
		:dimensions (dimensions backward)
		:initial-contents
		(map 'list
		     (lambda (x) (/ x (floor (size backward) stride)))
		     (copy backward 'array))))))
	(multiple-value-bind (forward inverse)
	    (test-real-fft-noise random-vector :stride stride)
	  (values dft-random-vector forward random-vector inverse)))))

;; (test-fft-noise 'double-float 10 :stride 1)
;; (test-fft-noise '(complex double-float) 10 :stride 1)

;;;;****************************************************************************
;;;; Constructing tests
;;;;****************************************************************************

#+(or)
(defun make-real-noise-fft-test (size stride)
  "Make a test of a random real-vector FFT." 
  (reset-urand)
  (let* ((randvec (make-urand-vector 'double-float size :stride stride))
	 (dft (forward-discrete-fourier-transform (copy randvec) :stride stride)))
    (list
     ;; Forward
     (make-test
      `(unpack (forward-fourier-transform ; computed FFT
		,(make-load-form (realpart-vector (copy randvec)))
		:stride ,stride)
	       :unpack-type 'complex :stride ,stride)
      dft)
     ;; Inverse
     (make-test
      `(unpack
	(vector/length
	 (forward-fourier-transform
	  ,(make-load-form (realpart-vector dft))
	  :half-complex t :stride ,stride)
	 ,stride)
	:stride ,stride)
      randvec))))

;; loop size from 1 to 99, stride from 1, 2, 3
#+(or)
(defun generate-fft-tests (&optional (size-max 99) (stride-max 3))
  (append
   '(lisp-unit:define-test fft-noise)
   (loop for size from 1 to size-max
      append
      (loop for stride from 1 to stride-max
	 append (make-real-noise-fft-test size stride)))))

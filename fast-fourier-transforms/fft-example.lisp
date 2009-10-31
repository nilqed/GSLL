;; Example FFT: transform a pulse
;; Sumant Oemrawsingh, Sat Oct 31 2009 - 15:31

;;; Here is an example program modelled after the example given in Section
;;; 15.3 of the GSL Manual, which computes the FFT of a short pulse. To make
;;; the resulting fourier transform real the pulse is defined for equal
;;; positive and negative times (-10 ... 10), where the negative times wrap
;;; around the end of the array.
;;; 
;;; The output array from the example in Section 15.3 of the GSL Manual can be
;;; reproduced with:
;;; (fft-pulse-test '(complex double-float) :dimension 128)
;;;
;;; This example program also yields the same output array as the example
;;; program in Section 15.4 of the GSL Manual:
;;; (fft-pulse-test '(complex double-float) :dimension 630)

(in-package :gsl)

(defun generate-pulse (element-type &key dimension)
  (assert (and (integerp dimension) (> dimension 20)))
  (let ((pulse (make-marray element-type :dimensions dimension))
        (init-value (coerce 1 element-type)))
    (setf (maref pulse 0) init-value)
    (loop for i from 1 to 10
          do (setf (maref pulse i) init-value
                   (maref pulse (- dimension i)) init-value))
    pulse))

(defun fft-pulse-test (element-type &key dimension)
  (let ((pulse (generate-pulse element-type :dimension dimension))
        (is-radix2 (= dimension (expt 2 (floor (log dimension 2))))))
    (cond
      ((subtypep element-type 'complex)
       (if is-radix2
         (fft-complex-radix2-forward pulse)
         (fft-complex-forward pulse)))
      ((subtypep element-type 'float)
       (if is-radix2
         (fft-real-radix2-transform pulse)
         (fft-real-transform pulse))))))

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

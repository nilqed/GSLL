;; A dirty hack to attempt to make a cleaner interface to the FFTs.
;; Sumant Oemrawsingh, Sat Oct 31 2009 - 23:48
;; Time-stamp: <2009-11-01 22:47:41EST fft-interface.lisp>

(in-package :gsl)

;; Generalised ways of making wavetables
(export 'make-fft-wavetable)
(defun make-fft-wavetable (element-type dimension &optional (half-complex nil))
  "Make a wavetable for an FFT of the given element type and length. T can be
  given as an optional third argument if the wavetable is meant for a Fourier
  transform on a half-complex vector."
  (cond ((eql element-type 'single-float)
         (if half-complex
           (make-fft-half-complex-wavetable-single-float dimension)
           (make-fft-real-wavetable-single-float dimension)))
        ((eql element-type 'double-float)
         (if half-complex
           (make-fft-real-wavetable-double-float dimension)
           (make-fft-half-complex-wavetable-double-float dimension)))
        ((equal element-type '(complex single-float))
         (make-fft-complex-wavetable-single-float dimension))
        ((equal element-type '(complex double-float))
         (make-fft-complex-wavetable-double-float dimension))))

;; Generalised ways of making workspaces
(export 'make-fft-workspace)
(defun make-fft-workspace (element-type dimension)
  "Make a wavetable for an FFT of the given element type and length."
  (cond ((eql element-type 'single-float)
         (make-fft-real-workspace-single-float dimension))
        ((eql element-type 'double-float)
         (make-fft-real-workspace-double-float dimension))
        ((equal element-type '(complex single-float))
         (make-fft-complex-workspace-single-float dimension))
        ((equal element-type '(complex double-float))
         (make-fft-complex-workspace-double-float dimension))))


;; An environment to allow more efficient FFTs of the same type and length
(export 'with-fourier-transform-environment)
(defmacro with-fourier-transform-environment
  ((wavetable workspace element-type dimension &optional (half-complex nil))
   &body body)
  "Create an environment where all FFTs will be performed on vectors of the
  same type and with the same length. This allows to calculculate and reuse the
  wavetable and workspace only once.

  The first and second arguments will be bound to the wavetable and workspace,
  the third argument is the element type of the vectors to be FFT'd and the
  fourth argument indicates the length of the vectors to which FFTs will be
  applied. Optionally, T can be given as a fifth argument if the element type
  of the vectors is real, but must be considered as half-complex."
  `(let ((,wavetable (make-fft-wavetable ,element-type ,dimension ,half-complex))
         (,workspace (make-fft-workspace ,element-type ,dimension)))
     ,@body))


;;; General FFT functions

(export 'backward-fourier-transform)
(defun backward-fourier-transform (vector &key wavetable workspace)
  "Generalised backward FFT. Perform a backward FFT on the given vector. If
  the length of the vector is not a power of 2, and the user has a suitable
  wavetable and/or workspace, these can be supplied as keyword arguments.
  
  Note that if the vector is of type real, it is assumed that it contains
  half-complex data."
  (let ((el-type (element-type vector))
        (len (size vector)))
    (if (power-of-2-p len)
      (cond ((subtypep el-type 'float)
             (fft-half-complex-radix2-backward vector))
            ((subtypep el-type 'complex)
             (fft-complex-radix2-backward vector)))
      (progn (unless wavetable
               (setf wavetable (make-fft-wavetable el-type len t)))
             (unless workspace
               (setf workspace (make-fft-workspace el-type len)))
             (cond ((subtypep el-type 'float)
                    (fft-half-complex-backward vector :wavetable wavetable
                                               :workspace workspace))
                   ((subtypep el-type 'complex)
                    (fft-complex-backward vector :wavetable wavetable
                                          :workspace workspace)))))))

(export 'inverse-fourier-transform)
(defun inverse-fourier-transform (vector &key workspace wavetable)
  "Generalised inverse FFT. Perform an inverse FFT on the given vector. If
  the length of the vector is not a power of 2, and the user has a suitable
  wavetable and/or workspace, these can be supplied as keyword arguments.
  
  Note that if the vector is of type real, it is assumed that it contains
  half-complex data."
  (let ((el-type (element-type vector))
        (len (size vector)))
    (if (power-of-2-p len)
      (cond ((subtypep el-type 'float)
             (fft-half-complex-radix2-inverse vector))
            ((subtypep el-type 'complex)
             (fft-complex-radix2-inverse vector)))
     (progn (unless wavetable
              (setf wavetable (make-fft-wavetable el-type len t)))
            (unless workspace
              (setf workspace (make-fft-workspace el-type len)))
             (cond ((subtypep el-type 'float)
                    (fft-half-complex-inverse vector :wavetable wavetable
                                              :workspace workspace))
                   ((subtypep el-type 'complex)
                    (fft-complex-inverse vector :wavetable wavetable
                                         :workspace workspace)))))))

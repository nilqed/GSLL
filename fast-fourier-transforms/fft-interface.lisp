;; A dirty hack to attempt to make a cleaner interface to the FFTs.
;; Sumant Oemrawsingh, Sat Oct 31 2009 - 23:48

(in-package :gsl)

;; Utility to determine if a vector can use a radix-2 transform.
(defun number-is-radix2 (num)
  (= 1 (logcount num)))

;; Generalised ways of making wavetables
(export 'make-fft-wavetable)
(defun make-fft-wavetable (element-type dimension &optional (half-complex nil))
  "Make a wavetable for an FFT of the given element type and length. T can be
  given as an optional third argument if the wavetable is meant for a Fourier
  transform on a half-complex vector."
  (cond ((eql element-type 'single-float)
         (if half-complex
           (make-fft-half-complex-wavetable-float dimension)
           (make-fft-real-wavetable-float dimension)))
        ((eql element-type 'double-float)
         (if half-complex
           (make-fft-real-wavetable dimension)
           (make-fft-half-complex-wavetable dimension)))
        ((equal element-type '(complex single-float))
         (make-fft-complex-wavetable-float dimension))
        ((equal element-type '(complex double-float))
         (make-fft-complex-wavetable dimension))))

;; Generalised ways of making workspaces
(export 'make-fft-workspace)
(defun make-fft-workspace (element-type dimension)
  "Make a wavetable for an FFT of the given element type and length."
  (cond ((eql element-type 'single-float)
         (make-fft-real-workspace-float dimension))
        ((eql element-type 'double-float)
         (make-fft-real-workspace dimension))
        ((equal element-type '(complex single-float))
         (make-fft-complex-workspace-float dimension))
        ((equal element-type '(complex double-float))
         (make-fft-complex-workspace dimension))))


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

;; Power of 2 functions
(defmfun forward-fourier-transform-radix2
    ((vector vector) &key (stride 1) (n (expt 2 (floor (log (size vector) 2)))))
  (double-float "gsl_fft_real_radix2_transform"
   single-float "gsl_fft_real_float_radix2_transform"
   complex-double-float "gsl_fft_complex_radix2_forward"
   complex-single-float "gsl_fft_complex_float_radix2_forward")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet))
  :definition :generic
  :element-types :float-complex
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Forward FFT for a real radix-2 vector")

(defmfun forward-fourier-transform-nonradix2
    ((vector vector) &key (stride 1) (n (size vector))
     (wavetable (eltcase single-float (make-fft-real-wavetable-float (size vector))
			 double-float (make-fft-real-wavetable (size vector))))
     (workspace (eltcase single-float (make-fft-real-workspace-float (size vector))
			 double-float (make-fft-real-workspace (size vector)))))
  (double-float "gsl_fft_real_transform"
   single-float "gsl_fft_real_float_transform"
   complex-double-float "gsl_fft_complex_forward"
   complex-single-float "gsl_fft_complex_float_forward")
  ("gsl_fft_real" :type "_transform")
  (((c-pointer vector) :pointer) (stride sizet) (n sizet)
   ((mpointer wavetable) :pointer) ((mpointer workspace) :pointer))
  :definition :generic
  :element-types :float
  :inputs (vector)
  :outputs (vector)
  :return (vector)
  :documentation
  "Forward FFT for a real vector")

(export 'forward-fourier-transform)
(defun forward-fourier-transform (vector &key wavetable workspace)
  "Generalised forward FFT. Perform a forward FFT on the given vector. If the
  length of the vector is not a power of 2, and the user has a suitable
  wavetable and/or workspace, these can be supplied as keyword arguments."
  (let ((el-type (element-type vector))
        (len (size vector)))
    (if (number-is-radix2 len)
	(forward-fourier-transform-radix2 vector)
	(progn (unless wavetable
		 (setf wavetable (make-fft-wavetable el-type len nil)))
	       (unless workspace
		 (setf workspace (make-fft-workspace el-type len)))
	       (forward-fourier-transform-radix2
		vector :wavetable wavetable :workspace workspace)))))

(export 'backward-fourier-transform)
(defun backward-fourier-transform (vector &key wavetable workspace)
  "Generalised backward FFT. Perform a backward FFT on the given vector. If
  the length of the vector is not a power of 2, and the user has a suitable
  wavetable and/or workspace, these can be supplied as keyword arguments.
  
  Note that if the vector is of type real, it is assumed that it contains
  half-complex data."
  (let ((el-type (element-type vector))
        (len (size vector)))
    (if (number-is-radix2 len)
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
    (if (number-is-radix2 len)
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

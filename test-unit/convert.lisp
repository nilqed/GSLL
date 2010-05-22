;; Convert the GSL tests
;; Liam Healy 2010-05-22 13:03:53EDT convert.lisp
;; Time-stamp: <2010-05-22 18:26:00EDT convert.lisp>

;;; This file is not normally loaded; it is only used to convert the
;;; GSL tests in C to CL tests.  It requires cl-ppcre, lisp-util, and iterate.
;;; (convert-tests-in-file "/home/liam/mathematics/gsl/cdf/test.c")

;;; Things it won't do yet:
;;; Will not convert fixnum to double.
;;; Will not recognize infinities and substitute the appropriate assert-posinf, assert-neginf test. 
;;; Will not parse/convert C math specified as an argument, e.g. 100 * TEST_TOL6.

(in-package :gsl)

;;; (convert-gsl-test "(gsl_cdf_tdist_P, (0.001, 1.0), 5.00318309780080559e-1, TEST_TOL6)")
;;; (GSL_CDF_TDIST_P (0.001d0 1.0d0) 0.5003183097800805d0 TEST_TOL6)
(defun convert-gsl-test (string)
  "Read the GSL test as a string and generate a test form.
   The first character of the string should be 
   open paren, and the last the close paren."
  (cons 'assert-to-tolerance
	(let ((ppcre-convert
	       (read-from-string
		(substitute
		 #\space
		 #\,			; Get rid of commas
		 (cl-ppcre:regex-replace ; Replace function name
		  "\\((\\w*)\\W*\\((.*\\))"
		  (cl-ppcre:regex-replace-all ; Floats without exponents become double-floats
		   "(\\.\\d*)(,|\\))"
		   (cl-ppcre:regex-replace-all ; Floats with "e" exponent become double-floats
		    "(\\d|\\.)e(\\d|\\+|\\-)"
		    (cl-ppcre:regex-replace ; Replace tolerance
		     "TEST_(\\w*)"
		     string
		     "+TEST-\\{1}+")
		    "\\{1}d\\{2}")
		   "\\{1}d0\\{2}")
		  (lambda (match &rest registers)
		    (declare (ignore match))
		    (format
		     nil
		     "((~a ~a"
		     (symbol-name (gsl-lookup (first registers)))
		     (second registers)))
		  :simple-calls t)))))
	  (if (and (numberp (lu:last1 ppcre-convert)) (zerop (lu:last1 ppcre-convert)))
	      (append (butlast ppcre-convert) (list '+dbl-epsilon+))
	      ppcre-convert))))

(defun GSL-test-form (string)
  "Replace the full C form with the TEST macro by just the argument."
  (cl-ppcre:regex-replace
   "\\s*TEST\\s*\\((.*)\\);"
   (remove #\newline string)
   "(\\{1})"))description

(defun file-to-string (file)
  "Find the tests in the file and convert them to lisp."
  (let ((stream (make-string-output-stream)))
    (iter:iter (iter:for ch iter::in-file file :using #'read-char)
	       (princ ch stream))
    (get-output-stream-string stream)))

(defun find-tests-in-file (file)
  (let ((filestring (file-to-string file)))
    (cl-ppcre:all-matches-as-strings " TEST .*;" filestring)))

(defun convert-tests-in-file (file)
  "Convert all the tests in the file to CL."
  ;; Only works for cdf/test.c at present.
  (let ((tests (find-tests-in-file file)))
    (mapcar (lambda (test) (convert-gsl-test (gsl-test-form test))) tests)))

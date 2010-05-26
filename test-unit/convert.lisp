;; Convert the GSL tests
;; Liam Healy 2010-05-22 13:03:53EDT convert.lisp
;; Time-stamp: <2010-05-25 23:15:21EDT convert.lisp>

;;; This file is not normally loaded; it is only used to convert the
;;; GSL tests in C to CL tests.  It requires cl-ppcre, lisp-util, and iterate.
;;; (convert-tests-in-file "/home/liam/mathematics/gsl/cdf/test.c")

;;; Things it won't do yet:
;;; Will not convert fixnum to double.
;;; Will not recognize infinities and substitute the appropriate assert-posinf, assert-neginf test. 
;;; Will not parse/convert C math specified as an argument, e.g. 100 * TEST_TOL6.

;;; In slime, set pretty margin wide:
;;; (setf *print-right-margin* 100)

(in-package :gsl)

;;; (princ (convert-gsl-test "(gsl_cdf_tdist_P, (0.001, 1.0), 5.00318309780080559e-1, TEST_TOL6)"))
;;; (ASSERT-TO-TOLERANCE (TDIST-P 0.001d0 1.0d0) 5.00318309780080559d-1 +TEST-TOL6+)
;;; (convert-gsl-test "(s,  gsl_sf_lngamma_e, (-0.1, &r), 2.368961332728788655 , TEST_TOL0, GSL_SUCCESS)" *sf-select*)

(defun convert-gsl-test (string &optional select-args)
  "Read the GSL test as a string and generate a test form.
   The first character of the string should be 
   open paren, and the last the close paren."
  (cons 'assert-to-tolerance
	(let ((ppcre-convert
	       (select-args
		(read-from-string
		 (substitute
		  #\space #\,		; Get rid of commas
		  (remove-return-value
		   (replace-function-name (translate-c-numbers (replace-tolerance string))))))
		select-args)))
	  (if (and (numberp (lu:last1 ppcre-convert)) (zerop (lu:last1 ppcre-convert)))
	      (append (butlast ppcre-convert) (list '+dbl-epsilon+))
	      ppcre-convert))))

(defun select-args (list discard)
  (if discard
      (loop for d in discard
	 for i from 0
	 when d collect (nth i list))
      list))

(defun replace-function-name (string)
  "Replace the names of GSL functions with their GSLL equivalents."
  (cl-ppcre:regex-replace
   ;;"\\((\\w*)\\W*\\((.*\\))"
    "(gsl_\\w*)\\W*\\((.*)\\)"
   string
   (lambda (match &rest registers)
     (declare (ignore match))
     (format
      nil
      "(~a ~a)"
      (symbol-name (gsl-lookup (first registers)))
      (second registers)))
   :simple-calls t))

(defun translate-c-numbers (string)
  "Translate the literal numbers in the string to CL double-float."
  (cl-ppcre:regex-replace-all ; Floats without exponents become double-floats
   ;;"(\\.\\d*)\\s*(,|\\))"
   "(\\+|\\-*\\d*\\.\\d*)($|[^0-9de.])"
   (cl-ppcre:regex-replace-all ; Floats with "e" exponent become double-floats
    "(\\+|\\-*\\d*\\.*\\d*)e(\\d*(\\+|\\-)*\\d*)"
    string
    "\"\\{1}d\\{2}\"")
   "\"\\{1}d0\"\\{2}"))

#|
;; http://www.regular-expressions.info/floatingpoint.html
;; need to exempt d from the first case 
(cl-ppcre:regex-replace-all "([-+]?[0-9]*\\.?[0-9]+)" "-4.2134" "\\{1}d0")
(cl-ppcre:regex-replace-all "([-+]?[0-9]*\\.?[0-9]+)[eE](([-+]?[0-9]+)?)"
			    "1.213e4"
			    "\\{1}d\\{2}")
|#

(defun replace-tolerance (string)
  (cl-ppcre:regex-replace "TEST_(\\w*)" string "+TEST-\\{1}+"))

(defun remove-return-value (string)
  (cl-ppcre:regex-replace "\&r" string ""))

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
    (cl-ppcre:all-matches-as-strings " TEST\s?\\(.*;" filestring)))

(defun convert-tests-in-file (file select-args)
  "Convert all the tests in the file to CL."
  ;; Only works for cdf/test.c at present.
  (let ((tests (find-tests-in-file file)))
    (mapcar (lambda (test) (convert-gsl-test (gsl-test-form test))) tests select-args)))

(defvar *sf-select* '(nil t t t nil)
  "The select arg list for special function tests.")

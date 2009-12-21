;; Macros to interface GSL functions, including definitions necessary for defmfun.
;; Liam Healy 
;; Time-stamp: <2009-12-20 23:03:02EST interface.lisp>

(in-package :gsl)

;;;;****************************************************************************
;;;; Lookup table to find CL functions from C function names
;;;;****************************************************************************

(defparameter *gsl-symbol-equivalence*
  (make-hash-table :test 'equal :size 2000))

(defun map-name (cl-name gsl-name)
  ;; Trust here that the library does not have two symbols that differ
  ;; only by the case of one or more letters.
  (setf (gethash (string-downcase gsl-name) *gsl-symbol-equivalence*) cl-name))

(export '(gsl-lookup))
(defun gsl-lookup (string)
  "Find the GSLL (Lisp) equivalent of the GSL symbol."
  (gethash (string-downcase string) *gsl-symbol-equivalence*))

;;;;****************************************************************************
;;;; Declare foreign objects
;;;;****************************************************************************

(defmacro scref (size &optional (index 0))
  "Reference C size(s)."
  `(cffi:mem-aref ,size 'sizet ,index))

(defun wfo-declare (d cbinfo)
  `(,(c-array:st-symbol d)
     ,@(if (eq (c-array:st-symbol d)
	       (parse-callback-static cbinfo 'foreign-argument))
	   `(',(parse-callback-static cbinfo 'callback-fnstruct))
	   `(',(c-array:st-actual-type d)))))

;;;;****************************************************************************
;;;; Checking results from GSL functions
;;;;****************************************************************************

(defun success-failure (value)
  "If status is either +success+ or +continue+, return T;
   otherwise, return NIL."
  (member value (list +success+ +continue+)))

(defun success-continue (value)
  "If status is +success+, return T, otherwise return NIL."
  (eql value +success+))

(defun check-gsl-status (status-code context)
  "Check the return status code from a GSL function and signal a warning
   if it is not :SUCCESS."
  (unless (success-failure status-code)
    (signal-gsl-warning status-code (format nil "in ~a" context))))

(defun check-null-pointer (pointer error-code reason)
  (when (cffi:null-pointer-p pointer)
    (signal-gsl-error error-code reason)))

;;;;****************************************************************************
;;;; Argument check
;;;;****************************************************************************

(defun cl-symbols (arglist)
  "The symbols in the arglist."
  (mapcar (lambda (s) (if (listp s) (first s) s)) arglist))

;;; (cl-argument-types '(a b) '((a :double) (b :int32)))
(defun cl-argument-types (cl-arguments c-arguments-types)
  "Create CL argument and types from the C arguments."
  (loop for sd in c-arguments-types
	for cl-type = (c-array:cffi-cl (c-array:st-type sd))
	append
	(when (and cl-type (member (c-array:st-symbol sd) (cl-symbols cl-arguments)))
	  (list (list (c-array:st-symbol sd) cl-type)))))

(defun declaration-form (cl-argument-types &optional ignores specials)
  (cons 'declare
	(append
	 (mapcar (lambda (v) (cons 'type (reverse v)))
		 cl-argument-types)
	 (when ignores (list (cons 'ignore ignores)))
	 (when specials (list (cons 'special specials))))))

;;;;****************************************************************************
;;;; Returns
;;;;****************************************************************************

(defvar *special-c-return*
  '(:error-code :number-of-answers :success-failure :success-continue
    :true-false :enumerate))

;;;;****************************************************************************
;;;; Variables in library
;;;;****************************************************************************

(defmacro defmpar
    (cl-symbol gsl-symbol documentation
     &key (c-type :pointer) (read-only t) gsl-version)
  "Define a library variable pointer."
  (if (have-at-least-gsl-version gsl-version)
      `(progn
	 (cffi:defcvar (,gsl-symbol ,cl-symbol :read-only ,read-only) ,c-type
	   ,documentation)
	 (map-name ',cl-symbol ,gsl-symbol)
	 (export ',cl-symbol))
      ;; Insufficient version number handled by binding a special of
      ;; the same name to the condition.  It would be nice to signal
      ;; the error, but it least this will provide some information.
      `(define-symbol-macro ,cl-symbol
	 (error
	  'obsolete-gsl-version :name ',cl-symbol :gsl-name
	  ,gsl-symbol :gsl-version ',gsl-version ))))

;;;;****************************************************************************
;;;; GSL library version
;;;;****************************************************************************

(cffi:defcvar ("gsl_version" *gsl-version* :read-only t) :string
          "The version of the GSL library being used.")
(map-name '*gsl-version* "gsl_version")
(export '*gsl-version*)

(defun have-at-least-gsl-version (major-minor)
  "The GSL version currently running is at least the specified
  major/minor version."
  (or (null major-minor)
      (let* ((sep-pos (position #\. *gsl-version*))
	     (my-major
	      (read-from-string *gsl-version* nil nil :end sep-pos))
	     (my-minor
	      (read-from-string *gsl-version* nil nil :start (1+ sep-pos))))
	(and (>= my-major (first major-minor))
	     (>= my-minor (second major-minor))))))

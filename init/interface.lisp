;; Macros to interface GSL functions.
;; Liam Healy 
;; Time-stamp: <2008-04-13 19:31:37EDT interface.lisp>
;; $Id$

(in-package :gsl)

(export '(gsl-lookup *make-sequence-type*))

;;;;****************************************************************************
;;;; Lookup table to find CL functions from C function names
;;;;****************************************************************************

(defparameter *gsl-symbol-equivalence*
  (make-hash-table :test 'equal :size 1500))

(defun map-name (cl-name gsl-name)
  ;; Trust here that the library does not have two symbols that differ
  ;; only by the case of one or more letters.
  (setf (gethash (string-downcase gsl-name) *gsl-symbol-equivalence*) cl-name))

(defun gsl-lookup (string)
  "Find the GSLL (Lisp) equivalent of the GSL symbol."
  (gethash (string-downcase string) *gsl-symbol-equivalence*))

;;;;****************************************************************************
;;;; Symbol-type declaration
;;;;****************************************************************************

;;; An "st" or symbol-type is a list (symbol type) where
;;; type could be (element-type array-dim).  These are examples of lists
;;; of sts: 
 ;; ((#:RET3500 SF-RESULT))
 ;; ((#:RET3501 (:DOUBLE (- NMAX NMIN)))) 
 ;; ((#:RET3502 (:DOUBLE (1+ KMAX))) (#:RET3503 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3504 (:DOUBLE (1+ KMAX))) (#:RET3505 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3506 :DOUBLE) (#:RET3507 :DOUBLE))

(defun make-st (symbol type)
  (list symbol type))

(defun st-symbol (decl)
  (first decl))

(defun st-type (decl)
  (second decl))

(defun st-arrayp (decl)
  (listp (st-type decl)))

(defun st-array-pointer-last-p (decl)
  (listp (st-type decl)))

(defun st-eltype (decl)
  (first (st-type decl)))

(defun st-dim (decl)
  (second (st-type decl)))

(defun st-pointer-last-p (decl)
  (third (st-type decl)))

(defun wfo-declare (d)
  `(,(st-symbol d)
    ,@(if (st-arrayp d)
	  `(',(st-eltype d) ,(st-dim d))
	  `(',(st-type d)))))

;;;;****************************************************************************
;;;; Checking results from GSL functions
;;;;****************************************************************************

(defun check-gsl-status (status-code context)
  "Check the return status code from a GSL function and signal a warning
   if it is not :SUCCESS."
  (unless (eql status-code success)
    (signal-gsl-warning status-code (format nil "in ~a" context))))

(defun check-null-pointer (pointer error-code reason)
  (when (cffi:null-pointer-p pointer)
    (signal-gsl-error error-code reason)))

(defun success-failure (value)
  "If status indicates failure, return NIL, othewise return T."
  ;;(not (eql value (cffi:foreign-enum-value 'gsl-errorno :FAILURE)))
  ;; More general, to allow :CONTINUE
  (not (minusp value)))

(defun success-continue (value)
  "If status indicates success, return T, othewise return NIL."
  (eql value success))

;;;;****************************************************************************
;;;; Argument check
;;;;****************************************************************************

;;; (cl-argument-types '(a b) '((a :double) (b :int32)))
(defun cl-argument-types (cl-arguments c-arguments-types)
  "Create CL argument and types from the C arguments."
  (loop for sd in c-arguments-types
	for cl-type = (cffi-cl (second sd))
	append
	(when (and cl-type (member (first sd) cl-arguments))
	  (list (list (first sd) cl-type)))))

(defun declaration-form (cl-argument-types)
  (cons 'declare
	(mapcar (lambda (v) (cons 'type (reverse v))) cl-argument-types)))

;;;;****************************************************************************
;;;; Macro defmfun
;;;;****************************************************************************

(defvar *special-c-return*
  '(:error-code :number-of-answers :success-failure :success-continue
    :true-false :enumerate))

;;; arglist    List of CL arguments.
;;; gsl-name   Name of the GSL C function, as a quoted string.
;;; c-arguments List of (symbol c-type). Anything not in arglist will be allocated.
;;; c-return,  a symbol naming a type, (e.g. :int, :double, :void),
;;;            or a list of (symbol type) to name the value,
;;;            or :error-code, :number-of-answers, :success-failure,
;;;            :true-false, :enumerate.  If :enumeration is given,
;;;            the :enumeration keyword argument will supply the name
;;;            of the enumeration.
;;; return, a list of quantities to return.
;;;            May be or include :c-return to include the c-return value
;;;            or its derivatives.
;;;            Default are allocated quantities in c-arguments, or :c-return if none.
;;; type       :function or :method
;;; index      Name under which this function should be cross-referenced; t
;;;            to use name, nil to not index.
;;; export     Whether to export the symbol.
;;; null-pointer-info Return value if C function returns a null pointer.
;;; documentation
;;; inputs     Arrays whose values are used by the GSL function
;;; outputs    Arrays that are written to in the GSL function
;;; after        After method.
;;; enumeration  The name of the enumeration return.
;;; global     Bind variable(s) in a let* enclosing the whole body.
(defmacro defmfun
    (&whole args
     name arglist gsl-name c-arguments
     &key (c-return :error-code)
     (return nil return-supplied-p)
     (type :function) (index t) (export (not (eq type :method)))
     null-pointer-info documentation inputs outputs after enumeration
     global category (cl-types nil cl-types-supplied-p))
  (declare (ignorable inputs outputs))
  (if category
      (defmfun-all-in-defmfun (rest args) category cl-types cl-types-supplied-p)
      (let* ((cargs (substitute '(mode sf-mode) :mode c-arguments))
	     (carg-symbs
	      (remove-if-not #'symbolp
			     (mapcar #'st-symbol (remove :mode c-arguments))))
	     (clargs
	      (or arglist carg-symbs))
	     (arglist-symbs
	      (when arglist		; can be method, so get symbol
		(mapcar (lambda (x) (if (listp x) (first x) x)) arglist)))
	     (cret-type (if (member c-return *special-c-return*)
			    :int
			    (if (listp c-return) (st-type c-return) c-return)))
	     (cret-name
	      (if (listp c-return) (st-symbol c-return) (make-symbol "CRETURN")))
	     (allocated		     ; Foreign objects to be allocated
	      (remove-if
	       (lambda (s)
		 (or (member s arglist-symbs) (member s global :key #'first)))
	       carg-symbs))
	     (allocated-decl
	      (mapcar
	       (lambda (s) (find s cargs :key #'st-symbol))
	       allocated))
	     (clret (or			; better as a symbol macro
		     (substitute cret-name :c-return return)
		     (mapcan #'cl-convert-form allocated-decl)
		     outputs
		     (unless (eq c-return :void)
		       (list cret-name))))
	     (clargs-types (cl-argument-types clargs cargs)))
	`(progn
	  (,(if (eq type :function) 'defun 'defmethod)
	   ,name
	   ,(let ((noaux
		   (if (member :mode c-arguments)
		       `(,@clargs &optional (mode :double-prec))
		       `(,@clargs))))
		 (if global
		     (append noaux (cons '&aux global))
		     noaux))
	   ,(declaration-form clargs-types)
	   ,@(when documentation (list documentation))
	   (,@(if allocated
		  `(cffi:with-foreign-objects
		    ,(mapcar #'wfo-declare allocated-decl))
		  '(let ()))
	    #-native ,@(mapcar (lambda (v) `(copy-cl-to-c ,v)) inputs)
	    (let ((,cret-name
		   (cffi:foreign-funcall
		    ,gsl-name
		    ,@(mapcan
		       (lambda (arg)
			 (list (if (member (st-symbol arg) allocated)
				   :pointer
				   (st-type arg))
			       (st-symbol arg)))
		       cargs)
		    ,cret-type)))
	      ,@(case c-return
		      (:void `((declare (ignore ,cret-name))))
		      (:error-code	; fill in arguments
		       `((check-gsl-status ,cret-name ',name))))
	      #-native
	      ,@(when outputs `(,(mapcar (lambda (x) `(setf (cl-invalid ,x) t))) outputs))
	      ,@(when (or null-pointer-info (eq c-return :pointer))
		      `((check-null-pointer ,cret-name
			 ,@(or null-pointer-info
			       '(:ENOMEM "No memory allocated")))))
	      ,@after
	      (values
	       ,@(case c-return
		       (:number-of-answers
			(mapcan
			 (lambda (vbl seq)
			   `((when (> ,cret-name ,seq) ,vbl)))
			 clret
			 (loop for i below (length clret) collect i)))
		       (:success-failure
			(if (equal clret outputs)
			    ;; success-failure more important than passed-in
			    `((success-failure ,cret-name))
			    (remove cret-name ; don't return c-return itself
				    `(,@clret (success-failure ,cret-name)))))
		       (:success-continue
			(if (equal clret outputs)
			    ;; success-failure more important than passed-in
			    `((success-continue ,cret-name))
			    (remove cret-name ; don't return c-return itself
				    `(,@clret (success-continue ,cret-name)))))
		       (:true-false
			`((not (zerop ,cret-name))))
		       (:enumerate
			`((cffi:foreign-enum-keyword ',enumeration ,cret-name)))
		       (t (unless
			      (or
			       (and (eq c-return :error-code)
				    (not allocated)
				    (not return-supplied-p))
			       (and (null return) return-supplied-p))
			    clret)))))))
	  ,@(when index `((map-name ',(if (eql index t) name index) ,gsl-name)))
	  ,@(when export `((export ',name)))))))

(defmacro defun-optionals
    (name arglist no-optional optionals &optional documentation)
  "Define a function with and without optional arguments."
  (let* ((optpos (position '&optional arglist))
	 (mandatory-arglist (subseq arglist 0 optpos))
	 (optional-arglist (subseq arglist (1+ optpos))))
    `(defun ,name ,arglist
       ,documentation
       (if ,(first optional-arglist)
	   (,(intern
	      (concatenate 'string (string name) (string optionals))
	      :gsl)
	     ,@mandatory-arglist ,@optional-arglist)
	   (,(intern
	      (concatenate 'string (string name)
			   (if no-optional (string no-optional) ""))
	      :gsl)
	     ,@mandatory-arglist)))))

;;;;****************************************************************************
;;;; Variables in library
;;;;****************************************************************************

(defmacro defmpar (cl-symbol gsl-symbol documentation)
  "Define a library variable pointer."
  `(progn
    (cffi:defcvar (,gsl-symbol ,cl-symbol :read-only t) :pointer
      ,documentation)
    (map-name ',cl-symbol ,gsl-symbol)
    (export ',cl-symbol)))

;;;;****************************************************************************
;;;; GSL library version
;;;;****************************************************************************

(cffi:defcvar ("gsl_version" *gsl-version* :read-only t) :string
          "The version of the GSL library being used.")
(map-name '*gsl-version* "gsl_version")
(export '*gsl-version*)

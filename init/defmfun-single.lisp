;; Helpers that define a single GSL function interface
;; Liam Healy 2009-01-07 22:02:20EST defmfun-single.lisp
;; Time-stamp: <2009-01-31 21:19:28EST defmfun-single.lisp>
;; $Id: $

(in-package :gsl)

(defun defgeneric-method-p (name)
  "When defining :method in a defgeneric, (nil foo) is used for
   the name, and foo will be returned from this function."
  (if (and (listp name) (null (first name)))
      (second name)))

(defun complex-scalars (cl-arguments c-arguments-types)
  (loop for sd in c-arguments-types
	for cl-type = (cffi-cl (st-type sd))
	append
	(when (and cl-type (subtypep cl-type 'complex)
		   (member (st-symbol sd)
			   (arglist-plain-and-categories cl-arguments)))
	  (list (list (st-symbol sd)
		      (gensym (string (st-symbol sd)))
		      (st-type sd))))))

(defun stupid-code-walk-find-variables (sexp)
  "This will work with the simplest s-expression forms only to find
   variables used."
  (labels ((scwfv (sexp)
	     (if (atom sexp)
		 (when (and (symbolp sexp)
			    (not (eql (symbol-package sexp)
				      (find-package :keyword))))
		   (list sexp))
		 (unless (member (first sexp) '(quote))
		   (mapcan #'scwfv (rest sexp))))))
    (remove nil (remove-duplicates (scwfv sexp)))))

(defun variables-used-in-c-arguments (c-arguments)
  "Find the arguments passed to the C function.  This is a poor
  quality code walker, but is sufficient for actual usage of defmfun."
  (remove-duplicates
   (mapcan (lambda (carg)
	     (stupid-code-walk-find-variables (st-symbol carg)))
	   c-arguments)
   :from-end t))

#+native
(defun native-pointer (array-symbols body)
  "Wrap the body with a form that obtains the native pointer
   and protects it during execution of the body."
  (if array-symbols
      ;; http://www.sbcl.org/manual/Calling-Lisp-From-C.html
      (native-pointer-protect array-symbols body)
      body))

(define-condition obsolete-gsl-version (error)
  ((name :initarg :name :reader name)
   (gsl-name :initarg :gsl-name :reader gsl-name)
   (gsl-version :initarg :gsl-version :reader gsl-version))
  (:report
   (lambda (condition stream)
     (apply 'format stream
	    "Function ~a (~a) is not available in the ~
               ~%currently loaded releases ~a of GSL; it was introduced in release ~d.~d."
	    (name condition)
	    (gsl-name condition)
	    *gsl-version*
	    (gsl-version condition))))
  (:documentation
   "An error indicating that the currently loaded version of the GSL libary
    does not have the function defined."))

(defun complete-definition
    (definition name arglist gsl-name c-arguments key-args
     &optional
     (body-maker 'body-no-optional-arg)
     (mapdown (eq body-maker 'body-optional-arg)))
  "A complete definition form, starting with defun, :method, or defmethod."
  (destructuring-bind
	(&key documentation before after
	      qualifier gsl-version &allow-other-keys)
      key-args
    (if (have-at-least-gsl-version gsl-version)
	`(,definition
	     ,@(when (and name (not (defgeneric-method-p name)))
		     (list name))
	     ,@(when qualifier (list qualifier))
	   ,arglist
	   ,(declaration-form
	     (cl-argument-types arglist c-arguments)
	     (set-difference	       ; find all the unused variables
	      (arglist-plain-and-categories arglist nil)
	      (union
	       (if mapdown
		   (apply 'union
			  (mapcar 'variables-used-in-c-arguments c-arguments))
		   (variables-used-in-c-arguments c-arguments))
	       ;; Forms in :before, :after are checked for used variables
	       (stupid-code-walk-find-variables
		(cons
		 'values
		 (append before after
			 (let ((auxstart (position '&aux arglist)))
			   ;; &aux bindings are checked
			   (when auxstart
			     (apply
			      'append
			      (mapcar 'rest (subseq arglist (1+ auxstart))))))))))))
	   ,@(when documentation (list documentation))
	   ,(funcall body-maker name arglist gsl-name c-arguments key-args))
        `(,definition
	     ,@(when (and name (not (defgeneric-method-p name)))
		     (list name))
	     ,@(when qualifier (list qualifier))
	   (&rest args)
	   (declare (ignore args))
	   (error 'obsolete-gsl-version
		  :name ',name :gsl-name ,gsl-name :gsl-version ',gsl-version)))))

(defun wrap-letlike (when binding wrapping body)
  (if when
      `(,wrapping ,binding ,@body)
      (if (eql (length body) 1)
	  (first body)
	  `(progn ,@body))))

(defun body-no-optional-arg (name arglist gsl-name c-arguments key-args)
  "Wrap necessary array-handling forms around the expanded unswitched
  body form."
  #-native
  (destructuring-bind (&key inputs &allow-other-keys) key-args
    `(progn
       ,@(mapcar (lambda (v) `(copy-cl-to-c ,v))
		 (intersection
		  inputs (arglist-plain-and-categories arglist nil)))
       ,(body-expand name arglist gsl-name c-arguments key-args)))
  #+native
  (destructuring-bind (&key inputs outputs &allow-other-keys) key-args
    (native-pointer
     (intersection
      (union inputs outputs) (arglist-plain-and-categories arglist nil))
     (body-expand name arglist gsl-name c-arguments key-args))))

(defun body-expand (name arglist gsl-name c-arguments key-args)
  "Expand the body (computational part) of the defmfun."
  (with-defmfun-key-args key-args
    (let* ((cret-type (if (member c-return *special-c-return*)
			  :int
			  (if (listp c-return) (st-type c-return) c-return)))
	   (cret-name
	    (if (listp c-return) (st-symbol c-return) (make-symbol "CRETURN")))
	   (complex-args (complex-scalars arglist c-arguments))
	   (allocated		     ; Foreign objects to be allocated
	    (remove-if
	     (lambda (s)
	       (member s (arglist-plain-and-categories arglist nil)))
	     (variables-used-in-c-arguments c-arguments)))
	   (allocated-decl
	    (mapcar
	     (lambda (s)
	       (or (find s c-arguments :key #'st-symbol)
		   ;; Catch programming errors, usually typos
		   (error "Could not find ~a among the arguments" s)))
	     allocated))
	   (clret (or			; better as a symbol macro
		   (substitute cret-name :c-return return)
		   (mapcan #'cl-convert-form allocated-decl)
		   outputs
		   (unless (eq c-return :void)
		     (list cret-name)))))
      (if (and complex-args (not *pass-complex-scalar-as-two-reals*))
	  '(error 'pass-complex-by-value) ; arglist should be declared ignore
	  (wrap-letlike
	   allocated-decl
	   (mapcar #'wfo-declare allocated-decl)
	   'cffi:with-foreign-objects
	   `(,@before
	     (let ((,cret-name
		    (cffi:foreign-funcall
		     ,gsl-name
		     ,@(mapcan
			(lambda (arg)
			  (let ((cfind	; variable is complex
				 (find (st-symbol arg) complex-args :key 'first)))
			    (if cfind	; make two successive scalars
				(passing-complex-by-value cfind)
				;; otherwise use without conversion
				(list (if (member (st-symbol arg) allocated)
					  :pointer
					  (st-type arg))
				      (st-symbol arg)))))
			c-arguments)
		     ,cret-type)))
	       ,@(case c-return
		       (:void `((declare (ignore ,cret-name))))
		       (:error-code	; fill in arguments
			`((check-gsl-status ,cret-name
					    ',(or (defgeneric-method-p name) name)))))
	       #-native
	       ,@(when outputs
		       (mapcar
			(lambda (x) `(setf (cl-invalid ,x) t (c-invalid ,x) nil))
			outputs))
	       ,@(when (eq cret-type :pointer)
		       `((check-null-pointer
			  ,cret-name
			  ,@'('memory-allocation-failure "No memory allocated."))))
	       ,@after
	       (values
		,@(defmfun-return
		   c-return cret-name clret allocated return return-supplied-p
		   enumeration outputs)))))))))

(defun defmfun-return
    (c-return cret-name clret allocated return return-supplied-p enumeration outputs)
  "Generate the return computation expression for defmfun."
  (case c-return
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
	 (remove cret-name		; don't return c-return itself
		 `(,@clret (success-failure ,cret-name)))))
    (:success-continue
     (if (equal clret outputs)
	 ;; success-failure more important than passed-in
	 `((success-continue ,cret-name))
	 (remove cret-name		; don't return c-return itself
		 `(,@clret (success-continue ,cret-name)))))
    (:true-false
     `((not (zerop ,cret-name))))
    (:enumerate
     `((cffi:foreign-enum-keyword ',enumeration ,cret-name)))
    (t (unless
	   (or
	    (and (eq c-return :error-code)
		 (not outputs)
		 (not allocated)
		 (not return-supplied-p))
	    (and (null return) return-supplied-p))
	 clret))))
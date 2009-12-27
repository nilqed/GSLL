;; Expand the body of a defmfun
;; Liam Healy 2009-04-13 22:07:13EDT body-expand.lisp
;; Time-stamp: <2009-12-27 09:50:35EST body-expand.lisp>
;;
;; Copyright 2009 Liam M. Healy
;; Distributed under the terms of the GNU General Public License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :gsl)

(defun creturn-st (c-return)
  "The symbol-type of the return from the C function."
  (c-array:make-st
   (if (listp c-return)
       (c-array:st-symbol c-return)
       (make-symbol "CRETURN"))
   (if (member c-return *special-c-return*)
       :int
       (if (listp c-return) (c-array:st-type c-return) c-return))))

#+fsbv
(defun variables-passed-by-value (c-arguments-st)
  "Create a list of (symbol newsymbol-st) for each structure
  variable in the C argument list."
  (loop for sd in c-arguments-st
     collect
     (when (fsbv:defined-type-p (c-array:st-type sd))
       (list (c-array:st-symbol sd)
	     (c-array:make-st
	      (make-symbol (string (c-array:st-symbol sd)))
	      (c-array:st-type sd))))))

;;; This function should never be called even when FSBV is absent,
;;; because the potential callers should all have #-fsbv
;;; conditionalization.  It is here just so that body-expand can
;;; compile when FSBV is absent.
#-fsbv
(defmacro no-fsbv-error (function-name &rest args)
  (declare (ignore args))
  `(error
    "System FSBV is not present, so function ~a cannot be used."
    ,function-name))

#+fsbv
(defun make-defcfun-for-fsbv (gsl-name ff-args)
  "Make a fsbv:defcfun form so that function will be prepped."
  (multiple-value-bind (args return-type)
      (fsbv:defcfun-args-from-ff-args ff-args)
    (let ((gsl-name-symbol (make-symbol gsl-name))
	  (symbargs
	   (mapcar (lambda (st) (c-array:make-st (gensym "ARG") (c-array:st-type st)))
		   args)))
      (values
       `(fsbv:defcfun (,gsl-name-symbol gsl-name) ,return-type
	  "Function definition generated for FSBV prepping; will actually
        be called by fsbv:foreign-funcall"
	  ,@symbargs)
       gsl-name-symbol))))

(defun ffexpand (pass-by-value-p gsl-name args)
  "Expand the foreign funcall."
  (if pass-by-value-p
      #+fsbv
      (multiple-value-bind (form symbol)
	  (make-defcfun-for-fsbv gsl-name args)
	(declare (special fsbv-functions))
	(push form fsbv-functions)
	`(fsbv:foreign-funcall ,symbol ,@args))
      #-fsbv
      `(no-fsbv-error no-fsbv-error ,@args)
      `(cffi:foreign-funcall ,gsl-name ,@args)))

(defun cl-convert-form (decl)
  "Generate a form that calls the appropriate converter from C/GSL to CL."
  (case (c-array:st-actual-type decl)
    (sf-result 
     `((val ,(c-array:st-symbol decl))
       (err ,(c-array:st-symbol decl))))
    (sf-result-e10
     `((val ,(c-array:st-symbol decl) 'sf-result-e10)
       (e10 ,(c-array:st-symbol decl))
       (err ,(c-array:st-symbol decl) 'sf-result-e10)))
    (c-array:complex-double-c
     `((c-array:complex-to-cl ,(c-array:st-symbol decl) 0 'c-array:complex-double-c)))
    (c-array:complex-float-c
     `((c-array:complex-to-cl ,(c-array:st-symbol decl) 0 'c-array:complex-float-c)))
    (t `((cffi:mem-aref ,(c-array:st-symbol decl) ',(c-array:st-actual-type decl))))))

(defun body-expand (name arglist gsl-name c-arguments key-args)
  "Expand the body (computational part) of the defmfun."
  (with-defmfun-key-args key-args
    (let* ((creturn-st (creturn-st c-return))
	   (allocated-return ; Allocated and then returned from CL function
	    (mapcar
	     (lambda (s)
	       (or (find s c-arguments :key #'c-array:st-symbol)
		   ;; Catch programming errors, usually typos
		   (error "Could not find ~a among the arguments" s)))
	     (remove-if
	      (lambda (s)
		(member s (arglist-plain-and-categories arglist nil)))
	      (variables-used-in-c-arguments c-arguments))))
	   (pbv				; passed by value
	    #+fsbv
	    (variables-passed-by-value (cons creturn-st c-arguments))
	    #-fsbv nil)
	   (clret (or			; better as a symbol macro
		   (substitute
		    (c-array:st-symbol creturn-st) :c-return
		    (mapcar (lambda (sym)
			      (let ((it (find sym allocated-return :key 'c-array:st-symbol)))
				(if it
				    (first (cl-convert-form it))
				    sym)))
			    return))
		   (mappend
		    #'cl-convert-form
		    (callback-remove-arg allocated-return cbinfo 'c-array:st-symbol))
		   outputs
		   (unless (eq c-return :void)
		     (list (c-array:st-symbol creturn-st))))))
      (wrap-letlike
       allocated-return
       (mapcar (lambda (d) (wfo-declare d cbinfo))
	       allocated-return)
       'cffi:with-foreign-objects
       `(,@(append
	    (callback-symbol-set
	     callback-dynamic cbinfo (first callback-dynamic-variables))
	    before
	    (when callback-object (callback-set-dynamic callback-object arglist)))
	   ,@(callback-set-slots
	      cbinfo callback-dynamic-variables callback-dynamic)
	   (let ((,(c-array:st-symbol creturn-st)
		  ,(ffexpand (some 'identity pbv)
			     gsl-name
			     (append
			      (mappend
			       (lambda (arg)
				 (list (cond
					 ((member (c-array:st-symbol arg)
						  allocated-return)
					  :pointer)
					 (t (c-array:st-type arg)))
				       (c-array:st-symbol arg)))
			       (mapcar 'c-array:st-pointer-generic-pointer
				       c-arguments))
			      (list (c-array:st-type creturn-st))))))
	     ,@(case c-return
		     (:void `((declare (ignore ,(c-array:st-symbol creturn-st)))))
		     (:error-code	; fill in arguments
		      `((check-gsl-status ,(c-array:st-symbol creturn-st)
					  ',(or (defgeneric-method-p name) name)))))
	     #-native
	     ,@(when outputs
		     (mapcar
		      (lambda (x)
			`(setf (c-array:cl-invalid ,x) t
			       (c-array:c-invalid ,x) nil))
		      outputs))
	     ,@(when (eq (c-array:st-type creturn-st) :pointer)
		     `((check-null-pointer
			,(c-array:st-symbol creturn-st)
			,@'('memory-allocation-failure "No memory allocated."))))
	     ,@after
	     (values
	      ,@(defmfun-return
		 c-return (c-array:st-symbol creturn-st) clret
		 allocated-return
		 return return-supplied-p
		 enumeration outputs))))))))

(defun defmfun-return
    (c-return cret-name clret allocated return return-supplied-p enumeration outputs)
  "Generate the return computation expression for defmfun."
  (case c-return
    (:number-of-answers
     (mappend
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


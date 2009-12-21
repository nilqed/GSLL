;; Symbol-type declaration
;; Liam Healy 2009-12-20 22:27:54EST symbol-type.lisp
;; Time-stamp: <2009-12-20 22:41:13EST symbol-type.lisp>

;;; An "st" or symbol-type is a list (symbol type) where
;;; type could be (element-type array-dim).  These are examples of lists
;;; of sts: 
 ;; ((#:RET3500 SF-RESULT))
 ;; ((#:RET3501 (:DOUBLE (- NMAX NMIN)))) 
 ;; ((#:RET3502 (:DOUBLE (1+ KMAX))) (#:RET3503 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3504 (:DOUBLE (1+ KMAX))) (#:RET3505 (:DOUBLE (1+ KMAX)))
 ;;  (#:RET3506 :DOUBLE) (#:RET3507 :DOUBLE))

(in-package :c-array)

(export
 '(make-st st-symbol st-type st-pointerp st-actual-type
   st-pointer-generic-pointer))

(defun make-st (symbol type)
  (list symbol type))

(defun st-symbol (decl)
  (first decl))

(defun st-type (decl)
  (second decl))

(defun st-pointerp (decl)
  "If this st represents a pointer, return the type of the object."
  (if
   (eq (st-type decl) :pointer)
   t				   ; return T for type if unknown type
   (if (and (listp (st-type decl))
	    (eq (first (st-type decl)) :pointer))
       (second (st-type decl)))))

(defun st-actual-type (decl)
  (or (st-pointerp decl) (st-type decl)))

(defun st-pointer-generic-pointer (decl)
  (if (st-pointerp decl)
      (make-st (st-symbol decl) :pointer)
      decl))



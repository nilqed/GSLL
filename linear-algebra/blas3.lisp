;; BLAS level 3, Matrix-matrix operations
;; Liam Healy, Wed Apr 26 2006 - 21:08
;; Time-stamp: <2008-03-09 14:13:42EDT blas3.lisp>
;; $Id$

(in-package :gsl)

;;;;****************************************************************************
;;;; Options
;;;;****************************************************************************

(cffi:defcenum cblas-side
  "/usr/include/gsl/gsl_cblas.h."
  (:left 141) :right)

;;;;****************************************************************************
;;;; Generic
;;;;****************************************************************************

(export '(gemm symm trmm trsm syrk syr2k))

(defgeneric gemm (TransA TransB alpha A B beta C)
  (:method :after (TransA TransB alpha A B beta C)
	   (cl-invalidate C))
  (:documentation			; FDL
   "The matrix-matrix product and sum C = alpha
  op(A) op(B) + beta C where op(A) = A, A^T, A^H for TransA =
  :NoTrans, :Trans, :ConjTrans and similarly for the
  parameter TransB."))

(defgeneric symm (side uplo alpha A B beta C)
  (:method :after (side uplo alpha A B beta C)
	   (cl-invalidate C))
  (:documentation			; FDL
   "The matrix-matrix product and sum C = alpha A
  B + beta C for Side is :Left and C = alpha B A + beta C
  for Side is :Right, where the matrix A is symmetric. When
  Uplo is :Upper then the upper triangle and diagonal of A
  are used, and when Uplo is :Lower then the lower triangle
  and diagonal of A are used."))

(defgeneric trmm (side uplo TransA diag alpha A B)
  (:method :after (side uplo TransA diag alpha A B)
	   (cl-invalidate B))
  (:documentation			; FDL
   "The matrix-matrix product B = \alpha op(A) B
  for Side is :Left and B = \alpha B op(A) for Side is
  :Right. The matrix A is triangular and op(A) = A, A^T, A^H
  for TransA = :NoTrans, :Trans, :ConjTrans When Uplo
  is :Upper then the upper triangle of A is used, and when
  Uplo is :Lower then the lower triangle of A is used. If
  Diag is :NonUnit then the diagonal of A is used, but if
  Diag is :Unit then the diagonal elements of the matrix A
  are taken as unity and are not referenced."))

(defgeneric trsm (side uplo TransA diag alpha A B)
  (:method :after (side uplo TransA diag alpha A B)
	   (cl-invalidate B))
  (:documentation
   "The inverse-matrix matrix product B = \alpha op(inv(A))B for
   Side is :Left and B = \alpha B op(inv(A)) for Side is
   :Right. The matrix A is triangular and op(A) = A, A^T, A^H
   for TransA = :NoTrans, :Trans, :ConjTrans When
   Uplo is :Upper then the upper triangle of A is used, and
   when Uplo is :Lower then the lower triangle of A is
   used. If Diag is :NonUnit then the diagonal of A is used,
   but if Diag is :Unit then the diagonal elements of the
   matrix A are taken as unity and are not referenced."))

(defgeneric syrk (uplo trans alpha A beta C)
  (:method :after (uplo trans alpha A beta C)
	   (cl-invalidate C))
  (:documentation "A rank-k update of the symmetric matrix C, C =
  alpha A A^T + beta C when Trans is :NoTrans and C =
  alpha A^T A + beta C when Trans is :Trans. Since the
  matrix C is symmetric only its upper half or lower half need to
  be stored. When Uplo is :Upper then the upper triangle and
  diagonal of C are used, and when Uplo is :Lower then the
  lower triangle and diagonal of C are used."))

(defgeneric syr2k (uplo trans alpha A B beta C)
  (:method :after (uplo trans alpha A B beta C)
	   (cl-invalidate C))
  (:documentation "A rank-2k update of the symmetric matrix C, C
  = alpha A B^T + alpha B A^T + beta C when Trans is
  :NoTrans and C = alpha A^T B + alpha B^T A + \beta C when
  Trans is :Trans. Since the matrix C is symmetric only its
  upper half or lower half need to be stored. When Uplo is
  :Upper then the upper triangle and diagonal of C are used,
  and when Uplo is :Lower then the lower triangle and
  diagonal of C are used."))

;;;;****************************************************************************
;;;; Single
;;;;****************************************************************************

(defmfun gemm
    (TransA TransB alpha (A matrix-single-float) (B matrix-single-float)
	    beta (C matrix-single-float))
  "gsl_blas_sgemm"
  ((transa cblas-transpose) (transb cblas-transpose)
   (alpha :float) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :float) ((pointer C) gsl-matrix-c))
  :type :method)

(defmfun symm
    (side uplo alpha (A matrix-single-float) (B matrix-single-float)
	  beta (C matrix-single-float))
  "gsl_blas_ssymm"
  ((side cblas-side) (uplo cblas-uplo) (alpha :float)
   ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :float) ((pointer C) gsl-matrix-c))
  :type :method)

(defmfun trmm
    (side uplo transa diag alpha (A matrix-single-float) (B matrix-single-float))
  "gsl_blas_strmm"
  ((side cblas-side) (uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   (alpha :float) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c))
  :type :method)

(defmfun trsm
    (side uplo transa diag alpha (A matrix-single-float) (B matrix-single-float))
  "gsl_blas_strsm"
  ((side cblas-side) (uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   (alpha :float) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c))
  :type :method)

(defmfun syrk
    (uplo trans alpha (A matrix-single-float) beta (C matrix-single-float))
  "gsl_blas_ssyrk"
  ((uplo cblas-uplo) (trans cblas-transpose) (alpha :float)
   ((pointer A) gsl-matrix-c) (beta :float) ((pointer C) gsl-matrix-c))
  :type :method)

(defmfun syr2k
    (uplo trans alpha (A matrix-single-float) (B matrix-single-float)
	  beta (C matrix-single-float))
  "gsl_blas_ssyr2k"
  ((uplo cblas-uplo) (trans cblas-transpose) (alpha :float)
   ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :float) ((pointer C) gsl-matrix-c))
  :type :method)

;;;;****************************************************************************
;;;; Double
;;;;****************************************************************************

(defmfun gemm
    (TransA TransB alpha (A matrix-double-float) (B matrix-double-float)
	    beta (C matrix-double-float))
  "gsl_blas_dgemm"
  ((transa cblas-transpose) (transb cblas-transpose)
   (alpha :double) (A gsl-matrix-c) (B gsl-matrix-c)
   (beta :double) (C gsl-matrix-c))
  :type :method)

(defmfun symm
    (side uplo alpha (A matrix-double-float) (B matrix-double-float)
	  beta (C matrix-double-float))
  "gsl_blas_dsymm"
  ((side cblas-side) (uplo cblas-uplo) (alpha :double)
   ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :double) ((pointer C) gsl-matrix-c))
  :type :method)

(defmfun trmm
    (side uplo transa diag alpha (A matrix-double-float) (B matrix-double-float))
  "gsl_blas_dtrmm"
  ((side cblas-side) (uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   (alpha :double) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c))
  :type :method)

(defmfun trsm
    (side uplo transa diag alpha (A matrix-double-float) (B matrix-double-float))
  "gsl_blas_dtrsm"
  ((side cblas-side) (uplo cblas-uplo) (transa cblas-transpose) (diag cblas-diag)
   (alpha :double) ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c))
  :type :method)

(defmfun syrk
    (uplo trans alpha (A matrix-double-float) beta (C matrix-double-float))
  "gsl_blas_dsyrk"
  ((uplo cblas-uplo) (trans cblas-transpose) (alpha :double)
   ((pointer A) gsl-matrix-c) (alpha :double) ((pointer C) gsl-matrix-c))
  :type :method)

(defmfun syr2k
    (uplo trans (alpha float) (A matrix-double-float) (B matrix-double-float)
	  beta (C matrix-double-float))
  "gsl_blas_dsyr2k"
  ((uplo cblas-uplo) (trans cblas-transpose) (alpha :double)
   ((pointer A) gsl-matrix-c) ((pointer B) gsl-matrix-c)
   (beta :double) ((pointer C) gsl-matrix-c))
  :type :method)

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************


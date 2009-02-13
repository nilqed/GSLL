;; Regression test NONLINEAR-LEAST-SQUARES for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST NONLINEAR-LEAST-SQUARES
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST
                         (LIST 5.045357801443204d0 0.10404905892045835d0
                               1.0192487061031013d0))
                        (MULTIPLE-VALUE-LIST
			 (NONLINEAR-LEAST-SQUARES-EXAMPLE 40 *levenberg-marquardt* NIL))))
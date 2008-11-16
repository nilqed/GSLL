;; Regression test GEOMETRIC for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST GEOMETRIC
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
                        (LIST (LIST 1 4 3 1 3 2 1 1 2 1 1))
                        (MULTIPLE-VALUE-LIST
                         (LETM ((RNG (RANDOM-NUMBER-GENERATOR *MT19937* 0)))
                               (LOOP FOR I FROM 0 TO 10 COLLECT
                                     (GEOMETRIC RNG 0.4d0)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.24d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (GEOMETRIC-PDF 2
                                                                          0.4d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.64d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (GEOMETRIC-P 2
                                                                        0.4d0)))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0.36d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (GEOMETRIC-Q 2
                                                                        0.4d0))))


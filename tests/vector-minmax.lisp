;; Regression test VECTOR-MINMAX for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-MINMAX
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -34.5 8.24)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SINGLE-FLOAT
                                                               (A -34.5 8.24
                                                                  3.29)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -34.5d0 8.24d0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-DOUBLE-FLOAT
                                                               (A -34.5d0
                                                                  8.24d0
                                                                  3.29d0)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -68 71)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 44 189)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -68 71)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 44 189)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -68 71)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 44 189)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST -68 71)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A -64 -68 71)
                                                               NIL)))
                                                            (MINMAX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 44 189)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 67 44 189)
                                                               NIL)))
                                                            (MINMAX V1)))))

;; Regression test VECTOR-MIN-INDEX for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST VECTOR-MIN-INDEX
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SINGLE-FLOAT
                                                               (A -34.5 8.24
                                                                  3.29 -8.93
                                                                  34.12 -6.15
                                                                  49.27 -13.49)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 0)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-DOUBLE-FLOAT
                                                               (A -34.5d0
                                                                  8.24d0 3.29d0
                                                                  -8.93d0
                                                                  34.12d0
                                                                  -6.15d0
                                                                  49.27d0
                                                                  -13.49d0)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 3)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-8
                                                               (A -64 -68 71
                                                                  -91 52 -10 73
                                                                  -5)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-8
                                                               (A 67 44 189 116
                                                                  163 140 161
                                                                  215)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 3)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-16
                                                               (A -64 -68 71
                                                                  -91 52 -10 73
                                                                  -5)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-16
                                                               (A 67 44 189 116
                                                                  163 140 161
                                                                  215)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 3)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-32
                                                               (A -64 -68 71
                                                                  -91 52 -10 73
                                                                  -5)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-32
                                                               (A 67 44 189 116
                                                                  163 140 161
                                                                  215)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 3)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-SIGNED-BYTE-64
                                                               (A -64 -68 71
                                                                  -91 52 -10 73
                                                                  -5)
                                                               NIL)))
                                                            (MIN-INDEX V1))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST 1)
                                                          (MULTIPLE-VALUE-LIST
                                                           (LETM
                                                            ((V1
                                                              (VECTOR-UNSIGNED-BYTE-64
                                                               (A 67 44 189 116
                                                                  163 140 161
                                                                  215)
                                                               NIL)))
                                                            (MIN-INDEX V1)))))


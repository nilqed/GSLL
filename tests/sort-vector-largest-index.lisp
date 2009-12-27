;; Regression test SORT-VECTOR-LARGEST-INDEX for GSLL, automatically generated
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

(LISP-UNIT:DEFINE-TEST SORT-VECTOR-LARGEST-INDEX
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(6 4 1))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   'SINGLE-FLOAT
                                                                   :INITIAL-CONTENTS
                                                                   '(-34.5 8.24
                                                                     3.29 -8.93
                                                                     34.12
                                                                     -6.15
                                                                     49.27
                                                                     -13.49))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(6 4 1))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   'DOUBLE-FLOAT
                                                                   :INITIAL-CONTENTS
                                                                   '(-34.5d0
                                                                     8.24d0
                                                                     3.29d0
                                                                     -8.93d0
                                                                     34.12d0
                                                                     -6.15d0
                                                                     49.27d0
                                                                     -13.49d0))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(6 2 4))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68 71
                                                                     -91 52 -10
                                                                     73 -5))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(7 2 4))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     8)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44 189
                                                                     116 163
                                                                     140 161
                                                                     215))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(6 2 4))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68 71
                                                                     -91 52 -10
                                                                     73 -5))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(7 2 4))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     16)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44 189
                                                                     116 163
                                                                     140 161
                                                                     215))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(6 2 4))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68 71
                                                                     -91 52 -10
                                                                     73 -5))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(7 2 4))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     32)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44 189
                                                                     116 163
                                                                     140 161
                                                                     215))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(6 2 4))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(SIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(-64 -68 71
                                                                     -91 52 -10
                                                                     73 -5))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1)))))
		       #+int64
                       (LISP-UNIT::ASSERT-NUMERICAL-EQUAL (LIST #(7 2 4))
                                                          (MULTIPLE-VALUE-LIST
                                                           (LET ((COMB
                                                                  (MAKE-COMBINATION
                                                                   8 3 NIL))
                                                                 (V1
                                                                  (MAKE-MARRAY
                                                                   '(UNSIGNED-BYTE
                                                                     64)
                                                                   :INITIAL-CONTENTS
                                                                   '(67 44 189
                                                                     116 163
                                                                     140 161
                                                                     215))))
                                                             (CL-ARRAY
                                                              (SORT-VECTOR-LARGEST-INDEX
                                                               COMB V1))))))


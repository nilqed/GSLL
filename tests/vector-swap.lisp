;; Regression test VECTOR-SWAP for GSLL, automatically generated
;;
;; Copyright 2009, 2010 Liam M. Healy
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

(LISP-UNIT:DEFINE-TEST VECTOR-SWAP
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(-8.93 34.12 -6.15) #(-34.5 8.24 3.29)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    'SINGLE-FLOAT
	    :INITIAL-CONTENTS '(-34.5 8.24 3.29)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    'SINGLE-FLOAT
	    :INITIAL-CONTENTS '(-8.93 34.12 -6.15))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST #(-8.93d0 34.12d0 -6.15d0)
	  #(-34.5d0 8.24d0 3.29d0)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    'DOUBLE-FLOAT
	    :INITIAL-CONTENTS '(-34.5d0 8.24d0 3.29d0)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    'DOUBLE-FLOAT
	    :INITIAL-CONTENTS '(-8.93d0 34.12d0 -6.15d0))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST
     #(#C(-8.93 34.12) #C(-6.15 49.27) #C(-13.49 32.5))
     #(#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15))))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX SINGLE-FLOAT)
	    :INITIAL-CONTENTS
	    '(#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15))))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX SINGLE-FLOAT)
	    :INITIAL-CONTENTS
	    '(#C(-8.93 34.12) #C(-6.15 49.27) #C(-13.49 32.5)))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST
    (LIST
     #(#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0) #C(-13.49d0 32.5d0))
     #(#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0) #C(34.12d0 -6.15d0))))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX DOUBLE-FLOAT)
	    :INITIAL-CONTENTS
	    '(#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0) #C(34.12d0 -6.15d0))))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX DOUBLE-FLOAT)
	    :INITIAL-CONTENTS
	    '(#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0) #C(-13.49d0 32.5d0)))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(-91 52 -10) #(-64 -68 71)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(SIGNED-BYTE 8)
	    :INITIAL-CONTENTS '(-64 -68 71)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(SIGNED-BYTE 8)
	    :INITIAL-CONTENTS '(-91 52 -10))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(116 163 140) #(67 44 189)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(UNSIGNED-BYTE 8)
	    :INITIAL-CONTENTS '(67 44 189)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(UNSIGNED-BYTE 8)
	    :INITIAL-CONTENTS '(116 163 140))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(-91 52 -10) #(-64 -68 71)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(SIGNED-BYTE 16)
	    :INITIAL-CONTENTS '(-64 -68 71)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(SIGNED-BYTE 16)
	    :INITIAL-CONTENTS '(-91 52 -10))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(116 163 140) #(67 44 189)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(UNSIGNED-BYTE 16)
	    :INITIAL-CONTENTS '(67 44 189)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(UNSIGNED-BYTE 16)
	    :INITIAL-CONTENTS '(116 163 140))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(-91 52 -10) #(-64 -68 71)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(SIGNED-BYTE 32)
	    :INITIAL-CONTENTS '(-64 -68 71)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(SIGNED-BYTE 32)
	    :INITIAL-CONTENTS '(-91 52 -10))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(116 163 140) #(67 44 189)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(UNSIGNED-BYTE 32)
	    :INITIAL-CONTENTS '(67 44 189)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(UNSIGNED-BYTE 32)
	    :INITIAL-CONTENTS '(116 163 140))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  #+int64
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(-91 52 -10) #(-64 -68 71)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(SIGNED-BYTE 64)
	    :INITIAL-CONTENTS '(-64 -68 71)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(SIGNED-BYTE 64)
	    :INITIAL-CONTENTS '(-91 52 -10))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2)))))
  #+int64
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST (LIST #(116 163 140) #(67 44 189)))
   (MULTIPLE-VALUE-LIST
    (LET ((V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(UNSIGNED-BYTE 64)
	    :INITIAL-CONTENTS '(67 44 189)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(UNSIGNED-BYTE 64)
	    :INITIAL-CONTENTS '(116 163 140))))
      (SWAP V2 V1)
      (LIST (CL-ARRAY V1) (CL-ARRAY V2))))))


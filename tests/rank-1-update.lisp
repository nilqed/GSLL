;; Regression test RANK-1-UPDATE for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST RANK-1-UPDATE
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((33772.18 17310.045 -44960.434)
	(-13648.694 -6946.5283 18135.074)
	(34314.832 17523.164 -45541.547)))
   (MULTIPLE-VALUE-LIST
    (LET ((M1
	   (GRID:MAKE-FOREIGN-ARRAY
	    'SINGLE-FLOAT
	    :INITIAL-CONTENTS
	    '((-34.5 8.24 3.29)
	      (-8.93 34.12 -6.15)
	      (49.27 -13.49 32.5))))
	  (V1
	   (GRID:MAKE-FOREIGN-ARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS '(42.73 -17.24 43.31)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY 'SINGLE-FLOAT :INITIAL-CONTENTS '(-16.12 -8.25 21.44)))
	  (S1 -49.08))
      (GRID:COPY-TO (RANK-1-UPDATE S1 V1 V2 M1)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((33772.17700799999d0 17310.044299999998d0 -44960.429295999995d0)
	(-13648.693903999998d0 -6946.528399999999d0 18135.074447999996d0)
	(34314.825376d0 17523.1621d0 -45541.53891200001d0)))
   (MULTIPLE-VALUE-LIST
    (LET ((M1
	   (GRID:MAKE-FOREIGN-ARRAY
	    'DOUBLE-FLOAT
	    :INITIAL-CONTENTS
	    '((-34.5d0 8.24d0 3.29d0)
	      (-8.93d0 34.12d0 -6.15d0)
	      (49.27d0 -13.49d0 32.5d0))))
	  (V1
	   (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS '(42.73d0 -17.24d0 43.31d0)))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :INITIAL-CONTENTS '(-16.12d0 -8.25d0 21.44d0)))
	  (S1 -49.08d0))
      (GRID:COPY-TO (RANK-1-UPDATE S1 V1 V2 M1)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((#C(37793.65 36629.23) #C(-101265.76 118286.42)
	   #C(68357.26 171191.28))
	(#C(36918.79 37780.418) #C(-104760.06 115889.945)
	   #C(63820.45 173533.31))
	(#C(-26166.797 1319.2356) #C(-8642.514 -77109.22)
	   #C(-85661.76 -33283.21))))
   (MULTIPLE-VALUE-LIST
    (LET ((M1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX SINGLE-FLOAT)
	    :INITIAL-CONTENTS
	    '((#C(-34.5 8.24) #C(3.29 -8.93) #C(34.12 -6.15))
	      (#C(-8.93 34.12) #C(-6.15 49.27) #C(-13.49 32.5))
	      (#C(49.27 -13.49) #C(32.5 42.73) #C(-17.24 43.31)))))
	  (V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX SINGLE-FLOAT)
	    :INITIAL-CONTENTS
	    '(#C(42.73 -17.24) #C(43.31 -16.12) #C(-8.25 21.44))))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX SINGLE-FLOAT)
	    :INITIAL-CONTENTS
	    '(#C(-16.12 -8.25) #C(21.44 -49.08) #C(-39.66 -49.46))))
	  (S1 #C(-49.08 -39.66)))
      (GRID:COPY-TO (RANK-1-UPDATE S1 V1 V2 M1)))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    #2A((#C(37793.646065999994d0 36629.23161199999d0)
	   #C(-101265.76059999998d0 118286.41839999997d0)
	   #C(68357.25449199998d0 171191.29244399996d0))
	(#C(36918.78463d0 37780.41610000001d0)
	   #C(-104760.05796d0 115889.92672d0) #C(63820.44154d0 173533.30234d0))
	(#C(-26166.794498000003d0 1319.234524000003d0)
	   #C(-8642.510840000003d0 -77109.20672d0)
	   #C(-85661.74775600001d0 -33283.210252d0))))
   (MULTIPLE-VALUE-LIST
    (LET ((M1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX DOUBLE-FLOAT)
	    :INITIAL-CONTENTS
	    '((#C(-34.5d0 8.24d0) #C(3.29d0 -8.93d0) #C(34.12d0 -6.15d0))
	      (#C(-8.93d0 34.12d0) #C(-6.15d0 49.27d0) #C(-13.49d0 32.5d0))
	      (#C(49.27d0 -13.49d0) #C(32.5d0 42.73d0) #C(-17.24d0 43.31d0)))))
	  (V1
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX DOUBLE-FLOAT)
	    :INITIAL-CONTENTS
	    '(#C(42.73d0 -17.24d0)
	      #C(43.31d0 -16.12d0)
	      #C(-8.25d0 21.44d0))))
	  (V2
	   (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX DOUBLE-FLOAT)
	    :INITIAL-CONTENTS
	    '(#C(-16.12d0 -8.25d0)
	      #C(21.44d0 -49.08d0)
	      #C(-39.66d0 -49.46d0))))
	  (S1 #C(-49.08d0 -39.66d0)))
      (GRID:COPY-TO (RANK-1-UPDATE S1 V1 V2 M1))))))


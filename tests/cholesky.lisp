;; Regression test CHOLESKY for GSLL, automatically generated

(in-package :gsl)

(LISP-UNIT:DEFINE-TEST CHOLESKY
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb2-soln*)
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM *HILB2*))))
  (let ((lisp-unit:*epsilon* (* 2 128 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb3-soln*)
     (MULTIPLE-VALUE-LIST
      (TEST-SV-SOLVE-DIM *HILB3*))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb4-soln*)
     (MULTIPLE-VALUE-LIST (TEST-SV-SOLVE-DIM *hilb4*))))
  (let ((lisp-unit:*epsilon* 0.5d0))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb12-soln*)
     (MULTIPLE-VALUE-LIST (TEST-SV-SOLVE-DIM *hilb12*))))
  (let ((lisp-unit:*epsilon* (* 2 16 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *hilb2*)
     (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-DECOMP-DIM *HILB2*))))
  (let ((lisp-unit:*epsilon* (* 2 128 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *HILB3*)
     (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-DECOMP-DIM *HILB3*))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *HILB4*)
     (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-DECOMP-DIM *HILB4*))))
  (let ((lisp-unit:*epsilon* (* 2 2048 double-float-epsilon)))
    (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
     (LIST *HILB12*)
     (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-DECOMP-DIM *HILB12*))))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :DIMENSIONS '(2 2) :INITIAL-CONTENTS
                 '(4.0d0 -6.000000000000001d0 -6.000000000000001d0
                   12.000000000000002d0)))
   (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-INVERT-DIM *HILB2*)))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :DIMENSIONS '(3 3) :INITIAL-CONTENTS
                 '(8.999999999999996d0 -35.99999999999997d0
                   29.999999999999968d0 -35.99999999999997d0
                   191.99999999999977d0 -179.99999999999974d0
                   29.999999999999968d0 -179.99999999999974d0
                   179.99999999999974d0)))
   (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-INVERT-DIM *HILB3*)))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :DIMENSIONS '(4 4) :INITIAL-CONTENTS
                 '(15.999999999997954d0 -119.99999999997637d0
                   239.9999999999425d0 -139.9999999999624d0
                   -119.99999999997637d0 1199.9999999997276d0
                   -2699.9999999993374d0 1679.999999999567d0
                   239.9999999999425d0 -2699.9999999993374d0
                   6479.999999998391d0 -4199.999999998949d0
                   -139.9999999999624d0 1679.999999999567d0
                   -4199.999999998949d0 2799.9999999993133d0)))
   (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-INVERT-DIM *HILB4*)))
  (LISP-UNIT:ASSERT-NUMERICAL-EQUAL
   (LIST
    (GRID:MAKE-FOREIGN-ARRAY 'DOUBLE-FLOAT :DIMENSIONS '(12 12) :INITIAL-CONTENTS
                 '(140.93894922210026d0 -9909.515598314396d0
                   228130.1469667386d0 -2538275.7124035973d0
                   1.6095812913531223d7 -6.334945916568248d7
                   1.6180891328288838d8 -2.729032142808902d8
                   3.0168290726564497d8 -2.1022884505162454d8
                   8.375524587383476d7 -1.4541456776519377d7
                   -9909.515598314396d0 932767.1833354546d0
                   -2.4237461160498716d7 2.8844188661576414d8
                   -1.909680714415059d9 7.7460245161891d9
                   -2.0231624766352333d10 3.471522329754068d10
                   -3.890662204538243d10 2.74177855208454d10
                   -1.102566741945975d10 1.929435802259886d9
                   228130.1469667386d0 -2.4237461160498716d7 6.73572464659023d8
                   -8.368269872726659d9 5.7091322157235695d10
                   -2.3676705075886935d11 6.290662520578335d11
                   -1.0941791365599775d12 1.2399453133327766d12
                   -8.81880779657557d11 3.574046855772319d11
                   -6.296195191459201d10 -2538275.7124035973d0
                   2.8844188661576414d8 -8.368269872726659d9
                   1.0712764655128993d11 -7.472089890616971d11
                   3.151988258294786d12 -8.488450614305809d12
                   1.4927814906150715d13 -1.7071606428565754d13
                   1.223562085794148d13 -4.991562204869468d12
                   8.843597326374552d11 1.6095812913531223d7
                   -1.909680714415059d9 5.7091322157235695d10
                   -7.472089890616971d11 5.301074505619902d12
                   -2.2665189250955867d13 6.171063739258524d13
                   -1.0951444151196363d14 1.2620388151208275d14
                   -9.10465178565691d13 3.7353182667592734d13
                   -6.650622666809788d12 -6.334945916568248d7 7.7460245161891d9
                   -2.3676705075886935d11 3.151988258294786d12
                   -2.2665189250955867d13 9.797282297050839d13
                   -2.69178428716829d14 4.813523771637035d14
                   -5.583296199291294d14 4.0506228228731506d14
                   -1.669993952432146d14 2.986227787462567d13
                   1.6180891328288838d8 -2.0231624766352333d10
                   6.290662520578335d11 -8.488450614305809d12
                   6.171063739258524d13 -2.69178428716829d14
                   7.452168344520158d14 -1.341299342463819d15
                   1.5645387966054885d15 -1.1406123499040888d15
                   4.722759913598374d14 -8.477277839776764d13
                   -2.729032142808902d8 3.471522329754068d10
                   -1.0941791365599775d12 1.4927814906150715d13
                   -1.0951444151196363d14 4.813523771637035d14
                   -1.341299342463819d15 2.4277247809232295d15
                   -2.8456190095051365d15 2.0834751168291395d15
                   -8.659507329260588d14 1.5596335658322444d14
                   3.0168290726564497d8 -3.890662204538243d10
                   1.2399453133327766d12 -1.7071606428565754d13
                   1.2620388151208275d14 -5.583296199291294d14
                   1.5645387966054885d15 -2.8456190095051365d15
                   3.349749344363914d15 -2.4618882271582375d15
                   1.0266886429154235d15 -1.854737730613888d14
                   -2.1022884505162454d8 2.74177855208454d10
                   -8.81880779657557d11 1.223562085794148d13
                   -9.10465178565691d13 4.0506228228731506d14
                   -1.1406123499040888d15 2.0834751168291395d15
                   -2.4618882271582375d15 1.8154690416182195d15
                   -7.594022010106465d14 1.3756208760169238d14
                   8.375524587383476d7 -1.102566741945975d10
                   3.574046855772319d11 -4.991562204869468d12
                   3.7353182667592734d13 -1.669993952432146d14
                   4.722759913598374d14 -8.659507329260588d14
                   1.0266886429154235d15 -7.594022010106465d14
                   3.1852041828393675d14 -5.7840886188367125d13
                   -1.4541456776519377d7 1.929435802259886d9
                   -6.296195191459201d10 8.843597326374552d11
                   -6.650622666809788d12 2.986227787462567d13
                   -8.477277839776764d13 1.5596335658322444d14
                   -1.854737730613888d14 1.3756208760169238d14
                   -5.7840886188367125d13 1.0527040765219566d13)))
   (MULTIPLE-VALUE-LIST (TEST-CHOLESKY-INVERT-DIM *HILB12*))))



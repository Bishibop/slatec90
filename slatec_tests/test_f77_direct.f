      PROGRAM TEST_F77
      COMPLEX Z, CSH, CCH
      REAL X, Y
      
      X = 0.0
      Y = 1.5707963267948966
      Z = CMPLX(X, Y)
      CALL CSHCH(Z, CSH, CCH)
      PRINT *, 'F77 Test: z = 0 + 1.5708i'
      PRINT *, '  CSH = ', CSH
      PRINT *, '  CCH = ', CCH
      PRINT *, '  cos(1.5708) = ', COS(Y)
      END
      PROGRAM TESTLAMW
C     Test program for LAMW - Lambert W function implementation
C
      REAL LAMW, X, W, WEXPECT, ERROR, RELERR, VERIFY
      REAL R1MACH, EPS
      INTEGER I, NTEST, NPASS, NFAIL
      PARAMETER (NTEST = 16)
C
C     Test data - from high precision reference calculations
      REAL XTEST(NTEST), WTEST(NTEST)
      DATA XTEST / -0.367879, -0.300000, -0.200000, -0.100000,
     &              0.000000,  0.100000,  0.200000,  0.500000,
     &              1.000000,  2.000000,  2.718282,  5.000000,
     &             10.000000, 20.000000, 50.000000, 100.000000 /
C
      DATA WTEST / -0.999527, -0.489402, -0.259171, -0.111833,
     &              0.000000,  0.091277,  0.168916,  0.351734,
     &              0.567143,  0.852606,  1.000000,  1.326725,
     &              1.745528,  2.205003,  2.860890,  3.385630 /
C
      EPS = R1MACH(3)
      NPASS = 0
      NFAIL = 0
C
      PRINT *, 'Testing LAMW - Lambert W Function Implementation'
      PRINT *, '==============================================='
      PRINT *, ''
      PRINT *, 'Machine epsilon:', EPS
      PRINT *, ''
      PRINT *, '      X           W(X)       Expected     Error',
     &         '      Rel.Error   Verify'
      PRINT *, '----------------------------------------------',
     &         '-------------------------'
C
      DO 10 I = 1, NTEST
         X = XTEST(I)
         WEXPECT = WTEST(I)
         W = LAMW(X)
         ERROR = ABS(W - WEXPECT)
         IF (ABS(WEXPECT) .GT. 0.0) THEN
            RELERR = ERROR / ABS(WEXPECT)
         ELSE
            RELERR = ERROR
         END IF
C
C        Verify: X = W * EXP(W)
         VERIFY = W * EXP(W)
C
         WRITE(*, 100) X, W, WEXPECT, ERROR, RELERR, VERIFY
C
C        Check if error is acceptable (100 * machine epsilon)
         IF (RELERR .LT. 100.0 * EPS) THEN
            NPASS = NPASS + 1
         ELSE
            NFAIL = NFAIL + 1
            PRINT *, '  *** FAILED ***'
         END IF
   10 CONTINUE
C
  100 FORMAT(F10.6, 5F12.6)
C
      PRINT *, ''
      PRINT *, 'Test Summary:'
      PRINT *, '============='
      PRINT *, 'Total tests:', NTEST
      PRINT *, 'Passed:     ', NPASS
      PRINT *, 'Failed:     ', NFAIL
      PRINT *, ''
C
C     Test special values
      PRINT *, 'Special Value Tests:'
      PRINT *, '==================='
C
C     Test W(0) = 0
      X = 0.0
      W = LAMW(X)
      PRINT *, 'W(0) =', W, ' (should be 0.0)'
C
C     Test W(e) = 1
      X = 2.718281828
      W = LAMW(X)
      PRINT *, 'W(e) =', W, ' (should be 1.0)'
C
C     Test W(-1/e) = -1
      X = -0.36787944
      W = LAMW(X)
      PRINT *, 'W(-1/e) =', W, ' (should be -1.0)'
C
C     Test derivative at x=1 (Omega constant)
C     W'(1) = W(1)/(1*(1+W(1))) = Omega/(1+Omega)
      X = 1.0
      W = LAMW(X)
      PRINT *, 'W(1) =', W, ' (Omega constant ≈ 0.567143)'
C
C     Test error condition: X < -1/e
      PRINT *, ''
      PRINT *, 'Error condition test (X < -1/e):'
      X = -0.5
      W = LAMW(X)
      PRINT *, 'W(-0.5) =', W, ' (should trigger error)'
C
      END
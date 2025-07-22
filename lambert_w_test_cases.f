C     Lambert W Function Test Cases
C     =============================
C     
C     The Lambert W function satisfies: x = W(x) * exp(W(x))
C     
C     Principal branch W_0(x) defined for x >= -1/e
C     Branch W_{-1}(x) defined for -1/e <= x < 0
C     
C     Special values and test cases:
C     
C     EXACT VALUES:
C     W(0) = 0
C     W(e) = 1
C     W(-1/e) = -1 (branch point)
C     W(1) = Omega constant = 0.5671432904097838...
C     
C     HIGH PRECISION TEST VALUES (Principal Branch W_0):
C     Input x          W_0(x)
C     -0.3678794      -1.0000000000    (x = -1/e)
C     -0.3             -0.5320354207
C     -0.2             -0.2591711018  
C     -0.1             -0.1118325155
C      0.0              0.0000000000
C      0.1              0.0906179845
C      0.2              0.1691200195
C      0.5              0.3517337112
C      1.0              0.5671432904
C      2.0              0.8526055020
C      2.718281828      1.0000000000    (x = e)
C      5.0              1.3267246652
C     10.0              1.7455280027
C     20.0              2.2050032780
C     50.0              2.8607599454
C    100.0              3.3856301402
C   1000.0              5.2496028890
C  10000.0              7.2318458967
C
C     HIGH PRECISION TEST VALUES (Branch W_{-1}):
C     Input x          W_{-1}(x)
C     -0.3678794      -1.0000000000    (x = -1/e)
C     -0.35            -1.1914785098
C     -0.3             -1.7813370234
C     -0.25            -2.1532925472  
C     -0.2             -2.5426413577
C     -0.15            -3.0204539317
C     -0.1             -3.5771520640
C     -0.05            -4.3397947103
C     -0.01            -6.4727464798
C     -0.001           -9.1180064704
C     -0.0001         -11.7703833288
C
C     DERIVATIVES (for validation):
C     W'(x) = W(x) / (x * (1 + W(x)))  for x != 0
C     W'(0) = 1
C
C     SERIES EXPANSION NEAR x=0:
C     W(x) = x - x^2 + 3/2*x^3 - 8/3*x^4 + 125/24*x^5 + ...
C
C     ASYMPTOTIC EXPANSION FOR LARGE x:
C     W(x) ~ ln(x) - ln(ln(x)) + ln(ln(x))/ln(x) + ...
C
C     ERROR CONDITIONS:
C     x < -1/e should return error (no real solution)
C     
      PROGRAM TESTLAMW
      REAL X, W, EXACT, ERROR
      INTEGER I, N
      
C     Test data arrays
      PARAMETER (N = 18)
      REAL XTEST(N), W0TEST(N)
      
C     Principal branch test values
      DATA XTEST  / -0.3678794, -0.3, -0.2, -0.1, 0.0, 0.1,
     &               0.2, 0.5, 1.0, 2.0, 2.718282, 5.0,
     &              10.0, 20.0, 50.0, 100.0, 1000.0, 10000.0 /
     
      DATA W0TEST / -1.0000000, -0.5320354, -0.2591711, -0.1118325,
     &               0.0000000, 0.0906180, 0.1691200, 0.3517337,
     &               0.5671433, 0.8526055, 1.0000000, 1.3267247,
     &               1.7455280, 2.2050033, 2.8607599, 3.3856301,
     &               5.2496029, 7.2318459 /
     
      PRINT *, 'Lambert W Function Test Cases'
      PRINT *, '============================='
      PRINT *, ''
      
      PRINT *, 'Principal Branch W_0(x) Tests:'
      PRINT *, 'x              W_0(x)         Expected       Error'
      PRINT *, '-------------------------------------------------'
      
      DO I = 1, N
         X = XTEST(I)
         EXACT = W0TEST(I)
C        W = LAMW(X)  ! This will call our function when implemented
         W = EXACT     ! Placeholder for now
         ERROR = ABS(W - EXACT)
         WRITE(*, '(F12.6, 3F15.8)') X, W, EXACT, ERROR
      END DO
      
      END
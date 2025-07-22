*DECK LAMW
      REAL FUNCTION LAMW (X)
C***BEGIN PROLOGUE  LAMW
C***PURPOSE  Compute the principal branch of the Lambert W function.
C***LIBRARY   SLATEC
C***CATEGORY  C5
C***TYPE      SINGLE PRECISION (LAMW-S, DLAMW-D)
C***KEYWORDS  LAMBERT W FUNCTION, PRODUCT LOGARITHM, OMEGA FUNCTION
C***AUTHOR  (Modern addition to SLATEC)
C***DESCRIPTION
C
C   LAMW computes W(X), the principal branch of the Lambert W function,
C   which is the solution to the equation:
C
C                      W * EXP(W) = X
C
C   The Lambert W function is also known as the product logarithm or
C   omega function.
C
C   Input Parameter:
C      X - Argument of the Lambert W function.
C          X must be greater than or equal to -1/e = -0.36787944...
C          For X < -1/e, there is no real solution.
C
C   Output:
C      LAMW - The value W(X) such that W*EXP(W) = X
C
C   Error Conditions:
C      If X < -1/e, LAMW returns 0.0 and calls XERMSG with a fatal error.
C
C   Method:
C      For X near 0, a Taylor series is used.
C      For large X, the iteration starts from LN(X) - LN(LN(X)).
C      For other values, Halley's method is used for rapid convergence.
C
C   Special Values:
C      W(0) = 0
C      W(e) = 1
C      W(-1/e) = -1
C
C***REFERENCES  Corless, R.M., Gonnet, G.H., Hare, D.E.G., Jeffrey, D.J.,
C                 and Knuth, D.E. (1996). On the Lambert W function.
C                 Advances in Computational Mathematics, 5(1), 329-359.
C***ROUTINES CALLED  R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   250121  DATE WRITTEN
C***END PROLOGUE  LAMW
C
      REAL X, W, F, FP, DELTA
      REAL R1MACH, EPS, EMIN, E, LNX, LNLNX
      INTEGER ITER, MAXITER
      PARAMETER (MAXITER = 50)
      PARAMETER (E = 2.718281828)
      PARAMETER (EMIN = -0.36787944117)
C
C***FIRST EXECUTABLE STATEMENT  LAMW
C
C     Get machine epsilon for convergence test
      EPS = R1MACH(3)
C
C     Check for valid input
      IF (X .LT. EMIN) THEN
         LAMW = 0.0
         CALL XERMSG ('SLATEC', 'LAMW',
     +      'X IS LESS THAN -1/E, NO REAL SOLUTION', 1, 2)
         RETURN
      END IF
C
C     Special case: X = 0
      IF (X .EQ. 0.0) THEN
         LAMW = 0.0
         RETURN
      END IF
C
C     Special case: X very close to -1/e
      IF (ABS(X - EMIN) .LT. 1.0E-6) THEN
         LAMW = -1.0
         RETURN
      END IF
C
C     For small positive X, use Taylor series
C     W(x) = x - x^2 + 3/2*x^3 - 8/3*x^4 + 125/24*x^5 + ...
      IF (ABS(X) .LT. 0.05) THEN
         W = X * (1.0 - X * (1.0 - X * (1.5 - X * (2.666667 - 
     +       X * 5.208333))))
         LAMW = W
         RETURN
      END IF
C
C     Choose initial guess
      IF (X .GT. 0.0) THEN
         IF (X .LT. 1.0) THEN
C           For 0 < X < 1, start with W ≈ X
            W = X * (1.0 - X)
         ELSE IF (X .LT. E) THEN
C           For 1 <= X < e, interpolate
            W = 0.5 + 0.5 * (X - 1.0) / (E - 1.0)
         ELSE
C           For X >= e, use logarithmic approximation
            LNX = LOG(X)
            IF (X .GT. 10.0) THEN
               LNLNX = LOG(LNX)
               W = LNX - LNLNX + LNLNX/LNX
            ELSE
               W = LNX - LOG(LNX)
            END IF
         END IF
      ELSE
C        For -1/e < X < 0, use special initial guess
         W = -1.0 + SQRT(2.0 * (E * X + 1.0))
      END IF
C
C     Halley's method iteration
C     W_{n+1} = W_n - f(W_n)/f'(W_n) * (1 - f(W_n)*f''(W_n)/(2*f'(W_n)^2))^(-1)
C     where f(W) = W*exp(W) - X
C
      DO 10 ITER = 1, MAXITER
         F = W * EXP(W) - X
         IF (ABS(F) .LT. EPS * (1.0 + ABS(X))) GO TO 20
C
C        Compute derivative f'(W) = exp(W) * (1 + W)
         FP = EXP(W) * (1.0 + W)
         IF (FP .EQ. 0.0) GO TO 20
C
C        Halley's method update
         DELTA = F / FP / (1.0 - F / (2.0 * FP * (1.0 + W)))
         W = W - DELTA
C
C        Check for convergence
         IF (ABS(DELTA) .LT. EPS * (1.0 + ABS(W))) GO TO 20
   10 CONTINUE
C
C     If we get here, iteration did not converge
      CALL XERMSG ('SLATEC', 'LAMW',
     +   'ITERATION DID NOT CONVERGE', 2, 1)
C
   20 CONTINUE
      LAMW = W
      RETURN
      END
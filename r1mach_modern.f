*DECK R1MACH
      REAL FUNCTION R1MACH (I)
C***BEGIN PROLOGUE  R1MACH
C***PURPOSE  Return floating point machine dependent constants.
C***LIBRARY   SLATEC
C***CATEGORY  R1
C***TYPE      SINGLE PRECISION (R1MACH-S, D1MACH-D)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***DESCRIPTION
C
C   Modern implementation using Fortran intrinsics for portability
C
C   R1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
C   R1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C   R1MACH(3) = B**(-T), the smallest relative spacing.
C   R1MACH(4) = B**(1-T), the largest relative spacing.
C   R1MACH(5) = LOG10(B)
C
C***END PROLOGUE  R1MACH
C
C     Modern implementation using Fortran intrinsics
C***FIRST EXECUTABLE STATEMENT  R1MACH
      IF (I .LT. 1  .OR.  I .GT. 5) THEN
         CALL XERMSG ('SLATEC', 'R1MACH', 'I OUT OF BOUNDS', 1, 2)
      END IF
C
      IF (I .EQ. 1) THEN
         R1MACH = TINY(1.0)
      ELSE IF (I .EQ. 2) THEN
         R1MACH = HUGE(1.0)
      ELSE IF (I .EQ. 3) THEN
         R1MACH = EPSILON(1.0)
      ELSE IF (I .EQ. 4) THEN
         R1MACH = EPSILON(1.0)
      ELSE IF (I .EQ. 5) THEN
         R1MACH = LOG10(REAL(RADIX(1.0)))
      END IF
C
      RETURN
      END
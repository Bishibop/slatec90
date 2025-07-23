#!/usr/bin/env python3
"""
Debug the infinity case specifically to understand F77 behavior.
"""

import subprocess
import tempfile
import os
import math

def test_f77_infinity():
    """Test F77 DENORM with infinity input."""
    
    f77_program = '''
      PROGRAM TEST_INFINITY
      IMPLICIT NONE
      DOUBLE PRECISION X(1), RESULT, DENORM
      DOUBLE PRECISION HUGE_VAL
      
C     Create infinity value
      HUGE_VAL = 1.0D308
      X(1) = HUGE_VAL * HUGE_VAL  ! This should create infinity
      
      WRITE(*,*) 'Input value:', X(1)
      WRITE(*,*) 'Is infinite?', .NOT.(X(1) .LE. HUGE_VAL)
      
      RESULT = DENORM(1, X)
      WRITE(*,*) 'DENORM result:', RESULT
      
      END

C     Original F77 DENORM implementation
      DOUBLE PRECISION FUNCTION DENORM(N,X)
      INTEGER N
      DOUBLE PRECISION X(*)
      INTEGER I
      DOUBLE PRECISION AGIANT,FLOATN,ONE,RDWARF,RGIANT,S1,S2,S3,
     *                 X1MAX,X3MAX,XABS,ZERO
      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT/FLOATN
      DO 90 I = 1, N
         XABS = DABS(X(I))
         WRITE(*,*) 'Processing element', I, 'XABS=', XABS
         WRITE(*,*) 'AGIANT=', AGIANT
         IF (XABS .GE. AGIANT) GO TO 70
         IF (XABS .GT. RDWARF) GO TO 30
         IF (XABS .LE. X3MAX) GO TO 10
         S3 = ONE + S3*(X3MAX/XABS)**2
         X3MAX = XABS
         GO TO 90
   10    CONTINUE
         IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2
         GO TO 90
   30    CONTINUE
         S2 = S2 + XABS**2
         WRITE(*,*) 'Added to S2, S2=', S2
         GO TO 90
   70    CONTINUE
         WRITE(*,*) 'Large component branch, X1MAX=', X1MAX
         IF (XABS .LE. X1MAX) GO TO 80
         S1 = ONE + S1*(X1MAX/XABS)**2
         X1MAX = XABS
         WRITE(*,*) 'Updated X1MAX=', X1MAX, 'S1=', S1
         GO TO 90
   80    CONTINUE
         S1 = S1 + (XABS/X1MAX)**2
         WRITE(*,*) 'Updated S1=', S1
   90    CONTINUE
      WRITE(*,*) 'Final: S1=', S1, 'S2=', S2, 'S3=', S3
      WRITE(*,*) 'Final: X1MAX=', X1MAX, 'X3MAX=', X3MAX
      IF (S1 .EQ. ZERO) GO TO 100
      DENORM = X1MAX*DSQRT(S1+(S2/X1MAX)/X1MAX)
      WRITE(*,*) 'Branch 1: DENORM=', DENORM
      RETURN
  100 CONTINUE
      IF (S2 .EQ. ZERO) GO TO 110
      IF (S2 .GE. X3MAX)
     *   DENORM = DSQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
      IF (S2 .LT. X3MAX)
     *   DENORM = DSQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
      WRITE(*,*) 'Branch 2: DENORM=', DENORM
      RETURN
  110 CONTINUE
      DENORM = X3MAX*DSQRT(S3)
      WRITE(*,*) 'Branch 3: DENORM=', DENORM
      RETURN
      END
'''
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(f77_program)
        f77_file = f.name
    
    try:
        # Compile F77 program
        exe_file = f77_file.replace('.f', '')
        compile_result = subprocess.run(
            ['gfortran', '-o', exe_file, f77_file],
            capture_output=True, text=True
        )
        
        if compile_result.returncode != 0:
            print(f"F77 compilation failed: {compile_result.stderr}")
            return
        
        # Run F77 program
        run_result = subprocess.run(
            [exe_file],
            capture_output=True, text=True
        )
        
        print("F77 Output:")
        print(run_result.stdout)
        if run_result.stderr:
            print("F77 Stderr:")
            print(run_result.stderr)
        
    finally:
        # Clean up
        for f in [f77_file, exe_file]:
            if os.path.exists(f):
                os.unlink(f)

if __name__ == '__main__':
    test_f77_infinity()
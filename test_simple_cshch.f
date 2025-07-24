      PROGRAM TEST_SIMPLE_CSHCH
      COMPLEX Z, CSH, CCH
      REAL PI
      
      PI = 3.14159265358979
      
C     Test case 1: z = 0 + i*pi/2
      Z = CMPLX(0.0, PI/2.0)
      CALL CSHCH(Z, CSH, CCH)
      WRITE(*,*) 'Test 1: z = 0 + i*pi/2'
      WRITE(*,*) 'sinh real:', REAL(CSH), ' imag:', AIMAG(CSH)
      WRITE(*,*) 'cosh real:', REAL(CCH), ' imag:', AIMAG(CCH)
      WRITE(*,*)
      
C     Test case 2: z = 100 + 0i
      Z = CMPLX(100.0, 0.0)
      CALL CSHCH(Z, CSH, CCH)
      WRITE(*,*) 'Test 2: z = 100 + 0i'  
      WRITE(*,*) 'sinh real:', REAL(CSH), ' imag:', AIMAG(CSH)
      WRITE(*,*) 'cosh real:', REAL(CCH), ' imag:', AIMAG(CCH)
      
      END
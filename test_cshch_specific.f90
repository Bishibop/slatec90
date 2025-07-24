program test_cshch_specific
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    real :: x, y
    
    ! Test 22: z = 0 + 1.5708i (π/2) - Expected cosh_real = 3.139e-07
    x = 0.0
    y = 1.5707963267948966
    z = cmplx(x, y)
    call cshch(z, csh, cch)
    print '(A)', "Test 22: z = 0 + 1.5708i"
    print '(A,ES23.15,A,ES23.15,A)', "  My csh = (", real(csh), ", ", aimag(csh), ")"
    print '(A,ES23.15,A,ES23.15,A)', "  My cch = (", real(cch), ", ", aimag(cch), ")"
    print '(A)', "  Expected csh = (0.0, 1.0)"
    print '(A)', "  Expected cch = (3.139164733e-07, 0.0)"
    print '(A,ES23.15)', "  Error in cosh_real: ", abs(real(cch) - 3.139164733e-07)
    print *, ""
    
    ! Test 23: z = 0 + 3.1416i (π) - Expected sinh_imag = -7.239e-06
    x = 0.0
    y = 3.141592653589793
    z = cmplx(x, y)
    call cshch(z, csh, cch)
    print '(A)', "Test 23: z = 0 + 3.1416i"
    print '(A,ES23.15,A,ES23.15,A)', "  My csh = (", real(csh), ", ", aimag(csh), ")"
    print '(A,ES23.15,A,ES23.15,A)', "  My cch = (", real(cch), ", ", aimag(cch), ")"
    print '(A)', "  Expected csh = (0.0, -7.239119789e-06)"
    print '(A)', "  Expected cch = (-1.0, 0.0)"
    print '(A,ES23.15)', "  Error in sinh_imag: ", abs(aimag(csh) - (-7.239119789e-06))
    print *, ""
    
    ! Test overflow: z = 100 + 0i
    x = 100.0
    y = 0.0
    z = cmplx(x, y)
    call cshch(z, csh, cch)
    print '(A)', "Test overflow: z = 100 + 0i"
    print '(A,ES23.15,A,ES23.15,A)', "  My csh = (", real(csh), ", ", aimag(csh), ")"
    print '(A,ES23.15,A,ES23.15,A)', "  My cch = (", real(cch), ", ", aimag(cch), ")"
    print '(A)', "  Should be large but finite values"
    
end program test_cshch_specific
program test_cshch_basic
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    
    ! Test case 1: z = 1 + 0i
    z = cmplx(1.0, 0.0)
    call cshch(z, csh, cch)
    print *, "Test 1: z = 1 + 0i"
    print *, "  csh = ", csh, " (expected: ~1.1752 + 0i)"
    print *, "  cch = ", cch, " (expected: ~1.5431 + 0i)"
    print *, "  sinh(1) = ", sinh(1.0)
    print *, "  cosh(1) = ", cosh(1.0)
    print *, ""
    
    ! Test case 2: z = 0 + i
    z = cmplx(0.0, 1.0)
    call cshch(z, csh, cch)
    print *, "Test 2: z = 0 + i"
    print *, "  csh = ", csh, " (expected: ~0 + 0.8415i)"
    print *, "  cch = ", cch, " (expected: ~0.5403 + 0i)"
    print *, "  sin(1) = ", sin(1.0)
    print *, "  cos(1) = ", cos(1.0)
    print *, ""
    
    ! Test case 3: z = 1 + i
    z = cmplx(1.0, 1.0)
    call cshch(z, csh, cch)
    print *, "Test 3: z = 1 + i"
    print *, "  csh = ", csh
    print *, "  cch = ", cch
    print *, ""
    
    ! Manual calculation for z = 1 + i
    print *, "Manual calculation for z = 1 + i:"
    print *, "  sinh(1)*cos(1) = ", sinh(1.0)*cos(1.0)
    print *, "  cosh(1)*sin(1) = ", cosh(1.0)*sin(1.0)
    print *, "  cosh(1)*cos(1) = ", cosh(1.0)*cos(1.0)
    print *, "  sinh(1)*sin(1) = ", sinh(1.0)*sin(1.0)
    
end program test_cshch_basic
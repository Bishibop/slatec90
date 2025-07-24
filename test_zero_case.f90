program test_zero_case
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    
    ! Test z = 0 + 0i
    z = cmplx(0.0, 0.0)
    call cshch(z, csh, cch)
    
    print *, "Test: z = 0 + 0i"
    print *, "  My csh = ", csh
    print *, "  My cch = ", cch
    print *, "  Expected: csh = (0,0), cch = (1,0)"
    print *, "  sinh(0) = ", sinh(0.0)
    print *, "  cosh(0) = ", cosh(0.0)
    
end program test_zero_case
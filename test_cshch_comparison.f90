program test_cshch_comparison
    use cshch_module, only: cshch
    implicit none
    
    complex :: z, csh_modern, cch_modern, csh_f77, cch_f77
    real :: x, y
    
    ! Test case that supposedly has issues: z = 0 + i*pi/2
    x = 0.0
    y = 1.5707963
    z = cmplx(x, y)
    
    ! Call modern version
    call cshch(z, csh_modern, cch_modern)
    
    ! Call F77 version with same signature
    call cshch_f77(z, csh_f77, cch_f77)
    
    print *, "Test case: z = 0 + i*pi/2"
    print *, "Modern: sinh =", real(csh_modern), aimag(csh_modern)
    print *, "        cosh =", real(cch_modern), aimag(cch_modern)
    print *, "F77:    sinh =", real(csh_f77), aimag(csh_f77)
    print *, "        cosh =", real(cch_f77), aimag(cch_f77)
    print *, ""
    
    ! Test another case: z = 100 + 0i (overflow test)
    x = 100.0
    y = 0.0
    z = cmplx(x, y)
    
    call cshch(z, csh_modern, cch_modern)
    call cshch_f77(z, csh_f77, cch_f77)
    
    print *, "Test case: z = 100 + 0i"
    print *, "Modern: sinh =", real(csh_modern), aimag(csh_modern)
    print *, "        cosh =", real(cch_modern), aimag(cch_modern)
    print *, "F77:    sinh =", real(csh_f77), aimag(csh_f77)
    print *, "        cosh =", real(cch_f77), aimag(cch_f77)
    
contains
    
    subroutine cshch_f77(z, csh, cch)
        ! Interface to F77 CSHCH
        complex :: z, csh, cch
        real :: zr, zi, cshr, cshi, cchr, cchi
        
        zr = real(z)
        zi = aimag(z)
        call cshch(zr, zi, cshr, cshi, cchr, cchi)
        csh = cmplx(cshr, cshi)
        cch = cmplx(cchr, cchi)
    end subroutine cshch_f77

end program test_cshch_comparison
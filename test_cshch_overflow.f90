program test_cshch_overflow
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    
    print *, "Testing overflow protection in CSHCH..."
    
    ! Test with x = 100
    z = cmplx(100.0, 0.0)
    print *, "Input: z = ", z
    
    call cshch(z, csh, cch)
    
    print *, "Output: csh = ", csh
    print *, "Output: cch = ", cch
    
    ! Check if values are finite
    print *, ""
    print *, "Is csh finite? real:", .not. (real(csh) /= real(csh)), " imag:", .not. (aimag(csh) /= aimag(csh))
    print *, "Is cch finite? real:", .not. (real(cch) /= real(cch)), " imag:", .not. (aimag(cch) /= aimag(cch))
    
end program test_cshch_overflow
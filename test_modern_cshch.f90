program test_modern_cshch
    use cshch_module, only: cshch
    implicit none
    
    complex :: z, csh, cch
    real :: pi
    
    pi = 3.14159265358979
    
    ! Test case 1: z = 0 + i*pi/2
    z = cmplx(0.0, pi/2.0)
    call cshch(z, csh, cch)
    write(*,*) 'Test 1: z = 0 + i*pi/2'
    write(*,*) 'sinh real:', real(csh), ' imag:', aimag(csh)
    write(*,*) 'cosh real:', real(cch), ' imag:', aimag(cch)
    write(*,*)
    
    ! Test case 2: z = 100 + 0i
    z = cmplx(100.0, 0.0)
    call cshch(z, csh, cch)
    write(*,*) 'Test 2: z = 100 + 0i'
    write(*,*) 'sinh real:', real(csh), ' imag:', aimag(csh)
    write(*,*) 'cosh real:', real(cch), ' imag:', aimag(cch)
    
end program test_modern_cshch
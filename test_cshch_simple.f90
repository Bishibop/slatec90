program test_cshch_simple
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    real :: x, y
    integer :: test_id
    
    ! Test 1: z = 0.0 + 0i
    x = 0.0
    y = 0.0
    z = cmplx(x, y)
    test_id = 1
    
    write(*,*) 'Testing z = ', z
    call cshch(z, csh, cch)
    write(*,*) 'sinh(z) = ', csh
    write(*,*) 'cosh(z) = ', cch
    write(*,*)
    
    ! Test 2: z = 1.0 + 0i  
    x = 1.0
    y = 0.0
    z = cmplx(x, y)
    test_id = 2
    
    write(*,*) 'Testing z = ', z
    call cshch(z, csh, cch)
    write(*,*) 'sinh(z) = ', csh
    write(*,*) 'cosh(z) = ', cch
    write(*,*)
    
    ! Test 3: z = 0.0 + 1i
    x = 0.0
    y = 1.0
    z = cmplx(x, y)
    test_id = 3
    
    write(*,*) 'Testing z = ', z
    call cshch(z, csh, cch)
    write(*,*) 'sinh(z) = ', csh
    write(*,*) 'cosh(z) = ', cch
    write(*,*)
    
    ! Test 4: z = 1.0 + 1i
    x = 1.0
    y = 1.0
    z = cmplx(x, y)
    test_id = 4
    
    write(*,*) 'Testing z = ', z
    call cshch(z, csh, cch)
    write(*,*) 'sinh(z) = ', csh
    write(*,*) 'cosh(z) = ', cch
    write(*,*)
    
    ! Test overflow protection: z = 90.0 + 0i
    x = 90.0
    y = 0.0
    z = cmplx(x, y)
    test_id = 5
    
    write(*,*) 'Testing overflow case z = ', z
    call cshch(z, csh, cch)
    write(*,*) 'sinh(z) = ', csh
    write(*,*) 'cosh(z) = ', cch
    
end program test_cshch_simple
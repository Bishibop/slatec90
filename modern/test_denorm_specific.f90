program test_denorm_specific
    use denorm_module
    implicit none
    
    double precision :: x(10), result
    integer :: i
    
    ! Test case 1: N=0 (empty vector)
    result = denorm(0, x)
    print *, 'Test N=0: result =', result
    
    ! Test case 2: N=1 with value 5.0
    x(1) = 5.0D0
    result = denorm(1, x)
    print *, 'Test N=1, x(1)=5.0: result =', result
    
    ! Test case 3: N=1 with value -5.0
    x(1) = -5.0D0
    result = denorm(1, x)
    print *, 'Test N=1, x(1)=-5.0: result =', result
    
    ! Test case 4: Very small value
    x(1) = 1.0D-20
    result = denorm(1, x)
    print *, 'Test N=1, x(1)=1e-20: result =', result
    
    ! Test case 5: Very large value
    x(1) = 1.0D19
    result = denorm(1, x)
    print *, 'Test N=1, x(1)=1e19: result =', result
    
    ! Test case 6: Two values - 3,4
    x(1) = 3.0D0
    x(2) = 4.0D0
    result = denorm(2, x)
    print *, 'Test N=2, x=[3,4]: result =', result
    
    ! Test case 7: Mixed magnitudes
    x(1) = 1.0D-20
    x(2) = 1.0D0
    x(3) = 1.0D19
    result = denorm(3, x)
    print *, 'Test N=3, mixed magnitudes: result =', result
    
end program test_denorm_specific
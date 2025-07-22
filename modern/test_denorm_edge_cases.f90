program test_denorm_edge_cases
    use denorm_module
    implicit none
    
    double precision :: x(10), result
    integer :: i
    
    write(*, '(A)') 'DENORM Edge Case Tests'
    write(*, '(A)') '====================='
    write(*, *)
    
    ! Test 1: Empty vector
    write(*, '(A)') 'Test 1: Empty vector (n=0)'
    result = denorm(0, x)
    write(*, '(A,G0.15)') 'Result: ', result
    write(*, *)
    
    ! Test 2: Single zero
    write(*, '(A)') 'Test 2: Single zero'
    x(1) = 0.0D0
    result = denorm(1, x)
    write(*, '(A,G0.15)') 'Result: ', result
    write(*, *)
    
    ! Test 3: Very small values (below RDWARF)
    write(*, '(A)') 'Test 3: Very small values (below RDWARF)'
    x(1) = 1.0D-20
    x(2) = 2.0D-20
    x(3) = 3.0D-20
    result = denorm(3, x)
    write(*, '(A,G0.15)') 'Result: ', result
    write(*, *)
    
    ! Test 4: Values at RDWARF boundary
    write(*, '(A)') 'Test 4: Values at RDWARF boundary'
    x(1) = 3.834D-20
    x(2) = 3.834D-20
    result = denorm(2, x)
    write(*, '(A,G0.15)') 'Result: ', result
    write(*, *)
    
    ! Test 5: Mix of small and intermediate values
    write(*, '(A)') 'Test 5: Mix of small and intermediate values'
    x(1) = 1.0D-20
    x(2) = 1.0D0
    x(3) = 2.0D0
    result = denorm(3, x)
    write(*, '(A,G0.15)') 'Result: ', result
    write(*, *)
    
    ! Test 6: Large values approaching RGIANT
    write(*, '(A)') 'Test 6: Large values approaching RGIANT'
    x(1) = 1.0D19
    x(2) = 1.0D19
    result = denorm(2, x)
    write(*, '(A,G0.15)') 'Result: ', result
    write(*, *)
    
    ! Test 7: Values at AGIANT boundary for n=10
    write(*, '(A)') 'Test 7: Values at AGIANT boundary (n=10)'
    do i = 1, 10
        x(i) = 1.304D19 / 10.0D0  ! RGIANT/n
    end do
    result = denorm(10, x)
    write(*, '(A,G0.15)') 'Result: ', result
    write(*, *)
    
    ! Test 8: Mix of all three ranges
    write(*, '(A)') 'Test 8: Mix of all three ranges'
    x(1) = 1.0D-20     ! Small
    x(2) = 1.0D0       ! Intermediate
    x(3) = 1.0D19      ! Large
    result = denorm(3, x)
    write(*, '(A,G0.15)') 'Result: ', result
    write(*, *)
    
end program test_denorm_edge_cases
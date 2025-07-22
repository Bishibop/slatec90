program test_denorm_validation
    use denorm_module
    implicit none
    
    double precision :: x(100), result
    double precision :: rdwarf, rgiant
    integer :: i
    
    ! Machine constants
    rdwarf = 3.834D-20
    rgiant = 1.304D19
    
    print *, '=== DENORM Validation Tests ==='
    print *, ''
    
    ! Test 1: N=0 case (should return 0.0, not 5.0)
    print *, 'Test 1: N=0 (empty vector)'
    result = denorm(0, x)
    print *, '  Result:', result
    print *, '  Expected: 0.0'
    print *, ''
    
    ! Test 2: Single element tests
    print *, 'Test 2a: N=1, x(1) = 5.0'
    x(1) = 5.0D0
    result = denorm(1, x)
    print *, '  Result:', result
    print *, '  Expected: 5.0'
    print *, ''
    
    print *, 'Test 2b: N=1, x(1) = -7.0'
    x(1) = -7.0D0
    result = denorm(1, x)
    print *, '  Result:', result
    print *, '  Expected: 7.0'
    print *, ''
    
    ! Test 3: Very small values (< RDWARF)
    print *, 'Test 3: Very small values'
    print *, '  RDWARF =', rdwarf
    
    print *, '  3a: x(1) = RDWARF/2'
    x(1) = rdwarf / 2.0D0
    result = denorm(1, x)
    print *, '    Result:', result
    print *, '    x(1):', x(1)
    print *, ''
    
    print *, '  3b: x(1) = RDWARF/10'
    x(1) = rdwarf / 10.0D0
    result = denorm(1, x)
    print *, '    Result:', result
    print *, '    x(1):', x(1)
    print *, ''
    
    ! Test 4: Very large values (> RGIANT)
    print *, 'Test 4: Very large values'
    print *, '  RGIANT =', rgiant
    
    print *, '  4a: x(1) = RGIANT/sqrt(N) for N=1'
    x(1) = rgiant
    result = denorm(1, x)
    print *, '    Result:', result
    print *, '    x(1):', x(1)
    print *, ''
    
    print *, '  4b: x(1) = RGIANT*2'
    x(1) = rgiant * 2.0D0
    result = denorm(1, x)
    print *, '    Result:', result
    print *, '    x(1):', x(1)
    print *, ''
    
    ! Test 5: Mixed magnitude vectors
    print *, 'Test 5: Mixed magnitude vectors'
    x(1) = 1.0D-20
    x(2) = 1.0D0
    x(3) = 1.0D19
    result = denorm(3, x)
    print *, '  Result:', result
    print *, '  Dominant term: 1.0D19'
    print *, ''
    
    ! Test 6: Three-sum boundaries
    print *, 'Test 6: Testing three-sum accumulation'
    
    ! All small
    print *, '  6a: All small components'
    do i = 1, 5
        x(i) = rdwarf / 2.0D0
    end do
    result = denorm(5, x)
    print *, '    Result:', result
    print *, ''
    
    ! All intermediate
    print *, '  6b: All intermediate components'
    do i = 1, 5
        x(i) = 1.0D0
    end do
    result = denorm(5, x)
    print *, '    Result:', result
    print *, '    Expected: sqrt(5) =', sqrt(5.0D0)
    print *, ''
    
    ! All large
    print *, '  6c: All large components'
    do i = 1, 3
        x(i) = rgiant / 2.0D0
    end do
    result = denorm(3, x)
    print *, '    Result:', result
    print *, ''
    
end program test_denorm_validation
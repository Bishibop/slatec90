program debug_denorm_cases
    use denorm_module
    implicit none
    
    double precision :: x(10), result, expected
    integer :: i
    
    print *, "Testing specific DENORM cases with known issues:"
    print *, "================================================"
    
    ! Test case 1: Simple 3-4-5 triangle (actual=5.0, expected=50.0???)
    print *, ""
    print *, "Test 1: Simple 3-4-5 triangle"
    x(1) = 3.0d0
    x(2) = 4.0d0
    result = denorm(2, x)
    expected = sqrt(3.0d0**2 + 4.0d0**2)
    print *, "  Input: [3.0, 4.0]"
    print *, "  Result:", result
    print *, "  Expected (calculated):", expected
    print *, "  Direct calculation:", sqrt(9.0d0 + 16.0d0)
    
    ! Test case with N=1 
    print *, ""
    print *, "Test 2: N=1 case"
    x(1) = 50.0d0
    result = denorm(1, x)
    print *, "  Input: [50.0]"
    print *, "  Result:", result
    print *, "  Expected:", abs(50.0d0)
    
    ! Test with very small values
    print *, ""
    print *, "Test 3: Small values (RDWARF test)"
    x(1) = 3.834d-20  ! RDWARF
    result = denorm(1, x)
    print *, "  Input: [3.834e-20]"
    print *, "  Result:", result
    print *, "  Expected:", 3.834d-20
    
    ! Test with large values
    print *, ""
    print *, "Test 4: Large values (RGIANT test)"
    x(1) = 1.304d19  ! RGIANT
    result = denorm(1, x)
    print *, "  Input: [1.304e19]"
    print *, "  Result:", result
    print *, "  Expected:", 1.304d19
    
    ! Let's trace through the algorithm for case 1
    print *, ""
    print *, "Detailed trace for Test 1:"
    print *, "=========================="
    x(1) = 3.0d0
    x(2) = 4.0d0
    
    ! Manual calculation
    block
        double precision :: s1, s2, s3, x1max, x3max, xabs, agiant
        double precision :: RDWARF, RGIANT
    RDWARF = 3.834d-20
    RGIANT = 1.304d19
    
    s1 = 0.0d0
    s2 = 0.0d0
    s3 = 0.0d0
    x1max = 0.0d0
    x3max = 0.0d0
    agiant = RGIANT / 2.0d0
    
    print *, "  RDWARF =", RDWARF
    print *, "  RGIANT =", RGIANT
    print *, "  agiant =", agiant
    
    do i = 1, 2
        xabs = abs(x(i))
        print *, "  Element", i, ": x =", x(i), ", xabs =", xabs
        
        if (xabs > RDWARF .and. xabs < agiant) then
            print *, "    -> Intermediate component"
            s2 = s2 + xabs**2
            print *, "    s2 now =", s2
        else if (xabs <= RDWARF) then
            print *, "    -> Small component"
        else
            print *, "    -> Large component"
        end if
    end do
    
    print *, "  Final sums: s1 =", s1, ", s2 =", s2, ", s3 =", s3
    print *, "  Final maxima: x1max =", x1max, ", x3max =", x3max
    
    if (s2 /= 0.0d0) then
        result = sqrt(s2)
        print *, "  Result = sqrt(s2) =", result
    end if
    end block
    
end program debug_denorm_cases
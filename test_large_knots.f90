program test_large_knots
    use bsplvn_module
    implicit none
    
    ! Test 42: Quadratic B-spline with VERY large knot values
    real :: t(10) = [1.0e10, 2.0e10, 3.0e10, 4.0e10, 5.0e10, 0.0, 0.0, 0.0, 0.0, 0.0]
    integer :: jhigh = 3
    real :: x = 2.5e10
    integer :: ileft = 3
    real :: vnikx(10)
    integer :: i
    real :: sum_check
    
    print *, "Test 42: Quadratic B-spline with very large knot values"
    print *, "Knots (in scientific notation):"
    do i = 1, 5
        print *, "  t(", i, ") =", t(i)
    end do
    print *, "jhigh =", jhigh
    print *, "x =", x
    print *, "ileft =", ileft
    
    ! Initialize
    vnikx = 0.0
    
    ! Call BSPLVN
    call bsplvn(t, jhigh, 1, x, ileft, vnikx)
    
    print *, ""
    print *, "Results:"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    ! Check sum
    sum_check = sum(vnikx(1:jhigh))
    print *, ""
    print *, "Sum of values:", sum_check
    
    print *, ""
    print *, "Expected values:"
    print *, "  [1.0, -1.250555215e-10, 2.500024891e-16]"
    
    ! The expected values themselves violate B-spline properties!
    print *, ""
    print *, "NOTE: The expected values include a negative number,"
    print *, "which suggests numerical precision issues in F77 implementation"
    
end program test_large_knots
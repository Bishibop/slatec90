program test_double_knot
    use bsplvn_module
    implicit none
    
    ! Test 12: Quadratic B-spline, double knot [0,1,1,2,3], x=1.0
    real :: t(10) = [0.0, 1.0, 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    integer :: jhigh = 3
    real :: x = 1.0
    integer :: ileft = 2
    real :: vnikx(10)
    integer :: i, expected_length, num_calls
    
    print *, "Test 12: Quadratic B-spline with double knot at x=1.0"
    print *, "Knots:", t(1:5)
    print *, "jhigh =", jhigh
    print *, "x =", x
    print *, "ileft =", ileft
    
    ! Expected 4 values, so need INDEX=2 call
    expected_length = 4
    num_calls = expected_length - jhigh  ! 4 - 3 = 1
    
    ! Initialize
    vnikx = 0.0
    
    ! First call with INDEX=1
    print *, ""
    print *, "Calling with INDEX=1..."
    call bsplvn(t, jhigh, 1, x, ileft, vnikx)
    
    print *, "After INDEX=1:"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    ! Call with INDEX=2
    print *, ""
    print *, "Calling with INDEX=2..."
    call bsplvn(t, jhigh, 2, x, ileft, vnikx)
    
    print *, "After INDEX=2:"
    do i = 1, expected_length
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    print *, ""
    print *, "Expected: [0.125, 0.375, 0.375, 0.125]"
    
end program test_double_knot
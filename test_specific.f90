program test_specific
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(20), vnikx(10), x
    integer :: jhigh, index_val, ileft, i
    
    ! Test 30: x=8.9, ileft=8, jhigh=3
    print *, "=== Test 30 Debug ==="
    
    ! Set up knots
    do i = 1, 10
        t(i) = real(i-1)
    end do
    
    x = 8.9
    ileft = 8
    jhigh = 3
    index_val = 1
    vnikx = 0.0
    
    print *, "Input:"
    print *, "  x =", x
    print *, "  ileft =", ileft, "(0-based)"
    print *, "  jhigh =", jhigh
    print *, "  Knots: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9"
    print *, ""
    print *, "Expected: x is in interval [8, 9)"
    print *, "For cubic B-splines (jhigh=3), we expect 4 non-zero values"
    print *, ""
    
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    print *, "Results:"
    do i = 1, jhigh+1
        print '(A,I1,A,ES20.13)', "  vnikx(", i, ") = ", vnikx(i)
    end do
    
    ! Check sum
    print *, ""
    print *, "Sum of B-spline values:", sum(vnikx(1:jhigh+1))
    print *, "(Should be 1.0 for partition of unity)"
    
end program test_specific
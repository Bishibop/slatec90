program test_manual_bsplvn
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(10), vnikx(10), x
    integer :: jhigh, index_val, ileft, i
    
    print *, "=== Manual B-spline Test ==="
    print *, ""
    
    ! Set up simple uniform knots: 0, 1, 2, 3, 4
    do i = 1, 5
        t(i) = real(i-1)
    end do
    
    ! Test 1: Linear B-spline (K=2) at x=0.5
    print *, "Test 1: Linear B-spline (K=2)"
    x = 0.5
    ileft = 0  ! x is in interval [0,1)
    jhigh = 2
    index_val = 1
    vnikx = 0.0
    
    print *, "x =", x, "in interval [", t(ileft+1), ",", t(ileft+2), ")"
    print *, "Expected values:"
    print *, "  B[0,2](0.5) = (1-0.5)/(1-0) = 0.5"
    print *, "  B[1,2](0.5) = (0.5-0)/(1-0) = 0.5"
    
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    print *, "Actual values:"
    print *, "  vnikx(1) =", vnikx(1), "(should be 0.5)"
    print *, "  vnikx(2) =", vnikx(2), "(should be 0.5)"
    print *, "  Sum =", vnikx(1) + vnikx(2)
    
    ! Test 2: Quadratic B-spline (K=3) at x=1.5
    print *, ""
    print *, "Test 2: Quadratic B-spline (K=3)"
    x = 1.5
    ileft = 1  ! x is in interval [1,2)
    jhigh = 3
    index_val = 1
    vnikx = 0.0
    
    print *, "x =", x, "in interval [", t(ileft+1), ",", t(ileft+2), ")"
    
    ! For quadratic B-splines, we expect 3 non-zero values
    ! Manual calculation is more complex due to recursion
    
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    print *, "Actual values:"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    print *, "  Sum =", sum(vnikx(1:jhigh))
    
end program test_manual_bsplvn
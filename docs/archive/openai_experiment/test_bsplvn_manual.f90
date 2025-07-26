program test_bsplvn_manual
    use BSplineModule, only: BSPLVN
    implicit none
    
    ! Test case: Linear B-spline, uniform knots [0,1,2,3], x=0.5
    real :: t(4), vnikx(10), x
    integer :: jhigh, index, ileft
    integer :: i
    
    ! Initialize knot vector
    t = [0.0, 1.0, 2.0, 3.0]
    
    ! Test parameters
    jhigh = 2    ! Order 2 (linear B-splines)
    x = 0.5
    ileft = 1    ! x is between t(1) and t(2)
    
    ! Initialize vnikx
    vnikx = 0.0
    
    ! First call with index=1
    index = 1
    print *, "Calling BSPLVN with index=1"
    call BSPLVN(t, jhigh, index, x, ileft, vnikx)
    
    print *, "After index=1 call:"
    do i = 1, jhigh
        print *, "vnikx(", i, ") = ", vnikx(i)
    end do
    
    ! For comparison with expected [0.5, 0.5]
    print *, ""
    print *, "Expected values: [0.5, 0.5]"
    
end program test_bsplvn_manual
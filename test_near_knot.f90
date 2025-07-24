program test_near_knot
    use bsplvn_module
    implicit none
    
    real :: t(10) = [0.0, 1.0, 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    integer :: jhigh = 3
    real :: x
    integer :: ileft = 2
    real :: vnikx(10)
    integer :: i
    
    print *, "Testing near double knot with jhigh=3, ileft=2"
    print *, "Knots:", t(1:5)
    
    ! Test at x = 0.999 (just before knot)
    x = 0.999
    vnikx = 0.0
    call bsplvn(t, jhigh, 1, x, ileft, vnikx)
    print *, ""
    print *, "x = 0.999:"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    ! Test at x = 1.0 (at knot)
    x = 1.0
    vnikx = 0.0
    call bsplvn(t, jhigh, 1, x, ileft, vnikx)
    print *, ""
    print *, "x = 1.0:"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    ! Test at x = 1.001 (just after knot)
    x = 1.001
    vnikx = 0.0
    call bsplvn(t, jhigh, 1, x, ileft, vnikx)
    print *, ""
    print *, "x = 1.001:"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    ! Try with different ileft values at x=1.0
    x = 1.0
    print *, ""
    print *, "Testing x=1.0 with different ileft values:"
    
    do ileft = 1, 3
        vnikx = 0.0
        call bsplvn(t, jhigh, 1, x, ileft, vnikx)
        print *, "ileft =", ileft, ":"
        do i = 1, jhigh
            if (vnikx(i) /= 0.0) print *, "  vnikx(", i, ") =", vnikx(i)
        end do
    end do
    
end program test_near_knot
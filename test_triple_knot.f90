program test_triple_knot
    use bsplvn_module
    implicit none
    
    ! Test 13: Cubic B-spline, triple knot [0,1,1,1,2,3], x=0.5
    real :: t(10) = [0.0, 1.0, 1.0, 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0]
    integer :: jhigh = 4
    integer :: index_val = 1  
    real :: x = 0.5
    integer :: ileft = 1
    real :: vnikx(10)
    integer :: i
    
    print *, "Test 13: Cubic B-spline with triple knot"
    print *, "Knots:", t(1:6)
    print *, "jhigh =", jhigh
    print *, "x =", x
    print *, "ileft =", ileft
    
    ! Initialize
    vnikx = 0.0
    
    ! Call BSPLVN
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    print *, "Result (all jhigh values):"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    print *, "Expected: [0.0, 1.0] (only 2 values)"
    
    ! Check which values are non-zero
    print *, ""
    print *, "Non-zero values:"
    do i = 1, jhigh
        if (abs(vnikx(i)) > 1e-10) then
            print *, "  vnikx(", i, ") =", vnikx(i)
        end if
    end do
    
end program test_triple_knot
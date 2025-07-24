program debug_bsplvn
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(10), vnikx(10), x
    integer :: jhigh, index_val, ileft, i
    
    ! Simple test case: uniform knots [0,1,2,3,4,5,6,7,8,9]
    do i = 1, 10
        t(i) = real(i-1)
    end do
    
    print *, "=== DEBUG: Linear B-spline (jhigh=2) ==="
    print *, "Knots t = [0,1,2,3,4,5,6,7,8,9]"
    print *, ""
    
    ! Test 1: x = 0.5, should be in interval [0,1)
    x = 0.5
    ileft = 0  ! 0-based index for interval [0,1)
    jhigh = 2
    index_val = 1
    vnikx = 0.0
    
    print *, "Test 1: x =", x, "ileft =", ileft
    print *, "Expected: x is in interval [t(", ileft, "), t(", ileft+1, ")) = [0, 1)"
    
    ! Try with ileft = 0 (as given in test data)
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    print *, "With ileft=0:"
    print *, "  vnikx(1) =", vnikx(1), "(should be 0.5)"
    print *, "  vnikx(2) =", vnikx(2), "(should be 0.5)"
    print *, "  vnikx(3) =", vnikx(3), "(should be 0.0)"
    
    ! Try with ileft = 1 (if F77 expects 1-based)
    vnikx = 0.0
    ileft = 1
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    print *, "With ileft=1:"
    print *, "  vnikx(1) =", vnikx(1)
    print *, "  vnikx(2) =", vnikx(2)
    print *, "  vnikx(3) =", vnikx(3)
    
    print *, ""
    print *, "=== Let's trace the algorithm ==="
    
    ! For linear B-splines (jhigh=2), with x=0.5, ileft=0
    print *, "Initial: j=1, vnikx(1)=1.0"
    print *, "Since j < jhigh (1 < 2), continue to computation"
    print *, ""
    print *, "First iteration (j=1):"
    print *, "  ipj = ileft + j = 0 + 1 = 1"
    print *, "  deltap(1) = t(1) - x = t(1) - 0.5"
    print *, "  imjp1 = ileft - j + 1 = 0 - 1 + 1 = 0"
    print *, "  deltam(1) = x - t(0)"
    print *, "  But t(0) is invalid in Fortran!"
    
end program debug_bsplvn
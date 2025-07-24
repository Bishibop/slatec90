program test_case_11
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(20), vnikx(10), x
    integer :: jhigh, index_val, ileft, i
    
    print *, "=== Debugging Test Case 11 ==="
    
    ! Set up knots from test 11
    do i = 1, 10
        t(i) = real(i-1)  ! 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
    end do
    
    ! Test 11 parameters
    x = 0.0
    ileft = 0
    jhigh = 2
    index_val = 1
    vnikx = -999.0  ! Initialize with sentinel value
    
    print *, "Input parameters:"
    print *, "  x =", x
    print *, "  ileft =", ileft
    print *, "  jhigh =", jhigh
    print *, "  First few knots:", (t(i), i=1,5)
    
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    print *, ""
    print *, "Output:"
    do i = 1, jhigh + 1
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    ! Try with ileft = 1 to see if that changes things
    print *, ""
    print *, "Trying with ileft = 1:"
    ileft = 1
    vnikx = -999.0
    
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    do i = 1, jhigh + 1
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
end program test_case_11
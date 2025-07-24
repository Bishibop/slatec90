program test_validation
    use bsplvn_module
    implicit none
    
    ! Test 42: Quadratic B-spline with large knot values
    real :: t(10) = [0.0, 100.0, 200.0, 300.0, 400.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    integer :: jhigh = 3
    real :: x = 250.0
    integer :: ileft = 3
    real :: vnikx(10)
    integer :: i
    real :: sum_check
    logical :: valid
    
    print *, "Test 42: Quadratic B-spline with large knot values"
    print *, "Knots:", t(1:5)
    print *, "jhigh =", jhigh
    print *, "x =", x
    print *, "ileft =", ileft
    
    ! Initialize
    vnikx = 0.0
    
    ! Call BSPLVN
    call bsplvn(t, jhigh, 1, x, ileft, vnikx)
    
    print *, ""
    print *, "Results:"
    valid = .true.
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
        
        ! Check validity
        if (vnikx(i) < 0.0) then
            print *, "    ERROR: NEGATIVE VALUE!"
            valid = .false.
        end if
        if (vnikx(i) > 1.0) then
            print *, "    ERROR: VALUE > 1.0!"
            valid = .false.
        end if
    end do
    
    ! Check partition of unity
    sum_check = sum(vnikx(1:jhigh))
    print *, ""
    print *, "Sum of values:", sum_check
    if (abs(sum_check - 1.0) > 1e-6) then
        print *, "ERROR: Partition of unity violated! Sum should be 1.0"
        valid = .false.
    end if
    
    if (.not. valid) then
        print *, ""
        print *, "CRITICAL: B-spline mathematical properties violated!"
    end if
    
end program test_validation
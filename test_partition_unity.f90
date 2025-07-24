program test_partition_unity
    use bsplvn_module
    implicit none
    
    ! Test 20: Order 2 partition of unity test at x=0.5
    real :: t(10) = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 0.0]
    integer :: jhigh = 2
    integer :: index_val = 1  
    real :: x = 0.5
    integer :: ileft = 1
    real :: vnikx(10)
    integer :: i
    real :: sum_vals
    
    print *, "Test 20: Order 2 partition of unity"
    print *, "Knots:", t(1:9)
    print *, "jhigh =", jhigh
    print *, "x =", x
    print *, "ileft =", ileft
    
    ! Initialize
    vnikx = 0.0
    
    ! Call BSPLVN
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    print *, "Result (first 5 values):"
    sum_vals = 0.0
    do i = 1, 5
        print *, "  vnikx(", i, ") =", vnikx(i)
        sum_vals = sum_vals + vnikx(i)
    end do
    print *, "Sum of first", jhigh, "values:", sum(vnikx(1:jhigh))
    print *, "Sum of first 3 values:", sum(vnikx(1:3))
    print *, "Expected 3 values: [0.25, 0.625, 0.125]"
    
end program test_partition_unity
program test_bsplvn_reference
    use bsplvn_module
    implicit none
    
    ! Test using exact inputs from reference test data
    ! Test 1: Linear B-spline, uniform knots [0,1,2,3], x=0.5
    ! Inputs: [[0.0, 1.0, 2.0, 3.0], 2, 1, 0.5, 1]
    real :: t(10) = [0.0, 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    integer :: jhigh = 2     ! inputs[1]
    integer :: index_val = 1 ! inputs[2]  
    real :: x = 0.5         ! inputs[3]
    integer :: ileft = 1    ! inputs[4]
    real :: vnikx(10)
    integer :: i
    
    print *, "Reference Test 1: Linear B-spline"
    print *, "Knots:", t(1:4)
    print *, "jhigh =", jhigh
    print *, "index =", index_val
    print *, "x =", x
    print *, "ileft =", ileft
    
    ! Initialize
    vnikx = 0.0
    
    ! Call BSPLVN
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    print *, "Result:"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    print *, "Expected: [0.5, 0.5]"
    
    ! Test the third reference test case which we know works
    print *, ""
    print *, "Reference Test 3: Quadratic B-spline"
    t = [0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    jhigh = 3
    index_val = 1
    x = 1.5
    ileft = 2
    
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    
    print *, "Result:"
    do i = 1, jhigh
        print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    print *, "Expected: [0.125, 0.75, 0.125]"
    
end program test_bsplvn_reference
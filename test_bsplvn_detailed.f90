program test_bsplvn_detailed
    use bsplvn_module
    implicit none
    
    ! Test 1: Linear B-spline, uniform knots [0,1,2,3], x=0.5
    real :: t1(10) = [0.0, 1.0, 2.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    real :: x1 = 0.5
    real :: vnikx1(10)
    integer :: ileft1 = 1  ! From test data
    integer :: jhigh1 = 2  ! Order 2
    
    ! Test 3: Quadratic B-spline, uniform knots [0,1,2,3,4], x=1.5  
    real :: t3(10) = [0.0, 1.0, 2.0, 3.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.0]
    real :: x3 = 1.5
    real :: vnikx3(10)
    integer :: ileft3 = 2  ! From test data
    integer :: jhigh3 = 3  ! Order 3
    
    integer :: i
    
    print *, "===== Test 1: Linear B-spline ====="
    print *, "Knots:", t1(1:4)
    print *, "x =", x1
    print *, "ileft =", ileft1
    print *, "jhigh =", jhigh1
    
    ! Initialize output
    vnikx1 = 0.0
    
    ! Call with INDEX=1
    call bsplvn(t1, jhigh1, 1, x1, ileft1, vnikx1)
    
    print *, "Result (INDEX=1):"
    do i = 1, jhigh1
        print *, "  vnikx(", i, ") =", vnikx1(i)
    end do
    print *, "Expected: [0.5, 0.5]"
    
    print *, ""
    print *, "===== Test 3: Quadratic B-spline ====="
    print *, "Knots:", t3(1:5)
    print *, "x =", x3
    print *, "ileft =", ileft3
    print *, "jhigh =", jhigh3
    
    ! Initialize output
    vnikx3 = 0.0
    
    ! Call with INDEX=1
    call bsplvn(t3, jhigh3, 1, x3, ileft3, vnikx3)
    
    print *, "Result (INDEX=1):"
    do i = 1, jhigh3
        print *, "  vnikx(", i, ") =", vnikx3(i)
    end do
    print *, "Expected: [0.125, 0.75, 0.125]"
    
end program test_bsplvn_detailed
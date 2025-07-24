program test_fp_precision
    implicit none
    
    real :: xt(5)
    real :: x
    integer :: i
    
    ! Test 254 array
    xt = [0.0, 1e-35, 1e-30, 1e-20, 1.0]
    x = 0.0
    
    print *, "=== Floating-point comparison test ==="
    print *, "X =", x
    print *, ""
    
    do i = 1, 5
        print *, "XT(", i, ") =", xt(i)
        print *, "  X >= XT(i)?", (x >= xt(i))
        print *, "  X > XT(i)?", (x > xt(i))
        print *, "  X == XT(i)?", (x == xt(i))
        print *, "  X < XT(i)?", (x < xt(i))
        print *, ""
    end do
    
    ! Test with 1e-30
    print *, "=== Testing with X = 1e-30 ==="
    x = 1e-30
    print *, "X =", x
    do i = 1, 5
        print *, "X >= XT(", i, ")?", (x >= xt(i))
    end do
    
end program test_fp_precision
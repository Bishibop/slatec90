program test_overflow_debug
    implicit none
    
    real :: x, expx
    real, parameter :: overflow_threshold = 88.0
    
    print *, "Testing overflow threshold..."
    print *, "overflow_threshold = ", overflow_threshold
    
    x = 100.0
    print *, "x = ", x
    print *, "abs(x) = ", abs(x)
    print *, "abs(x) > overflow_threshold = ", abs(x) > overflow_threshold
    
    ! Test what happens when we compute sinh/cosh directly
    print *, ""
    print *, "Direct computation:"
    print *, "sinh(100) = ", sinh(100.0)
    print *, "cosh(100) = ", cosh(100.0)
    
    ! Test the scaled computation
    print *, ""
    print *, "Scaled computation:"
    expx = exp(abs(x) - log(2.0))
    print *, "exp(|x| - log(2)) = ", expx
    
end program test_overflow_debug
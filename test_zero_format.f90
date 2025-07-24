program test_zero_format
    implicit none
    
    real :: x1, x2, x3, x4
    
    ! Different ways zero might be represented
    x1 = 0.0
    x2 = 0.0e0
    x3 = 0.000000e+00
    x4 = 1e-20  ! What we suspect is happening
    
    print *, "Different representations of zero:"
    print '(A,E20.12)', "x1 = 0.0         -> ", x1
    print '(A,E20.12)', "x2 = 0.0e0       -> ", x2
    print '(A,E20.12)', "x3 = 0.000000e+00-> ", x3
    print '(A,E20.12)', "x4 = 1e-20       -> ", x4
    
    print *, ""
    print *, "Are they equal to 0.0?"
    print *, "x1 == 0.0?", (x1 == 0.0)
    print *, "x2 == 0.0?", (x2 == 0.0)
    print *, "x3 == 0.0?", (x3 == 0.0)
    print *, "x4 == 0.0?", (x4 == 0.0)
    
    print *, ""
    print *, "Comparisons with 1e-35:"
    print *, "x1 > 1e-35?", (x1 > 1e-35)
    print *, "x4 > 1e-35?", (x4 > 1e-35)
    
end program test_zero_format
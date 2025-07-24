program test_read_values
    use intrv_module, only: intrv
    implicit none
    
    real :: xt(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! Test 254 - set values exactly as test generator would
    lxt = 5
    xt(1) = 0.000000e+00
    xt(2) = 1.000000e-35
    xt(3) = 1.000000e-30
    xt(4) = 1.000000e-20
    xt(5) = 1.000000e+00
    x = 0.000000e+00
    ilo = 1
    
    print *, "Test 254 with exact formatting:"
    print *, "x =", x, "(should be 0.0)"
    print *, "Is x exactly 0.0?", (x == 0.0)
    print *, "Is x > 0.0?", (x > 0.0)
    print *, "Is x < 0.0?", (x < 0.0)
    
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "ILEFT =", ileft
    print *, ""
    
    ! Test 255
    x = 1.000000e-30
    ilo = 1
    print *, "Test 255:"
    print *, "x =", x
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "ILEFT =", ileft
    
end program test_read_values
program test_sequence
    use intrv_module, only: intrv
    implicit none
    
    real :: xt253(5), xt254(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! First run test 253
    xt253 = [-10000000000.0, -100000.0, 0.0, 100000.0, 10000000000.0]
    lxt = 5
    x = 0.0
    ilo = 1
    
    print *, "=== Test 253 ==="
    call intrv(xt253, lxt, x, ilo, ileft, mflag)
    print *, "Results: ILEFT=", ileft, "MFLAG=", mflag, "ILO=", ilo
    
    ! Now run test 254, but DON'T reset ILO
    xt254 = [0.0, 1e-35, 1e-30, 1e-20, 1.0]
    lxt = 5
    x = 0.0
    ! ilo is still whatever test 253 set it to
    
    print *, ""
    print *, "=== Test 254 (with carried-over ILO) ==="
    print *, "Starting ILO =", ilo
    call intrv(xt254, lxt, x, ilo, ileft, mflag)
    print *, "Results: ILEFT=", ileft, "MFLAG=", mflag, "ILO=", ilo
    
    ! Now test 255 with same array
    x = 1e-30
    print *, ""
    print *, "=== Test 255 (with carried-over ILO) ==="
    print *, "Starting ILO =", ilo
    call intrv(xt254, lxt, x, ilo, ileft, mflag)
    print *, "Results: ILEFT=", ileft, "MFLAG=", mflag, "ILO=", ilo
    
end program test_sequence
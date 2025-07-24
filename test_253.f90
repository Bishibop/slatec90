program test_253
    use intrv_module, only: intrv
    implicit none
    
    real :: xt(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! Test 253
    xt = [-10000000000.0, -100000.0, 0.0, 100000.0, 10000000000.0]
    lxt = 5
    x = 0.0
    ilo = 1
    
    print *, "Test 253:"
    print *, "Array: ", xt
    print *, "Searching for x =", x
    print *, "Initial ILO =", ilo
    
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    
    print *, "Results:"
    print *, "  ILEFT =", ileft
    print *, "  MFLAG =", mflag
    print *, "  ILO =", ilo
    
end program test_253
program test_ilo_carryover
    use intrv_debug_module, only: intrv_debug
    implicit none
    
    real :: xt(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! Test 254 with ILO=4 (carried over from previous test?)
    xt = [0.0, 1e-35, 1e-30, 1e-20, 1.0]
    lxt = 5
    x = 0.0
    ilo = 4  ! Starting with 4 instead of 1
    
    print *, "Testing with ILO=4 (carried over?):"
    call intrv_debug(xt, lxt, x, ilo, ileft, mflag, 254)
    
    print *, ""
    print *, "FINAL RESULTS:"
    print *, "  ILEFT =", ileft
    print *, "  MFLAG =", mflag
    print *, "  ILO =", ilo
    
end program test_ilo_carryover
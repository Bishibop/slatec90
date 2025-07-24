program test_intrv_debug
    use intrv_debug_module, only: intrv_debug
    implicit none
    
    real :: xt(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! Test 254
    xt = [0.0, 1e-35, 1e-30, 1e-20, 1.0]
    lxt = 5
    x = 0.0
    ilo = 1
    
    call intrv_debug(xt, lxt, x, ilo, ileft, mflag, 254)
    
    print *, ""
    print *, "FINAL RESULTS:"
    print *, "  ILEFT =", ileft
    print *, "  MFLAG =", mflag
    print *, "  ILO =", ilo
    
end program test_intrv_debug
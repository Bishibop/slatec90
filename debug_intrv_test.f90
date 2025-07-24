program debug_intrv_test
    use intrv_module, only: intrv
    implicit none
    
    real :: xt(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    integer :: i
    
    ! Test 254: Array [0.0, 1e-35, 1e-30, 1e-20, 1.0], searching for 0.0
    print *, "=== Test 254 Debug ==="
    xt = [0.0, 1e-35, 1e-30, 1e-20, 1.0]
    lxt = 5
    x = 0.0
    ilo = 1
    
    print *, "Input array XT:"
    do i = 1, lxt
        print '(A,I2,A,E15.8)', "  XT(", i, ") = ", xt(i)
    end do
    print *, "X = ", x
    print *, "ILO = ", ilo
    print *, "LXT = ", lxt
    
    ! Add debug prints to the module temporarily
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    
    print *, "Results:"
    print *, "  ILEFT = ", ileft
    print *, "  MFLAG = ", mflag
    print *, "  ILO (out) = ", ilo
    
    print *, ""
    print *, "=== Test 255 Debug ==="
    ! Test 255: Same array, searching for 1e-30
    x = 1e-30
    ilo = 1
    print *, "X = ", x
    
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    
    print *, "Results:"
    print *, "  ILEFT = ", ileft
    print *, "  MFLAG = ", mflag
    print *, "  ILO (out) = ", ilo
    
end program debug_intrv_test
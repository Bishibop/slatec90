program test_offbyone
    use intrv_module, only: intrv
    implicit none
    
    real :: xt(10)  ! Larger array to test bounds
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! Initialize larger array with test 254 values
    xt = 0.0  ! Initialize all to zero
    xt(1) = 0.0
    xt(2) = 1e-35
    xt(3) = 1e-30
    xt(4) = 1e-20
    xt(5) = 1.0
    
    ! Test if the issue is with x being slightly non-zero due to floating point
    print *, "=== Testing potential floating-point issues ==="
    
    ! Test 1: Exact zero
    lxt = 5
    x = 0.0
    ilo = 1
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "x = 0.0: ILEFT =", ileft
    
    ! Test 2: Very small positive (might round to zero)
    x = 1e-40
    ilo = 1
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "x = 1e-40: ILEFT =", ileft
    
    ! Test 3: Very small negative (might round to zero)
    x = -1e-40
    ilo = 1
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "x = -1e-40: ILEFT =", ileft
    
    ! Test 4: What if x is actually 1e-20?
    x = 1e-20
    ilo = 1
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "x = 1e-20: ILEFT =", ileft
    
end program test_offbyone
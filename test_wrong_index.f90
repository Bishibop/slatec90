program test_wrong_index
    use intrv_module, only: intrv
    implicit none
    
    real :: xt(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! Test array
    xt = [0.0, 1e-35, 1e-30, 1e-20, 1.0]
    lxt = 5
    ilo = 1
    
    print *, "=== Testing if wrong index is used for x ==="
    print *, ""
    
    ! Test 254 should use x = 0.0 (index 0)
    print *, "Test 254: Should use x = 0.0"
    x = 0.0
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "  Correct x = 0.0 -> ILEFT =", ileft
    
    ! But what if it accidentally uses ilo (=1) as index for x?
    x = xt(1)  ! This would be 0.0
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "  Using xt(1) = ", x, " -> ILEFT =", ileft
    
    ! Or what if it uses position 4?
    x = xt(4)  ! This would be 1e-20
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "  Using xt(4) = ", x, " -> ILEFT =", ileft
    
    print *, ""
    print *, "Test 255: Should use x = 1e-30"
    x = 1e-30
    ilo = 1
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "  Correct x = 1e-30 -> ILEFT =", ileft
    
    ! What if it uses xt(4)?
    x = xt(4)
    ilo = 1
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "  Using xt(4) = ", x, " -> ILEFT =", ileft
    
end program test_wrong_index
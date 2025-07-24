program test_duplicates
    use intrv_module, only: intrv
    implicit none
    
    real :: xt(7)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! Test with duplicate zeros
    print *, "=== Test with duplicate zeros ==="
    xt = [0.0, 0.0, 0.0, 1e-20, 1.0, 2.0, 3.0]
    lxt = 7
    x = 0.0
    ilo = 1
    
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "Array has 0.0 at positions 1, 2, 3"
    print *, "ILEFT =", ileft, "(should be 3 - largest index with xt(i) <= x)"
    print *, ""
    
    ! Test what happens when searching in middle of tiny values
    print *, "=== Test searching between tiny values ==="
    xt(1:5) = [0.0, 1e-35, 1e-30, 1e-20, 1.0]
    lxt = 5
    x = 5e-31  ! Between 1e-35 and 1e-30
    ilo = 1
    
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print *, "X = 5e-31 (between positions 2 and 3)"
    print *, "ILEFT =", ileft, "(should be 2)"
    
end program test_duplicates
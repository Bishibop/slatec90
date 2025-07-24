program test_exact_254
    use intrv_module, only: intrv
    implicit none
    
    real :: xt(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    integer :: i
    
    ! Exact test 254 setup
    xt(1) = 0.0e0
    xt(2) = 1.0e-35
    xt(3) = 1.0e-30
    xt(4) = 1.0e-20
    xt(5) = 1.0e0
    lxt = 5
    x = 0.0e0
    ilo = 1
    
    print *, "=== Exact Test 254 ==="
    print *, "Array XT:"
    do i = 1, lxt
        print '(A,I1,A,E20.12)', "  XT(", i, ") = ", xt(i)
    end do
    print '(A,E20.12)', "X = ", x
    print *, "Initial ILO =", ilo
    
    ! Test different starting ILO values
    do i = 1, 5
        ilo = i
        print *, ""
        print *, "Testing with ILO =", ilo
        call intrv(xt, lxt, x, ilo, ileft, mflag)
        print *, "  ILEFT =", ileft, "MFLAG =", mflag, "ILO out =", ilo
    end do
    
end program test_exact_254
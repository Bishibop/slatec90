program test_reverse_engineer
    use intrv_module, only: intrv
    implicit none
    
    real :: xt(5)
    integer :: lxt, ilo, ileft, mflag
    real :: x
    real :: test_values(5)
    integer :: i
    
    ! Array from failing tests
    xt = [0.0, 1e-35, 1e-30, 1e-20, 1.0]
    lxt = 5
    
    ! Test different x values to see which gives ILEFT=4
    test_values = [0.0, 1e-35, 1e-30, 1e-20, 5e-21]
    
    print *, "Finding which x values give ILEFT=4:"
    print *, ""
    
    do i = 1, 5
        x = test_values(i)
        ilo = 1
        call intrv(xt, lxt, x, ilo, ileft, mflag)
        print '(A,E12.5,A,I2)', "x = ", x, " -> ILEFT = ", ileft
    end do
    
    print *, ""
    print *, "Testing x values between 1e-30 and 1e-20:"
    x = 5e-25
    ilo = 1
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print '(A,E12.5,A,I2)', "x = ", x, " -> ILEFT = ", ileft
    
    x = 1e-21
    ilo = 1
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    print '(A,E12.5,A,I2)', "x = ", x, " -> ILEFT = ", ileft
    
end program test_reverse_engineer
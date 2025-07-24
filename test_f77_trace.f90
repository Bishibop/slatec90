program test_f77_trace
    ! Test to understand F77 BSPLVN behavior
    implicit none
    
    real :: t(10), x
    integer :: ileft
    
    ! Set up knots: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
    t = (/0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0/)
    
    ! Test case: x = 0.5, ileft = 0 (0-based)
    x = 0.5
    ileft = 0
    
    ! What happens when J=1?
    ! IPJ = ILEFT + J = 0 + 1 = 1
    ! IMJP1 = ILEFT - J + 1 = 0 - 1 + 1 = 0
    
    print *, "For x =", x, "ileft =", ileft
    print *, "When J = 1:"
    print *, "  IPJ = ILEFT + J =", ileft + 1
    print *, "  IMJP1 = ILEFT - J + 1 =", ileft - 1 + 1
    print *, "  This would access T(", ileft + 1, ") and T(", ileft - 1 + 1, ")"
    print *, "  T(0) is INVALID in Fortran!"
    
    ! This suggests ILEFT should be 1-based in F77
    ileft = 1  ! 1-based: x in [t(1), t(2)) = [0, 1)
    print *, ""
    print *, "With 1-based ileft =", ileft
    print *, "When J = 1:"
    print *, "  IPJ = ILEFT + J =", ileft + 1
    print *, "  IMJP1 = ILEFT - J + 1 =", ileft - 1 + 1
    print *, "  This accesses T(", ileft + 1, ") =", t(ileft + 1)
    print *, "  and T(", ileft - 1 + 1, ") =", t(ileft - 1 + 1)
    
end program test_f77_trace
program test_find_ileft
    implicit none
    
    real :: t(5) = [1.0e10, 2.0e10, 3.0e10, 4.0e10, 5.0e10]
    real :: x = 2.5e10
    integer :: i, correct_ileft
    
    print *, "Finding correct ileft for x =", x
    print *, "Knot vector:"
    do i = 1, 5
        print *, "  t(", i, ") =", t(i)
    end do
    
    ! Find correct ileft such that t(ileft) <= x < t(ileft+1)
    correct_ileft = 0
    do i = 1, 4
        if (t(i) <= x .and. x < t(i+1)) then
            correct_ileft = i
            exit
        end if
    end do
    
    print *, ""
    print *, "Correct ileft =", correct_ileft
    print *, "Because t(", correct_ileft, ") =", t(correct_ileft), "<= x <", t(correct_ileft+1), "= t(", correct_ileft+1, ")"
    
    print *, ""
    print *, "Test 42 uses ileft = 3, which gives:"
    print *, "  t(3) =", t(3), "> x =", x, "(WRONG!)"
    
    print *, ""
    print *, "This explains why deltam becomes negative and produces invalid B-spline values."
    
end program test_find_ileft
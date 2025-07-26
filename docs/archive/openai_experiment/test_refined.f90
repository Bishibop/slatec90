program test_bsplvn_refined
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(20), vnikx(20), x
    integer :: jhigh, index, ileft, i, j
    
    ! Test 1: Linear B-spline, uniform knots [0,1,2,3], x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    vnikx = 0.0
    jhigh = 2
    index = 1
    x = 0.5
    ileft = 1
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 1, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 2
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.5
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.5
    write(*,*)

    ! Test 2: Linear B-spline, uniform knots [0,1,2,3], x=1.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    vnikx = 0.0
    jhigh = 2
    index = 1
    x = 1.5
    ileft = 2
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 2, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 2
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.5
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.5
    write(*,*)

    ! Test 3: Quadratic B-spline, uniform knots [0,1,2,3,4], x=1.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    vnikx = 0.0
    jhigh = 3
    index = 1
    x = 1.5
    ileft = 2
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 3, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 3
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.125
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.75
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 3, ') = ', 0.125
    write(*,*)

    ! Test 4: Quadratic B-spline, uniform knots [0,1,2,3,4], x=2.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    vnikx = 0.0
    jhigh = 3
    index = 1
    x = 2.0
    ileft = 2
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 4, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 3
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.0
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.5
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 3, ') = ', 0.5
    write(*,*)

    ! Test 5: Cubic B-spline, uniform knots [0,1,2,3,4,5], x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    vnikx = 0.0
    jhigh = 4
    index = 1
    x = 2.5
    ileft = 3
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 5, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 4
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.02083333395
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.4791666865
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 3, ') = ', 0.4791666865
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 4, ') = ', 0.02083333395
    write(*,*)

    ! Test 6: Cubic B-spline, uniform knots [0,1,2,3,4,5], x=3.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    vnikx = 0.0
    jhigh = 4
    index = 1
    x = 3.0
    ileft = 3
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 6, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 4
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.0
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.1666666716
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 3, ') = ', 0.6666666865
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 4, ') = ', 0.1666666716
    write(*,*)

    ! Test 7: Linear B-spline, non-uniform knots [0,0.5,2,4], x=1.0
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 2.0
    t(4) = 4.0
    vnikx = 0.0
    jhigh = 2
    index = 1
    x = 1.0
    ileft = 2
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 7, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 2
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.6666666865
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.3333333433
    write(*,*)

    ! Test 8: Linear B-spline, non-uniform knots [0,0.5,2,4], x=3.0
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 2.0
    t(4) = 4.0
    vnikx = 0.0
    jhigh = 2
    index = 1
    x = 3.0
    ileft = 3
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 8, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 2
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.5
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.5
    write(*,*)

    ! Test 9: Quadratic B-spline, non-uniform knots [0,0.5,1,3,5], x=2.0
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 1.0
    t(4) = 3.0
    t(5) = 5.0
    vnikx = 0.0
    jhigh = 3
    index = 1
    x = 2.0
    ileft = 3
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 9, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 3
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.200000003
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.6750000119
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 3, ') = ', 0.125
    write(*,*)

    ! Test 10: Quadratic B-spline, double knot [0,1,1,2,3], x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 3.0
    vnikx = 0.0
    jhigh = 3
    index = 1
    x = 0.5
    ileft = 1
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', 10, ':'
    write(*,'(A)') '  Results:'
    do j = 1, 3
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 1, ') = ', 0.25
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 2, ') = ', 0.5
    write(*,'(A,I2,A,F12.8)') '    vnikx(', 3, ') = ', 0.25
    write(*,*)

end program test_bsplvn_refined
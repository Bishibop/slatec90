program test_bsplvn_reference_all
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(100), vnikx(30), x
    integer :: jhigh, index_val, ileft
    integer :: i, j
    
    ! Test 1: Linear B-spline, uniform knots [0,1,2,3], x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 3.0
    jhigh = 2
    index_val = 1
    x = 0.5
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 1, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 2: Linear B-spline, uniform knots [0,1,2,3], x=1.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 3.0
    t(6) = 3.0
    jhigh = 2
    index_val = 1
    x = 1.5
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 2, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 3: Quadratic B-spline, uniform knots [0,1,2,3,4], x=1.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 4.0
    t(7) = 4.0
    jhigh = 3
    index_val = 1
    x = 1.5
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 3, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 4: Quadratic B-spline, uniform knots [0,1,2,3,4], x=2.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 4.0
    t(7) = 4.0
    jhigh = 3
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 4, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 5: Cubic B-spline, uniform knots [0,1,2,3,4,5], x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    t(9) = 5.0
    jhigh = 4
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 5, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 6: Cubic B-spline, uniform knots [0,1,2,3,4,5], x=3.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    t(9) = 5.0
    jhigh = 4
    index_val = 1
    x = 3.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 6, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 7: Linear B-spline, non-uniform knots [0,0.5,2,4], x=1.0
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 2.0
    t(4) = 4.0
    t(5) = 4.0
    t(6) = 4.0
    jhigh = 2
    index_val = 1
    x = 1.0
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 7, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 8: Linear B-spline, non-uniform knots [0,0.5,2,4], x=3.0
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 2.0
    t(4) = 4.0
    t(5) = 4.0
    t(6) = 4.0
    t(7) = 4.0
    jhigh = 2
    index_val = 1
    x = 3.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 8, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 9: Quadratic B-spline, non-uniform knots [0,0.5,1,3,5], x=2.0
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 1.0
    t(4) = 3.0
    t(5) = 5.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    jhigh = 3
    index_val = 1
    x = 2.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 9, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 10: Quadratic B-spline, double knot [0,1,1,2,3], x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 3.0
    t(6) = 3.0
    jhigh = 3
    index_val = 1
    x = 0.5
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 10, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 12: Quadratic B-spline, double knot [0,1,1,2,3], x=1.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 3.0
    t(6) = 3.0
    t(7) = 3.0
    jhigh = 3
    index_val = 1
    x = 1.0
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 12, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 13: Cubic B-spline, triple knot [0,1,1,1,2,3], x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 1.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    jhigh = 4
    index_val = 1
    x = 0.5
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 13, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 14: Order 2 B-spline at knot t[1]=1.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    jhigh = 2
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 14, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 15: Order 2 B-spline at knot t[2]=2.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 4.0
    jhigh = 2
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 15, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 16: Order 3 B-spline at knot t[1]=1.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    jhigh = 3
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 16, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 17: Order 3 B-spline at knot t[2]=2.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    jhigh = 3
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 17, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 18: Order 4 B-spline at knot t[1]=1.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    jhigh = 4
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 18, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 19: Order 4 B-spline at knot t[2]=2.0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 6.0
    jhigh = 4
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 19, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 20: Order 2 partition of unity test at x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 2
    index_val = 1
    x = 0.5
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 20, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 21: Order 3 partition of unity test at x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 3
    index_val = 1
    x = 0.5
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 21, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 22: Order 4 partition of unity test at x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 4
    index_val = 1
    x = 0.5
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 22, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 23: Order 2 partition of unity test at x=1.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 2
    index_val = 1
    x = 1.5
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 23, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 24: Order 3 partition of unity test at x=1.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 3
    index_val = 1
    x = 1.5
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 24, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 25: Order 4 partition of unity test at x=1.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 4
    index_val = 1
    x = 1.5
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 25, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 26: Order 2 partition of unity test at x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 2
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 26, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 27: Order 3 partition of unity test at x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 3
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 27, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 28: Order 4 partition of unity test at x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 4
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 28, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 29: Order 2 partition of unity test at x=3.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 2
    index_val = 1
    x = 3.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 29, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 30: Order 3 partition of unity test at x=3.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 3
    index_val = 1
    x = 3.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 30, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 31: Order 4 partition of unity test at x=3.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    jhigh = 4
    index_val = 1
    x = 3.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 31, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 32: Order 2 partition of unity test at x=4.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    jhigh = 2
    index_val = 1
    x = 4.5
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 32, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 33: Order 3 partition of unity test at x=4.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    jhigh = 3
    index_val = 1
    x = 4.5
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 33, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 34: Order 4 partition of unity test at x=4.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    t(11) = 8.0
    jhigh = 4
    index_val = 1
    x = 4.5
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 34, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 35: Order 2 partition of unity test at x=5.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    jhigh = 2
    index_val = 1
    x = 5.5
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 35, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 36: Order 3 partition of unity test at x=5.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    t(11) = 8.0
    jhigh = 3
    index_val = 1
    x = 5.5
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 36, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 37: Order 4 partition of unity test at x=5.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    t(11) = 8.0
    t(12) = 8.0
    jhigh = 4
    index_val = 1
    x = 5.5
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 37, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 38: Order 2 partition of unity test at x=6.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    t(11) = 8.0
    jhigh = 2
    index_val = 1
    x = 6.5
    ileft = 7
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 38, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 39: Order 3 partition of unity test at x=6.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    t(11) = 8.0
    t(12) = 8.0
    jhigh = 3
    index_val = 1
    x = 6.5
    ileft = 7
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 39, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 40: Order 4 partition of unity test at x=6.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    t(11) = 8.0
    t(12) = 8.0
    t(13) = 8.0
    jhigh = 4
    index_val = 1
    x = 6.5
    ileft = 7
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 40, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 41: Cubic B-spline with tiny intervals
    t = 0.0
    t(1) = 0.0
    t(2) = 1e-10
    t(3) = 2e-10
    t(4) = 3e-10
    t(5) = 4e-10
    t(6) = 5e-10
    t(7) = 5e-10
    t(8) = 5e-10
    t(9) = 5e-10
    jhigh = 4
    index_val = 1
    x = 2.5e-10
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 41, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 42: Quadratic B-spline with large knot values
    t = 0.0
    t(1) = 10000000000.0
    t(2) = 20000000000.0
    t(3) = 30000000000.0
    t(4) = 40000000000.0
    t(5) = 50000000000.0
    t(6) = 50000000000.0
    t(7) = 50000000000.0
    t(8) = 50000000000.0
    jhigh = 3
    index_val = 1
    x = 25000000000.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 42, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 43: Quadratic B-spline with mixed scale knots
    t = 0.0
    t(1) = 0.0
    t(2) = 1e-05
    t(3) = 1.0
    t(4) = 100000.0
    t(5) = 10000000000.0
    t(6) = 10000000000.0
    t(7) = 10000000000.0
    t(8) = 10000000000.0
    jhigh = 3
    index_val = 1
    x = 0.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 43, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 44: Quartic B-spline (order 5), x=3.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 7.0
    t(10) = 7.0
    t(11) = 7.0
    jhigh = 5
    index_val = 1
    x = 3.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 44, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 45: Quintic B-spline (order 6), x=4.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 8.0
    t(11) = 8.0
    t(12) = 8.0
    t(13) = 8.0
    jhigh = 6
    index_val = 1
    x = 4.5
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 45, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 46: Cubic B-spline INDEX=2 continuation, first call
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    t(9) = 5.0
    jhigh = 4
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 46, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 47: Cubic B-spline INDEX=2 continuation, second call
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    t(9) = 5.0
    jhigh = 4
    index_val = 2
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 47, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 48: Cubic B-spline with Chebyshev knots
    t = 0.0
    t(1) = -0.9876883405951377
    t(2) = -0.8910065241883678
    t(3) = -0.7071067811865475
    t(4) = -0.45399049973954675
    t(5) = -0.1564344650402306
    t(6) = 0.15643446504023092
    t(7) = 0.15643446504023092
    t(8) = 0.15643446504023092
    t(9) = 0.15643446504023092
    jhigh = 4
    index_val = 1
    x = 0.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 48, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 49: Quadratic B-spline with exponential knots
    t = 0.0
    t(1) = 0.1353352832366127
    t(2) = 0.36787944117144233
    t(3) = 1.0
    t(4) = 2.718281828459045
    t(5) = 7.38905609893065
    t(6) = 20.085536923187668
    t(7) = 20.085536923187668
    t(8) = 20.085536923187668
    jhigh = 3
    index_val = 1
    x = 2.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 49, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 50: Cubic B-spline with zero span (triple knot)
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 1.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 3.0
    t(9) = 3.0
    t(10) = 3.0
    jhigh = 4
    index_val = 1
    x = 1.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 50, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 51: Random test 1: order=2, x=4.568
    t = 0.0
    t(1) = 0.31782679481783593
    t(2) = 0.9369523986159245
    t(3) = 1.024951761715075
    t(4) = 1.113310681656804
    t(5) = 1.395379285251439
    t(6) = 2.326608933907396
    t(7) = 2.4489185380347624
    t(8) = 5.453665337483498
    t(9) = 5.904925124490397
    t(10) = 7.406677446676758
    t(11) = 7.415504997598329
    jhigh = 2
    index_val = 1
    x = 4.568053640410348
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 51, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 52: Random test 2: order=4, x=3.768
    t = 0.0
    t(1) = 0.06498759678061017
    t(2) = 1.5547949981178155
    t(3) = 2.204406220406967
    t(4) = 3.4025051651799187
    t(5) = 5.449414806032166
    t(6) = 5.892656838759088
    t(7) = 6.498844377795232
    t(8) = 6.981393949882269
    t(9) = 8.05819251832808
    t(10) = 8.094304566778266
    t(11) = 9.572130722067811
    jhigh = 4
    index_val = 1
    x = 3.768304377498131
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 52, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 53: Random test 3: order=4, x=6.546
    t = 0.0
    t(1) = 0.9671637683346401
    t(2) = 3.785343772083535
    t(3) = 5.362280914547007
    t(4) = 5.52040631273227
    t(5) = 6.037260313668911
    t(6) = 7.297317866938179
    t(7) = 8.071282732743802
    t(8) = 8.474943663474598
    t(9) = 9.731157639793706
    t(10) = 9.731157639793706
    jhigh = 4
    index_val = 1
    x = 6.546310010484579
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 53, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 54: Random test 4: order=4, x=6.987
    t = 0.0
    t(1) = 0.6955514882374092
    t(2) = 1.626540971560848
    t(3) = 3.552707002275215
    t(4) = 3.8012622503916615
    t(5) = 4.534103023684355
    t(6) = 6.612633185677101
    t(7) = 7.730683407886919
    t(8) = 8.341104266407504
    t(9) = 8.55317721015147
    t(10) = 8.664836667552697
    t(11) = 9.852215206607578
    jhigh = 4
    index_val = 1
    x = 6.987415597124201
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 54, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 55: Random test 5: order=2, x=3.250
    t = 0.0
    t(1) = 1.634024937619284
    t(2) = 1.7113864819809699
    t(3) = 3.794554417576478
    t(4) = 5.569497437746462
    t(5) = 6.091310056669882
    t(6) = 6.399997598540929
    t(7) = 6.480353852465935
    t(8) = 6.846142509898746
    t(9) = 7.291267979503492
    t(10) = 8.428519201898096
    t(11) = 9.895233506365953
    jhigh = 2
    index_val = 1
    x = 3.249932780604988
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 55, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 56: Random test 6: order=2, x=6.659
    t = 0.0
    t(1) = 0.6618860521154724
    t(2) = 4.011647870223289
    t(3) = 5.671800514785023
    t(4) = 7.17914270718976
    t(5) = 8.050458007712143
    t(6) = 9.13136124185545
    jhigh = 2
    index_val = 1
    x = 6.6588401961184145
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 56, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 57: Random test 7: order=4, x=5.618
    t = 0.0
    t(1) = 1.396303195255063
    t(2) = 1.4287159044206266
    t(3) = 4.284338272945797
    t(4) = 5.389772896672904
    t(5) = 5.835329249456562
    t(6) = 6.428518702004242
    t(7) = 7.4498898209160656
    t(8) = 7.470138116522579
    t(9) = 8.846831538867026
    jhigh = 4
    index_val = 1
    x = 5.617591415269616
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 57, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 58: Random test 8: order=2, x=2.974
    t = 0.0
    t(1) = 0.4711637542473457
    t(2) = 0.9090941217379389
    t(3) = 1.0964913035065915
    t(4) = 4.22159966799684
    t(5) = 5.095262936764645
    t(6) = 6.2744604170309
    t(7) = 7.920793643629641
    jhigh = 2
    index_val = 1
    x = 2.9735116873267837
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 58, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 59: Random test 9: order=2, x=4.735
    t = 0.0
    t(1) = 0.11481021942819636
    t(2) = 2.668251899525428
    t(3) = 5.29114345099137
    t(4) = 5.369703304087952
    t(5) = 6.40961798579808
    t(6) = 6.817103690265748
    t(7) = 7.207218193601946
    t(8) = 8.60779702234498
    t(9) = 9.710783776136182
    t(10) = 9.961213802400968
    jhigh = 2
    index_val = 1
    x = 4.735235911314284
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 59, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 60: Random test 10: order=4, x=5.006
    t = 0.0
    t(1) = 1.7865188053013137
    t(2) = 2.6338905075109076
    t(3) = 4.537237063292064
    t(4) = 5.005861130502983
    t(5) = 8.75852940378194
    t(6) = 9.126278393448205
    t(7) = 9.538159275210802
    t(8) = 9.538159275210802
    t(9) = 9.538159275210802
    jhigh = 4
    index_val = 1
    x = 5.005861130502983
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 60, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 61: Random test 11: order=4, x=3.924
    t = 0.0
    t(1) = 0.005718961279435053
    t(2) = 0.19476742385832302
    t(3) = 1.528392685496348
    t(4) = 3.241560570046731
    t(5) = 5.303536721951775
    t(6) = 5.393790301196257
    t(7) = 6.089702114381723
    t(8) = 7.625108000751513
    t(9) = 7.7862647863055825
    t(10) = 9.29098616264617
    jhigh = 4
    index_val = 1
    x = 3.9235546430075514
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 61, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 62: Random test 12: order=3, x=6.706
    t = 0.0
    t(1) = 0.6921251846838361
    t(2) = 0.8565345206787878
    t(3) = 4.8599046331661375
    t(4) = 7.606021652572315
    t(5) = 7.658344293069877
    t(6) = 8.780095992040405
    t(7) = 9.46949445297994
    jhigh = 3
    index_val = 1
    x = 6.706177501196493
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 62, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 63: Random test 13: order=4, x=6.532
    t = 0.0
    t(1) = 1.6512695897412766
    t(2) = 3.9899229999742913
    t(3) = 5.27673787418254
    t(4) = 6.065939174826289
    t(5) = 6.71686959638283
    t(6) = 6.8986763209962145
    t(7) = 7.129489728191219
    t(8) = 7.552651966358055
    t(9) = 9.289134930021769
    t(10) = 9.643629060074552
    jhigh = 4
    index_val = 1
    x = 6.531880761502782
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 63, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 64: Random test 14: order=2, x=5.088
    t = 0.0
    t(1) = 0.0719083898509354
    t(2) = 0.2103426000705888
    t(3) = 0.588737668763234
    t(4) = 0.6402589632634492
    t(5) = 0.674003281103075
    t(6) = 2.4790561366600796
    t(7) = 4.514861413911298
    t(8) = 5.539215224244797
    t(9) = 5.88440114410351
    t(10) = 7.07840946548454
    jhigh = 2
    index_val = 1
    x = 5.087964187633519
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 64, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 65: Random test 15: order=4, x=5.274
    t = 0.0
    t(1) = 2.7847726396454697
    t(2) = 4.854144991504907
    t(3) = 5.141561198085891
    t(4) = 5.392339313947546
    t(5) = 7.233525658809708
    t(6) = 8.823830751407376
    t(7) = 8.823830751407376
    t(8) = 8.823830751407376
    jhigh = 4
    index_val = 1
    x = 5.273803764546679
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 65, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 66: Random test 16: order=2, x=2.266
    t = 0.0
    t(1) = 0.5417118723217051
    t(2) = 0.6061267407701365
    t(3) = 0.9432579649188222
    t(4) = 3.542975665895538
    t(5) = 4.070599614859366
    t(6) = 4.111022760497381
    t(7) = 4.729729429445937
    t(8) = 6.461797512949844
    t(9) = 6.534554882657916
    t(10) = 6.589827495146631
    t(11) = 7.282135884411827
    t(12) = 8.638367483696033
    jhigh = 2
    index_val = 1
    x = 2.2661586996463847
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 66, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 67: Random test 17: order=2, x=1.740
    t = 0.0
    t(1) = 0.7538488837428936
    t(2) = 1.4018229709477448
    t(3) = 1.8348803578723327
    t(4) = 4.626280684009673
    t(5) = 5.362857539103444
    t(6) = 8.080222447317025
    t(7) = 8.7448595407058
    jhigh = 2
    index_val = 1
    x = 1.7396720443571083
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 67, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 68: Random test 18: order=5, x=4.235
    t = 0.0
    t(1) = 0.5863539997217893
    t(2) = 1.6631111060391401
    t(3) = 2.1374729919918165
    t(4) = 3.789731189769161
    t(5) = 4.010402925494526
    t(6) = 4.856411254507185
    t(7) = 8.36027585079952
    t(8) = 8.486957344143054
    t(9) = 9.263669830081277
    t(10) = 9.689962572847513
    t(11) = 9.689962572847513
    jhigh = 5
    index_val = 1
    x = 4.234766921587796
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 68, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 69: Random test 19: order=4, x=2.470
    t = 0.0
    t(1) = 0.584832920072994
    t(2) = 0.6095757147651581
    t(3) = 1.8989786726763713
    t(4) = 2.1770089841417493
    t(5) = 2.852492289469203
    t(6) = 4.866714116543295
    t(7) = 6.6187131195074524
    t(8) = 6.9659150360016255
    t(9) = 7.30505317415466
    t(10) = 7.357371787890434
    t(11) = 7.833615284842872
    t(12) = 7.851293567941216
    jhigh = 4
    index_val = 1
    x = 2.47015502974547
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 69, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 70: Random test 20: order=3, x=5.217
    t = 0.0
    t(1) = 0.5687957972868452
    t(2) = 0.6796086337575724
    t(3) = 0.6852133270379235
    t(4) = 4.037755513182775
    t(5) = 4.767885171089349
    t(6) = 5.0782849699341845
    t(7) = 5.311261352705413
    t(8) = 8.513425453441215
    t(9) = 8.618192019701294
    t(10) = 9.193871308320752
    jhigh = 3
    index_val = 1
    x = 5.217114513221835
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 70, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 71: Random test 21: order=4, x=5.645
    t = 0.0
    t(1) = 2.042591994235364
    t(2) = 4.192249153358725
    t(3) = 5.227827155319589
    t(4) = 5.836722892912247
    t(5) = 6.193815103321031
    t(6) = 9.347062577364273
    t(7) = 9.347062577364273
    t(8) = 9.347062577364273
    jhigh = 4
    index_val = 1
    x = 5.645417678035624
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 71, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 72: Random test 22: order=2, x=3.331
    t = 0.0
    t(1) = 0.7254311449315731
    t(2) = 2.999970797987622
    t(3) = 3.1617719627185403
    t(4) = 3.957858467912545
    t(5) = 4.5828552261858615
    t(6) = 6.716902229599713
    t(7) = 7.518644924144021
    t(8) = 9.984544408544423
    jhigh = 2
    index_val = 1
    x = 3.331012122884094
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 72, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 73: Random test 23: order=4, x=2.850
    t = 0.0
    t(1) = 0.6879131406001926
    t(2) = 1.324565703664673
    t(3) = 2.442844165081314
    t(4) = 2.8498739121665673
    t(5) = 3.4900883496451507
    t(6) = 4.3818457300770985
    t(7) = 5.058846940487653
    t(8) = 5.058846940487653
    t(9) = 5.058846940487653
    jhigh = 4
    index_val = 1
    x = 2.8498739121665673
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 73, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 74: Random test 24: order=4, x=2.401
    t = 0.0
    t(1) = 0.07823107152157949
    t(2) = 1.0703597770941764
    t(3) = 1.154286704191022
    t(4) = 1.3429111439336772
    t(5) = 2.7234821231481634
    t(6) = 2.9937875219997787
    t(7) = 5.532236408848159
    t(8) = 6.048298270302239
    t(9) = 6.539763177107326
    t(10) = 6.633887149660773
    t(11) = 8.171041351154617
    t(12) = 9.389300039271038
    jhigh = 4
    index_val = 1
    x = 2.4009762504512913
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 74, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 75: Random test 25: order=3, x=6.661
    t = 0.0
    t(1) = 0.4408676179905002
    t(2) = 0.5079889366253065
    t(3) = 1.3081966070170226
    t(4) = 2.5112125206989413
    t(5) = 3.335565386050882
    t(6) = 5.054090580979872
    t(7) = 6.342844473091942
    t(8) = 8.293480893722192
    t(9) = 8.529128153918974
    t(10) = 9.08159046218572
    t(11) = 9.79797953828836
    jhigh = 3
    index_val = 1
    x = 6.6605742566453845
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 75, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 76: Random test 26: order=2, x=2.949
    t = 0.0
    t(1) = 0.36028138060830606
    t(2) = 1.11873880589335
    t(3) = 1.490542854785195
    t(4) = 3.6921598451219175
    t(5) = 4.2978671288077175
    t(6) = 5.525250967449976
    t(7) = 5.609068647747463
    t(8) = 6.910196237558765
    t(9) = 7.056674467599907
    t(10) = 9.45050939046639
    jhigh = 2
    index_val = 1
    x = 2.94915705496231
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 76, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 77: Random test 27: order=3, x=9.122
    t = 0.0
    t(1) = 0.3989487811121817
    t(2) = 2.3673733222935867
    t(3) = 3.5367863461461835
    t(4) = 3.5780923225042613
    t(5) = 5.598835191438697
    t(6) = 6.66933410635543
    t(7) = 6.820666883105765
    t(8) = 7.494776111435392
    t(9) = 8.747127235410911
    t(10) = 9.257638225695565
    t(11) = 9.721962650172603
    t(12) = 9.73837022637289
    t(13) = 9.73837022637289
    jhigh = 3
    index_val = 1
    x = 9.121516464466728
    ileft = 8
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 77, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 78: Random test 28: order=2, x=6.968
    t = 0.0
    t(1) = 1.5919784298073125
    t(2) = 1.7705237032038124
    t(3) = 1.79360663418207
    t(4) = 2.4816724111740074
    t(5) = 3.825417225969343
    t(6) = 4.1171316655155366
    t(7) = 4.122944103537973
    t(8) = 6.699071292881968
    t(9) = 7.0127818813991265
    t(10) = 7.35057538827876
    t(11) = 7.823864961476317
    t(12) = 9.244872952415456
    jhigh = 2
    index_val = 1
    x = 6.96788165560114
    ileft = 7
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 78, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 79: Random test 29: order=5, x=6.660
    t = 0.0
    t(1) = 1.9957283002575188
    t(2) = 2.2292354720580896
    t(3) = 3.4965842444591866
    t(4) = 6.60033182748629
    t(5) = 8.205363809715621
    t(6) = 8.713230358630081
    t(7) = 9.18451901229495
    t(8) = 9.18451901229495
    t(9) = 9.18451901229495
    jhigh = 5
    index_val = 1
    x = 6.659650641864656
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 79, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 80: Random test 30: order=2, x=1.570
    t = 0.0
    t(1) = 0.27600535119635894
    t(2) = 0.6942075466281994
    t(3) = 2.612151963902102
    t(4) = 3.3114986821440704
    t(5) = 3.5114612908275222
    t(6) = 5.0940567552803735
    t(7) = 5.805897337387078
    t(8) = 6.795181183602637
    t(9) = 7.73326637440361
    t(10) = 8.436611632720808
    t(11) = 8.770384115755602
    t(12) = 9.83550572883087
    jhigh = 2
    index_val = 1
    x = 1.5696856584773762
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 80, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 81: Random test 31: order=3, x=4.073
    t = 0.0
    t(1) = 0.4438653371323109
    t(2) = 1.1564578402933046
    t(3) = 1.9007877450229604
    t(4) = 3.456871116798003
    t(5) = 4.360575154549723
    t(6) = 4.36393883870608
    t(7) = 7.864281782036819
    t(8) = 8.995047329111996
    t(9) = 9.8423584931659
    jhigh = 3
    index_val = 1
    x = 4.072820320142942
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 81, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 82: Random test 32: order=4, x=8.269
    t = 0.0
    t(1) = 1.2462531566436774
    t(2) = 6.231554099464091
    t(3) = 6.633874762655443
    t(4) = 9.000468350970914
    t(5) = 9.207708562180402
    t(6) = 9.488744859713144
    t(7) = 9.488744859713144
    t(8) = 9.488744859713144
    jhigh = 4
    index_val = 1
    x = 8.2685172524236
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 82, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 83: Random test 33: order=5, x=5.544
    t = 0.0
    t(1) = 1.7402734320994395
    t(2) = 1.9184009299014748
    t(3) = 3.2618290485739987
    t(4) = 3.7914527712254174
    t(5) = 5.544124776667937
    t(6) = 5.691052548658093
    t(7) = 6.649294342127715
    t(8) = 6.9720841419439905
    t(9) = 7.481345601588901
    t(10) = 7.481345601588901
    t(11) = 7.481345601588901
    jhigh = 5
    index_val = 1
    x = 5.544124776667937
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 83, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 84: Random test 34: order=4, x=4.162
    t = 0.0
    t(1) = 2.101826848826419
    t(2) = 3.0387494926835377
    t(3) = 3.222492297365381
    t(4) = 4.417860168379529
    t(5) = 6.066615704797627
    t(6) = 7.857588274775801
    t(7) = 7.857588274775801
    t(8) = 7.857588274775801
    jhigh = 4
    index_val = 1
    x = 4.162408379646448
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 84, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 85: Random test 35: order=3, x=7.366
    t = 0.0
    t(1) = 0.9338720007556267
    t(2) = 2.8378640499302743
    t(3) = 6.192608053623755
    t(4) = 6.588507449984218
    t(5) = 6.638538363049987
    t(6) = 7.359614459402217
    t(7) = 7.93735399106002
    t(8) = 9.519996624524753
    t(9) = 9.598320036830362
    t(10) = 9.598320036830362
    jhigh = 3
    index_val = 1
    x = 7.36646556655308
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 85, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 86: Random test 36: order=3, x=6.803
    t = 0.0
    t(1) = 0.46213718092758427
    t(2) = 1.4735396224577268
    t(3) = 4.554161683800891
    t(4) = 6.112739730606801
    t(5) = 7.684893390001969
    t(6) = 8.065628945762658
    t(7) = 9.839360227818513
    t(8) = 9.839360227818513
    jhigh = 3
    index_val = 1
    x = 6.803110017473616
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 86, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 87: Random test 37: order=5, x=4.251
    t = 0.0
    t(1) = 0.055448181380317596
    t(2) = 1.0658729656353894
    t(3) = 1.7588668170653166
    t(4) = 2.4398443957843883
    t(5) = 4.251461939427341
    t(6) = 4.943771690104369
    t(7) = 6.560580111117841
    t(8) = 7.509644766184729
    t(9) = 7.700461885740251
    t(10) = 7.700461885740251
    t(11) = 7.700461885740251
    jhigh = 5
    index_val = 1
    x = 4.251461939427341
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 87, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 88: Random test 38: order=5, x=4.001
    t = 0.0
    t(1) = 1.213585991893844
    t(2) = 1.3333910555702888
    t(3) = 3.172979574653476
    t(4) = 4.646432374982367
    t(5) = 5.311318858555887
    t(6) = 5.589252903718673
    t(7) = 9.174700144554402
    t(8) = 9.174700144554402
    t(9) = 9.174700144554402
    jhigh = 5
    index_val = 1
    x = 4.001260329420351
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 88, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 89: Random test 39: order=5, x=6.554
    t = 0.0
    t(1) = 1.591579075429993
    t(2) = 2.773113531307405
    t(3) = 4.267407568586555
    t(4) = 4.459136120767004
    t(5) = 4.500697572469081
    t(6) = 7.517767865005115
    t(7) = 7.776748353339718
    t(8) = 8.398628878419208
    t(9) = 8.614949097832623
    t(10) = 8.920202963410182
    t(11) = 9.068547304561134
    jhigh = 5
    index_val = 1
    x = 6.553957278852696
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 89, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 90: Random test 40: order=3, x=3.796
    t = 0.0
    t(1) = 0.8057765765766844
    t(2) = 1.508306580130866
    t(3) = 2.3449357801463533
    t(4) = 3.3584753662131392
    t(5) = 3.830352924313382
    t(6) = 4.398725932649027
    t(7) = 7.1354486261105645
    t(8) = 8.930268327903168
    jhigh = 3
    index_val = 1
    x = 3.795772456898016
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 90, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 91: Random test 41: order=4, x=3.908
    t = 0.0
    t(1) = 0.6226571058358532
    t(2) = 3.3088139103808354
    t(3) = 3.8947681266694123
    t(4) = 4.148678958982181
    t(5) = 4.6592430335115065
    t(6) = 8.328911693404876
    t(7) = 8.328911693404876
    t(8) = 8.328911693404876
    jhigh = 4
    index_val = 1
    x = 3.908463560613808
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 91, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 92: Random test 42: order=3, x=5.812
    t = 0.0
    t(1) = 0.058960835839930725
    t(2) = 3.517588026718247
    t(3) = 3.8038102892771164
    t(4) = 7.5347512505938585
    t(5) = 7.6559457611806945
    t(6) = 8.567320323039343
    t(7) = 8.567320323039343
    jhigh = 3
    index_val = 1
    x = 5.811879451197276
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 92, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 93: Random test 43: order=3, x=5.398
    t = 0.0
    t(1) = 0.2902481994671524
    t(2) = 1.6504473120350882
    t(3) = 2.1942163462143616
    t(4) = 2.2053869432381887
    t(5) = 3.3612954369838244
    t(6) = 4.043166691376371
    t(7) = 4.3583597604663655
    t(8) = 4.673901492323101
    t(9) = 5.461323097338391
    t(10) = 6.032525889412414
    t(11) = 6.791418850283497
    t(12) = 6.791418850283497
    jhigh = 3
    index_val = 1
    x = 5.398339739942144
    ileft = 7
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 93, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 94: Random test 44: order=4, x=2.229
    t = 0.0
    t(1) = 0.2696645190513769
    t(2) = 0.27102046340312436
    t(3) = 0.5028463348862755
    t(4) = 1.3569948723056424
    t(5) = 2.1166028421148155
    t(6) = 3.7910386418813955
    t(7) = 3.9402025633970474
    t(8) = 4.616984440515117
    t(9) = 5.643919830247741
    t(10) = 6.427496480093358
    jhigh = 4
    index_val = 1
    x = 2.22882888383763
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 94, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 95: Random test 45: order=2, x=7.922
    t = 0.0
    t(1) = 0.5208560824325548
    t(2) = 2.242221657197482
    t(3) = 2.78256615638806
    t(4) = 4.215648267587388
    t(5) = 4.70306836213047
    t(6) = 7.490242610828507
    t(7) = 8.349109149121212
    t(8) = 9.512041198605617
    t(9) = 9.51919692381921
    jhigh = 2
    index_val = 1
    x = 7.921644947652637
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 95, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 96: Random test 46: order=4, x=2.780
    t = 0.0
    t(1) = 0.3102999888071212
    t(2) = 0.4025813754076657
    t(3) = 1.1438944610930657
    t(4) = 2.3854494188890585
    t(5) = 2.47283303732877
    t(6) = 2.562436965621866
    t(7) = 4.6503398343522075
    t(8) = 4.735610609461281
    t(9) = 6.213356565861579
    t(10) = 8.39228846284598
    t(11) = 9.477570222166518
    jhigh = 4
    index_val = 1
    x = 2.779780951831426
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 96, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 97: Random test 47: order=3, x=4.158
    t = 0.0
    t(1) = 1.6376581016719916
    t(2) = 3.1103891959859986
    t(3) = 3.7530695942531844
    t(4) = 5.757967284124142
    t(5) = 5.7868473250924355
    t(6) = 7.183572285960082
    t(7) = 7.777918861754326
    t(8) = 9.080261510474445
    t(9) = 9.288871684071216
    t(10) = 9.637347441012087
    jhigh = 3
    index_val = 1
    x = 4.1579174166800215
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 97, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 98: Random test 48: order=2, x=5.275
    t = 0.0
    t(1) = 0.4106913020613612
    t(2) = 2.428493805371321
    t(3) = 3.0158268548766696
    t(4) = 5.3274524531050025
    t(5) = 5.659325553673159
    t(6) = 6.6152134068735595
    t(7) = 6.842590391666562
    t(8) = 6.971930024957722
    t(9) = 7.96227216600212
    t(10) = 8.055279636461664
    t(11) = 8.305716337803183
    jhigh = 2
    index_val = 1
    x = 5.275426807813584
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 98, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 99: Random test 49: order=3, x=4.338
    t = 0.0
    t(1) = 0.12652150165853082
    t(2) = 1.5299114909385991
    t(3) = 3.6218803849507197
    t(4) = 4.200587408418276
    t(5) = 4.335234013258484
    t(6) = 4.597393476017463
    t(7) = 4.9020170499040825
    t(8) = 8.915750148051032
    t(9) = 8.915750148051032
    jhigh = 3
    index_val = 1
    x = 4.337707310179084
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 99, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 100: Random test 50: order=3, x=6.908
    t = 0.0
    t(1) = 0.866628980556744
    t(2) = 2.6839553804922534
    t(3) = 2.7008239638740084
    t(4) = 4.355744930038943
    t(5) = 4.834975038831962
    t(6) = 5.381729064482558
    t(7) = 7.310262143051222
    t(8) = 8.08199218806756
    t(9) = 8.307310188906033
    t(10) = 8.517131600193318
    t(11) = 8.81631184002772
    jhigh = 3
    index_val = 1
    x = 6.90835890015037
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 100, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 101: Cubic B-spline with clamped boundaries, x=0.5
    t = 0.0
    t(1) = 0.0
    t(2) = 0.0
    t(3) = 0.0
    t(4) = 1.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 3.0
    t(9) = 3.0
    jhigh = 4
    index_val = 1
    x = 0.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 101, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 102: Cubic B-spline with clamped boundaries, x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 0.0
    t(3) = 0.0
    t(4) = 1.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 3.0
    t(9) = 3.0
    t(10) = 3.0
    t(11) = 3.0
    jhigh = 4
    index_val = 1
    x = 2.5
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 102, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 103: Derivative relationship test, order=2, x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    jhigh = 2
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 103, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 104: Derivative relationship test, order=3, x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 6.0
    jhigh = 3
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 104, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 105: Derivative relationship test, order=4, x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 6.0
    t(9) = 6.0
    jhigh = 4
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 105, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 106: Derivative relationship test, order=5, x=2.5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 6.0
    t(9) = 6.0
    t(10) = 6.0
    jhigh = 5
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 106, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 107: Greville point test, order=2, i=0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 2
    index_val = 1
    x = 0.5
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 107, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 108: Greville point test, order=2, i=1
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 2
    index_val = 1
    x = 1.5
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 108, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 109: Greville point test, order=2, i=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 2
    index_val = 1
    x = 2.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 109, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 110: Greville point test, order=2, i=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 2
    index_val = 1
    x = 3.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 110, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 111: Greville point test, order=2, i=4
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 7.0
    jhigh = 2
    index_val = 1
    x = 4.5
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 111, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 112: Greville point test, order=2, i=5
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 7.0
    t(10) = 7.0
    jhigh = 2
    index_val = 1
    x = 5.5
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 112, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 113: Greville point test, order=3, i=0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 3
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 113, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 114: Greville point test, order=3, i=1
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 3
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 114, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 115: Greville point test, order=3, i=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 3
    index_val = 1
    x = 3.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 115, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 116: Greville point test, order=3, i=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 7.0
    jhigh = 3
    index_val = 1
    x = 4.0
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 116, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 117: Greville point test, order=3, i=4
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 7.0
    t(10) = 7.0
    jhigh = 3
    index_val = 1
    x = 5.0
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 117, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 118: Greville point test, order=4, i=0
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 4
    index_val = 1
    x = 1.5
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 118, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 119: Greville point test, order=4, i=1
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    jhigh = 4
    index_val = 1
    x = 2.5
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 119, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 120: Greville point test, order=4, i=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 7.0
    jhigh = 4
    index_val = 1
    x = 3.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 120, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 121: Greville point test, order=4, i=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 7.0
    t(10) = 7.0
    jhigh = 4
    index_val = 1
    x = 4.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 121, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 122: Periodic-like knot sequence, cubic spline
    t = 0.0
    t(1) = -3.141592653589793
    t(2) = -2.0943951023931953
    t(3) = -1.0471975511965976
    t(4) = 0.0
    t(5) = 1.0471975511965976
    t(6) = 2.0943951023931953
    t(7) = 3.141592653589793
    t(8) = 4.1887902047863905
    t(9) = 4.1887902047863905
    t(10) = 4.1887902047863905
    jhigh = 4
    index_val = 1
    x = 0.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 122, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 123: High order B-spline, order=10
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 9.0
    t(11) = 10.0
    t(12) = 11.0
    t(13) = 12.0
    t(14) = 13.0
    t(15) = 14.0
    t(16) = 15.0
    t(17) = 16.0
    t(18) = 17.0
    t(19) = 18.0
    t(20) = 19.0
    t(21) = 20.0
    t(22) = 21.0
    t(23) = 22.0
    t(24) = 23.0
    t(25) = 24.0
    jhigh = 10
    index_val = 1
    x = 12.5
    ileft = 13
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 123, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 124: High order B-spline, order=15
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 9.0
    t(11) = 10.0
    t(12) = 11.0
    t(13) = 12.0
    t(14) = 13.0
    t(15) = 14.0
    t(16) = 15.0
    t(17) = 16.0
    t(18) = 17.0
    t(19) = 18.0
    t(20) = 19.0
    t(21) = 20.0
    t(22) = 21.0
    t(23) = 22.0
    t(24) = 23.0
    t(25) = 24.0
    t(26) = 24.0
    t(27) = 24.0
    t(28) = 24.0
    t(29) = 24.0
    t(30) = 24.0
    jhigh = 15
    index_val = 1
    x = 12.5
    ileft = 13
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 124, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 125: High order B-spline, order=18
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 9.0
    t(11) = 10.0
    t(12) = 11.0
    t(13) = 12.0
    t(14) = 13.0
    t(15) = 14.0
    t(16) = 15.0
    t(17) = 16.0
    t(18) = 17.0
    t(19) = 18.0
    t(20) = 19.0
    t(21) = 20.0
    t(22) = 21.0
    t(23) = 22.0
    t(24) = 23.0
    t(25) = 24.0
    t(26) = 24.0
    t(27) = 24.0
    t(28) = 24.0
    t(29) = 24.0
    t(30) = 24.0
    t(31) = 24.0
    t(32) = 24.0
    t(33) = 24.0
    jhigh = 18
    index_val = 1
    x = 12.5
    ileft = 13
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 125, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 126: High order B-spline, order=19
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 9.0
    t(11) = 10.0
    t(12) = 11.0
    t(13) = 12.0
    t(14) = 13.0
    t(15) = 14.0
    t(16) = 15.0
    t(17) = 16.0
    t(18) = 17.0
    t(19) = 18.0
    t(20) = 19.0
    t(21) = 20.0
    t(22) = 21.0
    t(23) = 22.0
    t(24) = 23.0
    t(25) = 24.0
    t(26) = 24.0
    t(27) = 24.0
    t(28) = 24.0
    t(29) = 24.0
    t(30) = 24.0
    t(31) = 24.0
    t(32) = 24.0
    t(33) = 24.0
    t(34) = 24.0
    jhigh = 19
    index_val = 1
    x = 12.5
    ileft = 13
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 126, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 127: High order B-spline, order=20
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 6.0
    t(8) = 7.0
    t(9) = 8.0
    t(10) = 9.0
    t(11) = 10.0
    t(12) = 11.0
    t(13) = 12.0
    t(14) = 13.0
    t(15) = 14.0
    t(16) = 15.0
    t(17) = 16.0
    t(18) = 17.0
    t(19) = 18.0
    t(20) = 19.0
    t(21) = 20.0
    t(22) = 21.0
    t(23) = 22.0
    t(24) = 23.0
    t(25) = 24.0
    t(26) = 24.0
    t(27) = 24.0
    t(28) = 24.0
    t(29) = 24.0
    t(30) = 24.0
    t(31) = 24.0
    t(32) = 24.0
    t(33) = 24.0
    t(34) = 24.0
    t(35) = 24.0
    jhigh = 20
    index_val = 1
    x = 12.5
    ileft = 13
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 127, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 128: Quadratic B-spline with negative knots, x=-2.0
    t = 0.0
    t(1) = -5.0
    t(2) = -3.0
    t(3) = -1.0
    t(4) = 0.0
    t(5) = 2.0
    t(6) = 4.0
    t(7) = 4.0
    t(8) = 4.0
    jhigh = 3
    index_val = 1
    x = -2.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 128, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 129: Quadratic B-spline with negative knots, x=1.0
    t = 0.0
    t(1) = -5.0
    t(2) = -3.0
    t(3) = -1.0
    t(4) = 0.0
    t(5) = 2.0
    t(6) = 4.0
    t(7) = 4.0
    t(8) = 4.0
    t(9) = 4.0
    jhigh = 3
    index_val = 1
    x = 1.0
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 129, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 130: Uniform knots spacing=0.1, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.2
    t(4) = 0.30000000000000004
    t(5) = 0.4
    t(6) = 0.5
    t(7) = 0.6000000000000001
    t(8) = 0.7000000000000001
    jhigh = 2
    index_val = 1
    x = 0.35000000000000003
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 130, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 131: Uniform knots spacing=0.1, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.2
    t(4) = 0.30000000000000004
    t(5) = 0.4
    t(6) = 0.5
    t(7) = 0.6000000000000001
    t(8) = 0.7000000000000001
    t(9) = 0.7000000000000001
    jhigh = 3
    index_val = 1
    x = 0.35000000000000003
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 131, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 132: Uniform knots spacing=0.1, order=4
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.2
    t(4) = 0.30000000000000004
    t(5) = 0.4
    t(6) = 0.5
    t(7) = 0.6000000000000001
    t(8) = 0.7000000000000001
    t(9) = 0.7000000000000001
    t(10) = 0.7000000000000001
    jhigh = 4
    index_val = 1
    x = 0.35000000000000003
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 132, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 133: Uniform knots spacing=0.5, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 1.0
    t(4) = 1.5
    t(5) = 2.0
    t(6) = 2.5
    t(7) = 3.0
    t(8) = 3.5
    jhigh = 2
    index_val = 1
    x = 1.75
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 133, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 134: Uniform knots spacing=0.5, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 1.0
    t(4) = 1.5
    t(5) = 2.0
    t(6) = 2.5
    t(7) = 3.0
    t(8) = 3.5
    t(9) = 3.5
    jhigh = 3
    index_val = 1
    x = 1.75
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 134, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 135: Uniform knots spacing=0.5, order=4
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 1.0
    t(4) = 1.5
    t(5) = 2.0
    t(6) = 2.5
    t(7) = 3.0
    t(8) = 3.5
    t(9) = 3.5
    t(10) = 3.5
    jhigh = 4
    index_val = 1
    x = 1.75
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 135, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 136: Uniform knots spacing=2.0, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 2.0
    t(3) = 4.0
    t(4) = 6.0
    t(5) = 8.0
    t(6) = 10.0
    t(7) = 12.0
    t(8) = 14.0
    jhigh = 2
    index_val = 1
    x = 7.0
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 136, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 137: Uniform knots spacing=2.0, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 2.0
    t(3) = 4.0
    t(4) = 6.0
    t(5) = 8.0
    t(6) = 10.0
    t(7) = 12.0
    t(8) = 14.0
    t(9) = 14.0
    jhigh = 3
    index_val = 1
    x = 7.0
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 137, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 138: Uniform knots spacing=2.0, order=4
    t = 0.0
    t(1) = 0.0
    t(2) = 2.0
    t(3) = 4.0
    t(4) = 6.0
    t(5) = 8.0
    t(6) = 10.0
    t(7) = 12.0
    t(8) = 14.0
    t(9) = 14.0
    t(10) = 14.0
    jhigh = 4
    index_val = 1
    x = 7.0
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 138, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 139: Uniform knots spacing=5.0, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 5.0
    t(3) = 10.0
    t(4) = 15.0
    t(5) = 20.0
    t(6) = 25.0
    t(7) = 30.0
    t(8) = 35.0
    jhigh = 2
    index_val = 1
    x = 17.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 139, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 140: Uniform knots spacing=5.0, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 5.0
    t(3) = 10.0
    t(4) = 15.0
    t(5) = 20.0
    t(6) = 25.0
    t(7) = 30.0
    t(8) = 35.0
    t(9) = 35.0
    jhigh = 3
    index_val = 1
    x = 17.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 140, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 141: Uniform knots spacing=5.0, order=4
    t = 0.0
    t(1) = 0.0
    t(2) = 5.0
    t(3) = 10.0
    t(4) = 15.0
    t(5) = 20.0
    t(6) = 25.0
    t(7) = 30.0
    t(8) = 35.0
    t(9) = 35.0
    t(10) = 35.0
    jhigh = 4
    index_val = 1
    x = 17.5
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 141, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 142: Uniform knots spacing=10.0, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 10.0
    t(3) = 20.0
    t(4) = 30.0
    t(5) = 40.0
    t(6) = 50.0
    t(7) = 60.0
    t(8) = 70.0
    jhigh = 2
    index_val = 1
    x = 35.0
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 142, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 143: Uniform knots spacing=10.0, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 10.0
    t(3) = 20.0
    t(4) = 30.0
    t(5) = 40.0
    t(6) = 50.0
    t(7) = 60.0
    t(8) = 70.0
    t(9) = 70.0
    jhigh = 3
    index_val = 1
    x = 35.0
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 143, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 144: Uniform knots spacing=10.0, order=4
    t = 0.0
    t(1) = 0.0
    t(2) = 10.0
    t(3) = 20.0
    t(4) = 30.0
    t(5) = 40.0
    t(6) = 50.0
    t(7) = 60.0
    t(8) = 70.0
    t(9) = 70.0
    t(10) = 70.0
    jhigh = 4
    index_val = 1
    x = 35.0
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 144, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 145: Special pattern knots, order=2
    t = 0.0
    t(1) = 0
    t(2) = 1
    t(3) = 4
    t(4) = 9
    t(5) = 16
    t(6) = 25
    t(7) = 25
    jhigh = 2
    index_val = 1
    x = 6.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 145, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 146: Special pattern knots, order=3
    t = 0.0
    t(1) = 0
    t(2) = 1
    t(3) = 4
    t(4) = 9
    t(5) = 16
    t(6) = 25
    t(7) = 25
    t(8) = 25
    jhigh = 3
    index_val = 1
    x = 6.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 146, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 147: Special pattern knots, order=4
    t = 0.0
    t(1) = 0
    t(2) = 1
    t(3) = 4
    t(4) = 9
    t(5) = 16
    t(6) = 25
    t(7) = 25
    t(8) = 25
    t(9) = 25
    jhigh = 4
    index_val = 1
    x = 6.5
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 147, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 148: Special pattern knots, order=2
    t = 0.0
    t(1) = 0
    t(2) = 1
    t(3) = 1.41
    t(4) = 1.73
    t(5) = 2
    t(6) = 2.24
    t(7) = 2.24
    jhigh = 2
    index_val = 1
    x = 1.5699999999999998
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 148, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 149: Special pattern knots, order=3
    t = 0.0
    t(1) = 0
    t(2) = 1
    t(3) = 1.41
    t(4) = 1.73
    t(5) = 2
    t(6) = 2.24
    t(7) = 2.24
    t(8) = 2.24
    jhigh = 3
    index_val = 1
    x = 1.5699999999999998
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 149, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 150: Special pattern knots, order=4
    t = 0.0
    t(1) = 0
    t(2) = 1
    t(3) = 1.41
    t(4) = 1.73
    t(5) = 2
    t(6) = 2.24
    t(7) = 2.24
    t(8) = 2.24
    t(9) = 2.24
    jhigh = 4
    index_val = 1
    x = 1.5699999999999998
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 150, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 151: Special pattern knots, order=2
    t = 0.0
    t(1) = 1
    t(2) = 2
    t(3) = 3
    t(4) = 5
    t(5) = 8
    t(6) = 13
    t(7) = 13
    jhigh = 2
    index_val = 1
    x = 4.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 151, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 152: Special pattern knots, order=3
    t = 0.0
    t(1) = 1
    t(2) = 2
    t(3) = 3
    t(4) = 5
    t(5) = 8
    t(6) = 13
    t(7) = 13
    t(8) = 13
    jhigh = 3
    index_val = 1
    x = 4.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 152, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 153: Special pattern knots, order=4
    t = 0.0
    t(1) = 1
    t(2) = 2
    t(3) = 3
    t(4) = 5
    t(5) = 8
    t(6) = 13
    t(7) = 13
    t(8) = 13
    t(9) = 13
    jhigh = 4
    index_val = 1
    x = 4.0
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 153, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 154: Special pattern knots, order=2
    t = 0.0
    t(1) = 0
    t(2) = 0.1
    t(3) = 0.5
    t(4) = 0.9
    t(5) = 0.99
    t(6) = 1
    t(7) = 1
    jhigh = 2
    index_val = 1
    x = 0.7
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 154, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 155: Special pattern knots, order=3
    t = 0.0
    t(1) = 0
    t(2) = 0.1
    t(3) = 0.5
    t(4) = 0.9
    t(5) = 0.99
    t(6) = 1
    t(7) = 1
    t(8) = 1
    jhigh = 3
    index_val = 1
    x = 0.7
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 155, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 156: Special pattern knots, order=4
    t = 0.0
    t(1) = 0
    t(2) = 0.1
    t(3) = 0.5
    t(4) = 0.9
    t(5) = 0.99
    t(6) = 1
    t(7) = 1
    t(8) = 1
    t(9) = 1
    jhigh = 4
    index_val = 1
    x = 0.7
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 156, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 157: Edge evaluation x=0.9999999999, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    jhigh = 2
    index_val = 1
    x = 0.9999999999
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 157, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 158: Edge evaluation x=0.9999999999, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    jhigh = 3
    index_val = 1
    x = 0.9999999999
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 158, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 159: Edge evaluation x=1.0000000001, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    jhigh = 2
    index_val = 1
    x = 1.0000000001
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 159, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 160: Edge evaluation x=1.0000000001, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    jhigh = 3
    index_val = 1
    x = 1.0000000001
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 160, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 161: Edge evaluation x=1.9999999999, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    jhigh = 2
    index_val = 1
    x = 1.9999999999
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 161, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 162: Edge evaluation x=1.9999999999, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    jhigh = 3
    index_val = 1
    x = 1.9999999999
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 162, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 163: Edge evaluation x=2.0000000001, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    jhigh = 2
    index_val = 1
    x = 2.0000000001
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 163, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 164: Edge evaluation x=2.0000000001, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    jhigh = 3
    index_val = 1
    x = 2.0000000001
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 164, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 165: Edge evaluation x=1e-15, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    jhigh = 2
    index_val = 1
    x = 1e-15
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 165, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 166: Edge evaluation x=1e-15, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    jhigh = 3
    index_val = 1
    x = 1e-15
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 166, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 167: Edge evaluation x=4.999999999, order=2
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    t(9) = 5.0
    jhigh = 2
    index_val = 1
    x = 4.999999999
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 167, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 168: Edge evaluation x=4.999999999, order=3
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 2.0
    t(4) = 3.0
    t(5) = 4.0
    t(6) = 5.0
    t(7) = 5.0
    t(8) = 5.0
    t(9) = 5.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 4.999999999
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 168, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 169: Additional random test 168
    t = 0.0
    t(1) = -9.796977711226457
    t(2) = -9.426000044598208
    t(3) = -8.287740760839576
    t(4) = -6.363202856846164
    t(5) = -5.75760299640522
    t(6) = -4.474628484876302
    t(7) = -3.1932231367143267
    t(8) = -2.420213917434719
    t(9) = 2.206634084610412
    t(10) = 4.023675006644032
    t(11) = 5.95664713656193
    t(12) = 7.0190567262491825
    t(13) = 7.606399595164508
    t(14) = 8.961251555540624
    jhigh = 5
    index_val = 1
    x = -0.9786872502460096
    ileft = 7
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 169, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 170: Additional random test 169
    t = 0.0
    t(1) = -8.138932988906566
    t(2) = -5.568071990549397
    t(3) = -3.875879397398232
    t(4) = -0.18357329842827674
    t(5) = 2.918057994819046
    t(6) = 3.812186788930461
    t(7) = 3.8357431059040366
    t(8) = 5.858657362646349
    t(9) = 5.858657362646349
    t(10) = 5.858657362646349
    jhigh = 6
    index_val = 1
    x = 0.9751331401395671
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 170, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 171: Additional random test 170
    t = 0.0
    t(1) = -7.608905174898943
    t(2) = -7.586882304953884
    t(3) = -6.297003151246761
    t(4) = -6.148314130969652
    t(5) = -4.97192647598117
    t(6) = -4.581671460476073
    t(7) = -3.384174056081397
    t(8) = -1.4899237458957053
    t(9) = 0.618438622915356
    t(10) = 0.7172793076749961
    t(11) = 4.057098843715378
    t(12) = 4.918708734272192
    t(13) = 5.243792189682059
    t(14) = 5.243792189682059
    jhigh = 3
    index_val = 1
    x = 1.350584765138585
    ileft = 9
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 171, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 172: Additional random test 171
    t = 0.0
    t(1) = -9.717032654878723
    t(2) = -7.989477838184184
    t(3) = -6.4111691262919335
    t(4) = -6.117648438178531
    t(5) = -5.450336730348906
    t(6) = -4.3400259345009
    t(7) = 0.492737382172157
    t(8) = 4.491700021861032
    t(9) = 9.532140457661136
    t(10) = 9.532140457661136
    t(11) = 9.532140457661136
    jhigh = 6
    index_val = 1
    x = -4.565042652508794
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 172, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 173: Additional random test 172
    t = 0.0
    t(1) = -7.4744101408318215
    t(2) = 1.0671793359721669
    t(3) = 3.9483478582038884
    t(4) = 7.369223945258881
    t(5) = 9.485898622054759
    t(6) = 9.485898622054759
    t(7) = 9.485898622054759
    jhigh = 3
    index_val = 1
    x = 3.9483478582038884
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 173, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 174: Additional random test 173
    t = 0.0
    t(1) = -8.972465656328444
    t(2) = -6.3127265921983655
    t(3) = -1.190624037279795
    t(4) = -0.612061101389795
    t(5) = 1.4812843925266446
    t(6) = 1.4812843925266446
    t(7) = 1.4812843925266446
    jhigh = 3
    index_val = 1
    x = -1.190624037279795
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 174, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 175: Additional random test 174
    t = 0.0
    t(1) = -8.927818514149337
    t(2) = -8.518356594887205
    t(3) = -7.016048310526896
    t(4) = -1.985851154961205
    t(5) = 1.2567919416912847
    t(6) = 2.588914139039966
    t(7) = 2.588914139039966
    jhigh = 3
    index_val = 1
    x = -6.5875715474036785
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 175, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 176: Additional random test 175
    t = 0.0
    t(1) = -7.630968755770768
    t(2) = -5.4862572586993075
    t(3) = -1.1455799196134322
    t(4) = -0.9897107022249152
    t(5) = 0.4514507026043564
    t(6) = 2.126353024859256
    t(7) = 5.288868906523884
    t(8) = 5.814816596868388
    t(9) = 5.814816596868388
    t(10) = 5.814816596868388
    jhigh = 6
    index_val = 1
    x = -1.1129628723198808
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 176, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 177: Additional random test 176
    t = 0.0
    t(1) = -8.795393912549184
    t(2) = -8.376250690660314
    t(3) = -8.015328504504236
    t(4) = -5.2026964465061525
    t(5) = -4.707077494199852
    t(6) = 1.3725894124846647
    t(7) = 2.510300608132063
    t(8) = 5.254742087094172
    t(9) = 9.202175548685982
    t(10) = 9.202175548685982
    t(11) = 9.202175548685982
    jhigh = 6
    index_val = 1
    x = 0.8790164414918157
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 177, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 178: Additional random test 177
    t = 0.0
    t(1) = -9.347202892381514
    t(2) = -4.237902208126214
    t(3) = -1.8296485467994668
    t(4) = -0.6017504198462831
    t(5) = 3.787422715695378
    t(6) = 3.787422715695378
    t(7) = 3.787422715695378
    jhigh = 3
    index_val = 1
    x = -1.8296485467994668
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 178, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 179: Additional random test 178
    t = 0.0
    t(1) = -8.576404796930415
    t(2) = -7.704473950779085
    t(3) = -7.155274291126455
    t(4) = -7.020211978493416
    t(5) = -6.043615724870717
    t(6) = -5.503810994679523
    t(7) = -5.331523876799489
    t(8) = -4.708907486822007
    t(9) = -4.687350567187196
    t(10) = 1.7957917841051554
    t(11) = 5.824954900899231
    t(12) = 6.078113605628676
    jhigh = 2
    index_val = 1
    x = -5.459633379145273
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 179, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 180: Additional random test 179
    t = 0.0
    t(1) = -4.555076137114737
    t(2) = -3.918176189104665
    t(3) = -1.9498296167438873
    t(4) = -1.216931026525252
    t(5) = -0.6260405944767626
    t(6) = 0.79963125196028
    t(7) = 1.9008807463880935
    t(8) = 6.484806026400186
    t(9) = 8.423850579352763
    t(10) = 8.423850579352763
    t(11) = 8.423850579352763
    jhigh = 5
    index_val = 1
    x = -0.6260405944767626
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 180, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 181: Additional random test 180
    t = 0.0
    t(1) = -9.48270215223329
    t(2) = -5.421511685563461
    t(3) = 2.074272818918212
    t(4) = 4.690678465712306
    t(5) = 7.788457785849072
    t(6) = 7.788457785849072
    jhigh = 2
    index_val = 1
    x = 4.686354663871619
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 181, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 182: Additional random test 181
    t = 0.0
    t(1) = -7.908002308348852
    t(2) = -6.496185441240547
    t(3) = -4.436868551151583
    t(4) = -3.578834880868569
    t(5) = -1.833038131399693
    t(6) = -1.282300258459939
    t(7) = -1.1554119115959427
    t(8) = -0.5997921007929659
    t(9) = 0.3792768197520786
    t(10) = 1.5249396920549358
    t(11) = 5.26218008343411
    t(12) = 6.284898890812066
    t(13) = 6.42842490516767
    t(14) = 9.381104721314074
    t(15) = 9.93890949865127
    jhigh = 3
    index_val = 1
    x = -0.853454963396933
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 182, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 183: Additional random test 182
    t = 0.0
    t(1) = -4.235644092770885
    t(2) = -3.7096568565966415
    t(3) = -3.5345242744801064
    t(4) = -1.9164988599850865
    t(5) = -0.9034818279654111
    t(6) = 0.2919250465821026
    t(7) = 1.0019321794356593
    t(8) = 3.1532077296625793
    t(9) = 6.271320694759453
    t(10) = 8.903870791264211
    t(11) = 9.403694536171542
    t(12) = 9.762384295635083
    t(13) = 9.762384295635083
    jhigh = 6
    index_val = 1
    x = 0.6199315467290403
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 183, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 184: Additional random test 183
    t = 0.0
    t(1) = -6.2483491721094575
    t(2) = -2.7644128169664945
    t(3) = 2.5081748429001784
    t(4) = 5.128863081111474
    t(5) = 5.19981072122034
    jhigh = 2
    index_val = 1
    x = -0.6564642479753897
    ileft = 1
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 184, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 185: Additional random test 184
    t = 0.0
    t(1) = -7.571478332747603
    t(2) = -5.214050753446992
    t(3) = -1.2376780985524345
    t(4) = 2.177433341639299
    t(5) = 3.9650005824434444
    t(6) = 8.553455216888956
    t(7) = 9.462936316433886
    jhigh = 3
    index_val = 1
    x = 0.379055021583089
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 185, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 186: Additional random test 185
    t = 0.0
    t(1) = -8.13581597436955
    t(2) = -0.7710421164243328
    t(3) = 1.0450281810746613
    t(4) = 8.25859756968336
    t(5) = 9.845142787904393
    t(6) = 9.845142787904393
    jhigh = 2
    index_val = 1
    x = 5.077542994493958
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 186, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 187: Additional random test 186
    t = 0.0
    t(1) = -5.125381878497233
    t(2) = -4.531502065734516
    t(3) = -0.032489905856094126
    t(4) = 0.17744030139023437
    t(5) = 4.332066519835822
    t(6) = 6.6944789115338885
    t(7) = 9.604892652067509
    t(8) = 9.604892652067509
    jhigh = 3
    index_val = 1
    x = 1.6416928930672985
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 187, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 188: Additional random test 187
    t = 0.0
    t(1) = -4.475051943559181
    t(2) = -4.343220334343467
    t(3) = -4.028883004564657
    t(4) = -1.7011515289730976
    t(5) = -0.2071930668859796
    t(6) = 0.1547535161931286
    t(7) = 0.16481783185787968
    t(8) = 1.7387544482832222
    t(9) = 5.800123640062269
    t(10) = 6.410989463711566
    t(11) = 7.280538688571763
    t(12) = 7.586525102929521
    t(13) = 8.6849678736504
    t(14) = 9.978046664586774
    jhigh = 3
    index_val = 1
    x = 1.1579472049696538
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 188, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 189: Additional random test 188
    t = 0.0
    t(1) = -9.127054018053961
    t(2) = -6.226952525857891
    t(3) = -5.328751078842138
    t(4) = -3.564452995419252
    t(5) = -3.097521166139849
    t(6) = -0.8931076626699781
    t(7) = 0.8686012591734595
    t(8) = 1.0383483414162207
    t(9) = 1.4359528396980217
    t(10) = 3.949968552411427
    t(11) = 4.894103031303917
    t(12) = 5.510889501985176
    jhigh = 5
    index_val = 1
    x = -1.5007498261971082
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 189, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 190: Additional random test 189
    t = 0.0
    t(1) = -9.693048041694787
    t(2) = -8.005766045160431
    t(3) = -7.9927079567029535
    t(4) = -7.475074603812226
    t(5) = -7.11689168066105
    t(6) = -6.957893595795537
    t(7) = -3.3601059213727247
    t(8) = 3.040380028012118
    t(9) = 5.847873213998607
    t(10) = 6.900788462207647
    t(11) = 7.418659426984387
    t(12) = 8.208833553929338
    t(13) = 9.296488767418207
    t(14) = 9.307071803537074
    t(15) = 9.399135880822126
    jhigh = 2
    index_val = 1
    x = 0.12244860117577439
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 190, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 191: Additional random test 190
    t = 0.0
    t(1) = -3.6667769213320067
    t(2) = -3.2287753189713664
    t(3) = -2.399812049546899
    t(4) = 2.9967330515854513
    t(5) = 3.854091971737642
    t(6) = 4.374348504459659
    t(7) = 7.035305847013827
    t(8) = 7.046826731317861
    t(9) = 7.186843683365332
    t(10) = 7.186843683365332
    t(11) = 7.186843683365332
    jhigh = 5
    index_val = 1
    x = 3.854091971737642
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 191, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 192: Additional random test 191
    t = 0.0
    t(1) = -5.450274385993725
    t(2) = -5.304595137959569
    t(3) = -4.252419919159127
    t(4) = 2.3491892320544636
    t(5) = 3.6879914881183335
    t(6) = 3.6879914881183335
    jhigh = 2
    index_val = 1
    x = -1.9830710691223659
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 192, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 193: Additional random test 192
    t = 0.0
    t(1) = -9.421055012048638
    t(2) = -7.989855193543651
    t(3) = -6.672704265415497
    t(4) = -4.010678395118705
    t(5) = 4.077816586580758
    t(6) = 5.207393859885308
    t(7) = 5.207393859885308
    jhigh = 3
    index_val = 1
    x = -4.553349764685956
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 193, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 194: Additional random test 193
    t = 0.0
    t(1) = -6.498612898741909
    t(2) = -6.398862276973778
    t(3) = -5.116051356320632
    t(4) = -2.83073315399273
    t(5) = -2.349114027662522
    t(6) = -1.7589650204884926
    t(7) = -1.3862642428919987
    t(8) = 2.1898357377707356
    t(9) = 3.6360074270545635
    jhigh = 3
    index_val = 1
    x = -3.259858451663213
    ileft = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 194, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 195: Additional random test 194
    t = 0.0
    t(1) = -9.81205137090258
    t(2) = -7.137645597429807
    t(3) = -6.840487558564261
    t(4) = -4.920453211729154
    t(5) = -4.893465836816919
    t(6) = -3.0886875111383656
    t(7) = -1.513821230782046
    t(8) = -1.1654043279237118
    t(9) = -0.7770017329000485
    t(10) = 1.7514341025284281
    t(11) = 2.778774047200244
    t(12) = 3.552217713981589
    t(13) = 6.092661539502927
    t(14) = 8.02418847197766
    jhigh = 4
    index_val = 1
    x = -1.4031704699821685
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 195, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 196: Additional random test 195
    t = 0.0
    t(1) = -6.015660017822153
    t(2) = -5.255885961448508
    t(3) = -4.095384922334613
    t(4) = -2.306135020653091
    t(5) = -0.3358387788156776
    t(6) = 1.4384538470147774
    t(7) = 1.4962386035729018
    t(8) = 9.558889691537253
    t(9) = 9.853840872537951
    t(10) = 9.853840872537951
    t(11) = 9.853840872537951
    jhigh = 5
    index_val = 1
    x = -0.3358387788156776
    ileft = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 196, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 197: Additional random test 196
    t = 0.0
    t(1) = -6.197360214458234
    t(2) = -5.619909221437891
    t(3) = -5.397632180591674
    t(4) = -4.987463999127679
    t(5) = 4.903443082679402
    t(6) = 5.111546474833544
    t(7) = 5.514819592067617
    t(8) = 5.5662323154015585
    t(9) = 6.038639150905752
    t(10) = 6.656777993463766
    t(11) = 7.301347953945321
    t(12) = 8.058698368995508
    t(13) = 8.21233907437421
    t(14) = 9.472056336558495
    jhigh = 3
    index_val = 1
    x = 3.056281969770551
    ileft = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 197, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 198: Additional random test 197
    t = 0.0
    t(1) = -8.197332485075622
    t(2) = -7.3568040450643295
    t(3) = -6.487198591113867
    t(4) = -4.097790967416895
    t(5) = -3.821342320947279
    t(6) = -1.183536196743768
    t(7) = 0.6170474740557008
    t(8) = 0.7881551796884203
    t(9) = 1.591476107368056
    t(10) = 2.926812177810456
    t(11) = 4.647195358784765
    t(12) = 4.94961729876743
    t(13) = 6.609452391347947
    t(14) = 8.269739489633938
    t(15) = 9.429791624226798
    jhigh = 4
    index_val = 1
    x = 4.171983090522762
    ileft = 9
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 198, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 199: Additional random test 198
    t = 0.0
    t(1) = -8.49401278186485
    t(2) = -5.487706086018593
    t(3) = -4.097171016550163
    t(4) = -2.051770095934014
    t(5) = -0.6333722229460932
    t(6) = 3.488124606004689
    t(7) = 3.5264744943703263
    t(8) = 5.083029861226709
    t(9) = 6.084650772090214
    t(10) = 6.914889634339158
    t(11) = 7.407939186500794
    t(12) = 9.347681523336245
    t(13) = 9.347681523336245
    jhigh = 6
    index_val = 1
    x = 3.502152038316328
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 199, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 200: Additional random test 199
    t = 0.0
    t(1) = -9.721626897981977
    t(2) = -9.499507007070353
    t(3) = -7.833077956015386
    t(4) = -5.323561955768337
    t(5) = -0.9042794453225067
    t(6) = 0.7314461653811826
    t(7) = 1.691201833043321
    t(8) = 3.446815947020262
    t(9) = 3.4565676374750716
    t(10) = 6.4483460245354856
    t(11) = 8.805837835591085
    t(12) = 8.805837835591085
    t(13) = 8.805837835591085
    jhigh = 6
    index_val = 1
    x = 0.7314461653811826
    ileft = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 200, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 200: Additional random test 200
    t = 0.0
    t(1) = -8.73565917679609
    t(2) = -8.056160027563568
    t(3) = -7.2867190258732695
    t(4) = -7.2044597498555785
    t(5) = -5.572655998520199
    t(6) = -3.956196509414996
    t(7) = -3.9127083128798956
    t(8) = -1.834082884091746
    t(9) = -0.9271248622221488
    t(10) = -0.14750762043588317
    t(11) = 6.477107027808952
    t(12) = 7.745186170570047
    t(13) = 8.187752768557843
    t(14) = 8.925230657638924
    jhigh = 6
    index_val = 1
    x = -2.686631544066471
    ileft = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 200, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

end program test_bsplvn_reference_all

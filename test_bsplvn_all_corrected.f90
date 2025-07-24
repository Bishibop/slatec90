program test_bsplvn_all
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(100), vnikx(30), x
    integer :: jhigh, index_val, ileft, vnikx_size
    integer :: i, j
    
    ! Test 1: Uniform knots, order 1, x=0.0
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
    jhigh = 1
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 1, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 2: Uniform knots, order 1, x=0.5
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
    jhigh = 1
    index_val = 1
    x = 0.5
    ileft = 0
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 2, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 3: Uniform knots, order 1, x=1.0
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
    jhigh = 1
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 3, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 4: Uniform knots, order 1, x=1.5
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
    jhigh = 1
    index_val = 1
    x = 1.5
    ileft = 1
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 4, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 5: Uniform knots, order 1, x=2.0
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
    jhigh = 1
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 5, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 6: Uniform knots, order 1, x=2.5
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
    jhigh = 1
    index_val = 1
    x = 2.5
    ileft = 2
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 6, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 7: Uniform knots, order 1, x=3.0
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
    jhigh = 1
    index_val = 1
    x = 3.0
    ileft = 3
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 7, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 8: Uniform knots, order 1, x=5.0
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
    jhigh = 1
    index_val = 1
    x = 5.0
    ileft = 5
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 8, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 9: Uniform knots, order 1, x=7.5
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
    jhigh = 1
    index_val = 1
    x = 7.5
    ileft = 7
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 9, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 10: Uniform knots, order 1, x=8.9
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
    t(11) = 9.0
    jhigh = 1
    index_val = 1
    x = 8.9
    ileft = 8
    vnikx_size = 2
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 10, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 11: Uniform knots, order 2, x=0.0
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
    jhigh = 2
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 11, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 12: Uniform knots, order 2, x=0.5
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
    jhigh = 2
    index_val = 1
    x = 0.5
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 12, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 13: Uniform knots, order 2, x=1.0
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
    jhigh = 2
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 13, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 14: Uniform knots, order 2, x=1.5
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
    jhigh = 2
    index_val = 1
    x = 1.5
    ileft = 1
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 14, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 15: Uniform knots, order 2, x=2.0
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
    jhigh = 2
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 15, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 16: Uniform knots, order 2, x=2.5
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
    jhigh = 2
    index_val = 1
    x = 2.5
    ileft = 2
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 16, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 17: Uniform knots, order 2, x=3.0
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
    jhigh = 2
    index_val = 1
    x = 3.0
    ileft = 3
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 17, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 18: Uniform knots, order 2, x=5.0
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
    jhigh = 2
    index_val = 1
    x = 5.0
    ileft = 5
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 18, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 19: Uniform knots, order 2, x=7.5
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
    t(11) = 9.0
    jhigh = 2
    index_val = 1
    x = 7.5
    ileft = 7
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 19, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 20: Uniform knots, order 2, x=8.9
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
    t(11) = 9.0
    t(12) = 9.0
    jhigh = 2
    index_val = 1
    x = 8.9
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 20, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 21: Uniform knots, order 3, x=0.0
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
    jhigh = 3
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 21, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 22: Uniform knots, order 3, x=0.5
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
    jhigh = 3
    index_val = 1
    x = 0.5
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 22, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 23: Uniform knots, order 3, x=1.0
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
    jhigh = 3
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 23, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 24: Uniform knots, order 3, x=1.5
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
    jhigh = 3
    index_val = 1
    x = 1.5
    ileft = 1
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 24, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 25: Uniform knots, order 3, x=2.0
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
    jhigh = 3
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 25, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 26: Uniform knots, order 3, x=2.5
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
    jhigh = 3
    index_val = 1
    x = 2.5
    ileft = 2
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 26, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 27: Uniform knots, order 3, x=3.0
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
    jhigh = 3
    index_val = 1
    x = 3.0
    ileft = 3
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 27, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 28: Uniform knots, order 3, x=5.0
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
    jhigh = 3
    index_val = 1
    x = 5.0
    ileft = 5
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 28, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 29: Uniform knots, order 3, x=7.5
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
    t(11) = 9.0
    t(12) = 9.0
    jhigh = 3
    index_val = 1
    x = 7.5
    ileft = 7
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 29, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 30: Uniform knots, order 3, x=8.9
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
    t(11) = 9.0
    t(12) = 9.0
    t(13) = 9.0
    jhigh = 3
    index_val = 1
    x = 8.9
    ileft = 8
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 30, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 31: Uniform knots, order 4, x=0.0
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
    jhigh = 4
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 31, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 32: Uniform knots, order 4, x=0.5
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
    jhigh = 4
    index_val = 1
    x = 0.5
    ileft = 0
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 32, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 33: Uniform knots, order 4, x=1.0
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
    jhigh = 4
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 33, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 34: Uniform knots, order 4, x=1.5
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
    jhigh = 4
    index_val = 1
    x = 1.5
    ileft = 1
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 34, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 35: Uniform knots, order 4, x=2.0
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
    jhigh = 4
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 35, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 36: Uniform knots, order 4, x=2.5
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
    jhigh = 4
    index_val = 1
    x = 2.5
    ileft = 2
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 36, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 37: Uniform knots, order 4, x=3.0
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
    jhigh = 4
    index_val = 1
    x = 3.0
    ileft = 3
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 37, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 38: Uniform knots, order 4, x=5.0
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
    t(11) = 9.0
    jhigh = 4
    index_val = 1
    x = 5.0
    ileft = 5
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 38, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 39: Uniform knots, order 4, x=7.5
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
    t(11) = 9.0
    t(12) = 9.0
    t(13) = 9.0
    jhigh = 4
    index_val = 1
    x = 7.5
    ileft = 7
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 39, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 40: Uniform knots, order 4, x=8.9
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
    t(11) = 9.0
    t(12) = 9.0
    t(13) = 9.0
    t(14) = 9.0
    jhigh = 4
    index_val = 1
    x = 8.9
    ileft = 8
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 40, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 41: Uniform knots, order 5, x=0.0
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
    jhigh = 5
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 41, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 42: Uniform knots, order 5, x=0.5
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
    jhigh = 5
    index_val = 1
    x = 0.5
    ileft = 0
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 42, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 43: Uniform knots, order 5, x=1.0
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
    jhigh = 5
    index_val = 1
    x = 1.0
    ileft = 1
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 43, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 44: Uniform knots, order 5, x=1.5
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
    jhigh = 5
    index_val = 1
    x = 1.5
    ileft = 1
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 44, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 45: Uniform knots, order 5, x=2.0
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
    jhigh = 5
    index_val = 1
    x = 2.0
    ileft = 2
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 45, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 46: Uniform knots, order 5, x=2.5
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
    jhigh = 5
    index_val = 1
    x = 2.5
    ileft = 2
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 46, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 47: Uniform knots, order 5, x=3.0
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
    jhigh = 5
    index_val = 1
    x = 3.0
    ileft = 3
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 47, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 48: Uniform knots, order 5, x=5.0
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
    t(11) = 9.0
    t(12) = 9.0
    jhigh = 5
    index_val = 1
    x = 5.0
    ileft = 5
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 48, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 49: Uniform knots, order 5, x=7.5
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
    t(11) = 9.0
    t(12) = 9.0
    t(13) = 9.0
    t(14) = 9.0
    jhigh = 5
    index_val = 1
    x = 7.5
    ileft = 7
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 49, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 50: Uniform knots, order 5, x=8.9
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
    t(11) = 9.0
    t(12) = 9.0
    t(13) = 9.0
    t(14) = 9.0
    t(15) = 9.0
    jhigh = 5
    index_val = 1
    x = 8.9
    ileft = 8
    vnikx_size = 6
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 50, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 51: Non-uniform knots, order 2, x=0.000
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 51, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 52: Non-uniform knots, order 2, x=0.287
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 0.28657894736842104
    ileft = 1
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 52, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 53: Non-uniform knots, order 2, x=0.573
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 0.5731578947368421
    ileft = 2
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 53, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 54: Non-uniform knots, order 2, x=0.860
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 0.8597368421052631
    ileft = 3
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 54, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 55: Non-uniform knots, order 2, x=1.146
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 1.1463157894736842
    ileft = 4
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 55, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 56: Non-uniform knots, order 2, x=1.433
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 1.4328947368421052
    ileft = 4
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 56, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 57: Non-uniform knots, order 2, x=1.719
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 1.7194736842105263
    ileft = 5
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 57, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 58: Non-uniform knots, order 2, x=2.006
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 2.0060526315789473
    ileft = 5
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 58, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 59: Non-uniform knots, order 2, x=2.293
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 2.2926315789473684
    ileft = 6
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 59, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 60: Non-uniform knots, order 2, x=2.579
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 2.5792105263157894
    ileft = 6
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 60, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 61: Non-uniform knots, order 2, x=2.866
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 2.8657894736842104
    ileft = 7
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 61, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 62: Non-uniform knots, order 2, x=3.152
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 3.1523684210526315
    ileft = 7
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 62, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 63: Non-uniform knots, order 2, x=3.439
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 2
    index_val = 1
    x = 3.4389473684210525
    ileft = 7
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 63, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 64: Non-uniform knots, order 2, x=3.726
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    jhigh = 2
    index_val = 1
    x = 3.7255263157894736
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 64, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 65: Non-uniform knots, order 2, x=4.012
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    jhigh = 2
    index_val = 1
    x = 4.012105263157895
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 65, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 66: Non-uniform knots, order 2, x=4.299
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    jhigh = 2
    index_val = 1
    x = 4.298684210526316
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 66, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 67: Non-uniform knots, order 2, x=4.585
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 2
    index_val = 1
    x = 4.585263157894737
    ileft = 9
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 67, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 68: Non-uniform knots, order 2, x=4.872
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 2
    index_val = 1
    x = 4.871842105263157
    ileft = 9
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 68, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 69: Non-uniform knots, order 2, x=5.158
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 2
    index_val = 1
    x = 5.158421052631579
    ileft = 9
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 69, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 70: Non-uniform knots, order 2, x=5.445
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 2
    index_val = 1
    x = 5.445
    ileft = 9
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 70, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 71: Non-uniform knots, order 3, x=0.000
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 71, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 72: Non-uniform knots, order 3, x=0.287
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 0.28657894736842104
    ileft = 1
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 72, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 73: Non-uniform knots, order 3, x=0.573
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 0.5731578947368421
    ileft = 2
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 73, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 74: Non-uniform knots, order 3, x=0.860
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 0.8597368421052631
    ileft = 3
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 74, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 75: Non-uniform knots, order 3, x=1.146
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 1.1463157894736842
    ileft = 4
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 75, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 76: Non-uniform knots, order 3, x=1.433
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 1.4328947368421052
    ileft = 4
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 76, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 77: Non-uniform knots, order 3, x=1.719
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 1.7194736842105263
    ileft = 5
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 77, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 78: Non-uniform knots, order 3, x=2.006
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 2.0060526315789473
    ileft = 5
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 78, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 79: Non-uniform knots, order 3, x=2.293
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 2.2926315789473684
    ileft = 6
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 79, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 80: Non-uniform knots, order 3, x=2.579
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 3
    index_val = 1
    x = 2.5792105263157894
    ileft = 6
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 80, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 81: Non-uniform knots, order 3, x=2.866
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    jhigh = 3
    index_val = 1
    x = 2.8657894736842104
    ileft = 7
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 81, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 82: Non-uniform knots, order 3, x=3.152
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    jhigh = 3
    index_val = 1
    x = 3.1523684210526315
    ileft = 7
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 82, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 83: Non-uniform knots, order 3, x=3.439
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    jhigh = 3
    index_val = 1
    x = 3.4389473684210525
    ileft = 7
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 83, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 84: Non-uniform knots, order 3, x=3.726
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 3
    index_val = 1
    x = 3.7255263157894736
    ileft = 8
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 84, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 85: Non-uniform knots, order 3, x=4.012
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 3
    index_val = 1
    x = 4.012105263157895
    ileft = 8
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 85, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 86: Non-uniform knots, order 3, x=4.299
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 3
    index_val = 1
    x = 4.298684210526316
    ileft = 8
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 86, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 87: Non-uniform knots, order 3, x=4.585
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    jhigh = 3
    index_val = 1
    x = 4.585263157894737
    ileft = 9
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 87, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 88: Non-uniform knots, order 3, x=4.872
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    jhigh = 3
    index_val = 1
    x = 4.871842105263157
    ileft = 9
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 88, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 89: Non-uniform knots, order 3, x=5.158
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    jhigh = 3
    index_val = 1
    x = 5.158421052631579
    ileft = 9
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 89, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 90: Non-uniform knots, order 3, x=5.445
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    jhigh = 3
    index_val = 1
    x = 5.445
    ileft = 9
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 90, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 91: Non-uniform knots, order 4, x=0.000
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 4
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 91, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 92: Non-uniform knots, order 4, x=0.287
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 4
    index_val = 1
    x = 0.28657894736842104
    ileft = 1
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 92, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 93: Non-uniform knots, order 4, x=0.573
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 4
    index_val = 1
    x = 0.5731578947368421
    ileft = 2
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 93, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 94: Non-uniform knots, order 4, x=0.860
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 4
    index_val = 1
    x = 0.8597368421052631
    ileft = 3
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 94, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 95: Non-uniform knots, order 4, x=1.146
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 4
    index_val = 1
    x = 1.1463157894736842
    ileft = 4
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 95, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 96: Non-uniform knots, order 4, x=1.433
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 4
    index_val = 1
    x = 1.4328947368421052
    ileft = 4
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 96, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 97: Non-uniform knots, order 4, x=1.719
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 4
    index_val = 1
    x = 1.7194736842105263
    ileft = 5
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 97, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 98: Non-uniform knots, order 4, x=2.006
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    jhigh = 4
    index_val = 1
    x = 2.0060526315789473
    ileft = 5
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 98, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 99: Non-uniform knots, order 4, x=2.293
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    jhigh = 4
    index_val = 1
    x = 2.2926315789473684
    ileft = 6
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 99, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 100: Non-uniform knots, order 4, x=2.579
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    jhigh = 4
    index_val = 1
    x = 2.5792105263157894
    ileft = 6
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 100, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 101: Non-uniform knots, order 4, x=2.866
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 4
    index_val = 1
    x = 2.8657894736842104
    ileft = 7
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 101, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 102: Non-uniform knots, order 4, x=3.152
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 4
    index_val = 1
    x = 3.1523684210526315
    ileft = 7
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 102, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 103: Non-uniform knots, order 4, x=3.439
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    jhigh = 4
    index_val = 1
    x = 3.4389473684210525
    ileft = 7
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 103, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 104: Non-uniform knots, order 4, x=3.726
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    jhigh = 4
    index_val = 1
    x = 3.7255263157894736
    ileft = 8
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 104, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 105: Non-uniform knots, order 4, x=4.012
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    jhigh = 4
    index_val = 1
    x = 4.012105263157895
    ileft = 8
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 105, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 106: Non-uniform knots, order 4, x=4.299
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    jhigh = 4
    index_val = 1
    x = 4.298684210526316
    ileft = 8
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 106, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 107: Non-uniform knots, order 4, x=4.585
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    t(15) = 5.5
    jhigh = 4
    index_val = 1
    x = 4.585263157894737
    ileft = 9
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 107, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 108: Non-uniform knots, order 4, x=4.872
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    t(15) = 5.5
    jhigh = 4
    index_val = 1
    x = 4.871842105263157
    ileft = 9
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 108, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 109: Non-uniform knots, order 4, x=5.158
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    t(15) = 5.5
    jhigh = 4
    index_val = 1
    x = 5.158421052631579
    ileft = 9
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 109, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 110: Non-uniform knots, order 4, x=5.445
    t = 0.0
    t(1) = 0.0
    t(2) = 0.1
    t(3) = 0.3
    t(4) = 0.6
    t(5) = 1.0
    t(6) = 1.5
    t(7) = 2.1
    t(8) = 2.8
    t(9) = 3.6
    t(10) = 4.5
    t(11) = 5.5
    t(12) = 5.5
    t(13) = 5.5
    t(14) = 5.5
    t(15) = 5.5
    jhigh = 4
    index_val = 1
    x = 5.445
    ileft = 9
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 110, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 111: Non-uniform knots, order 2, x=0.000
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 111, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 112: Non-uniform knots, order 2, x=0.104
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.10421052631578948
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 112, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 113: Non-uniform knots, order 2, x=0.208
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.20842105263157895
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 113, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 114: Non-uniform knots, order 2, x=0.313
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.3126315789473684
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 114, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 115: Non-uniform knots, order 2, x=0.417
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.4168421052631579
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 115, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 116: Non-uniform knots, order 2, x=0.521
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.5210526315789474
    ileft = 1
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 116, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 117: Non-uniform knots, order 2, x=0.625
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.6252631578947369
    ileft = 1
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 117, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 118: Non-uniform knots, order 2, x=0.729
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.7294736842105263
    ileft = 2
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 118, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 119: Non-uniform knots, order 2, x=0.834
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.8336842105263158
    ileft = 3
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 119, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 120: Non-uniform knots, order 2, x=0.938
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 0.9378947368421053
    ileft = 4
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 120, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 121: Non-uniform knots, order 2, x=1.042
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.0421052631578949
    ileft = 5
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 121, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 122: Non-uniform knots, order 2, x=1.146
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.1463157894736842
    ileft = 6
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 122, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 123: Non-uniform knots, order 2, x=1.251
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.2505263157894737
    ileft = 7
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 123, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 124: Non-uniform knots, order 2, x=1.355
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.3547368421052632
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 124, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 125: Non-uniform knots, order 2, x=1.459
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.4589473684210525
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 125, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 126: Non-uniform knots, order 2, x=1.563
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.563157894736842
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 126, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 127: Non-uniform knots, order 2, x=1.667
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.6673684210526316
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 127, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 128: Non-uniform knots, order 2, x=1.772
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.7715789473684211
    ileft = 9
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 128, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 129: Non-uniform knots, order 2, x=1.876
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.8757894736842107
    ileft = 9
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 129, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 130: Non-uniform knots, order 2, x=1.980
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    jhigh = 2
    index_val = 1
    x = 1.98
    ileft = 9
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 130, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 131: Non-uniform knots, order 3, x=0.000
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 131, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 132: Non-uniform knots, order 3, x=0.104
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.10421052631578948
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 132, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 133: Non-uniform knots, order 3, x=0.208
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.20842105263157895
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 133, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 134: Non-uniform knots, order 3, x=0.313
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.3126315789473684
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 134, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 135: Non-uniform knots, order 3, x=0.417
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.4168421052631579
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 135, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 136: Non-uniform knots, order 3, x=0.521
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.5210526315789474
    ileft = 1
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 136, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 137: Non-uniform knots, order 3, x=0.625
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.6252631578947369
    ileft = 1
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 137, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 138: Non-uniform knots, order 3, x=0.729
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.7294736842105263
    ileft = 2
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 138, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 139: Non-uniform knots, order 3, x=0.834
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.8336842105263158
    ileft = 3
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 139, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 140: Non-uniform knots, order 3, x=0.938
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 0.9378947368421053
    ileft = 4
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 140, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 141: Non-uniform knots, order 3, x=1.042
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.0421052631578949
    ileft = 5
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 141, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 142: Non-uniform knots, order 3, x=1.146
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.1463157894736842
    ileft = 6
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 142, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 143: Non-uniform knots, order 3, x=1.251
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.2505263157894737
    ileft = 7
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 143, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 144: Non-uniform knots, order 3, x=1.355
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.3547368421052632
    ileft = 8
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 144, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 145: Non-uniform knots, order 3, x=1.459
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.4589473684210525
    ileft = 8
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 145, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 146: Non-uniform knots, order 3, x=1.563
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.563157894736842
    ileft = 8
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 146, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 147: Non-uniform knots, order 3, x=1.667
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.6673684210526316
    ileft = 8
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 147, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 148: Non-uniform knots, order 3, x=1.772
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.7715789473684211
    ileft = 9
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 148, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 149: Non-uniform knots, order 3, x=1.876
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.8757894736842107
    ileft = 9
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 149, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 150: Non-uniform knots, order 3, x=1.980
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    jhigh = 3
    index_val = 1
    x = 1.98
    ileft = 9
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 150, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 151: Non-uniform knots, order 4, x=0.000
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 151, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 152: Non-uniform knots, order 4, x=0.104
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.10421052631578948
    ileft = 0
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 152, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 153: Non-uniform knots, order 4, x=0.208
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.20842105263157895
    ileft = 0
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 153, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 154: Non-uniform knots, order 4, x=0.313
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.3126315789473684
    ileft = 0
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 154, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 155: Non-uniform knots, order 4, x=0.417
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.4168421052631579
    ileft = 0
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 155, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 156: Non-uniform knots, order 4, x=0.521
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.5210526315789474
    ileft = 1
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 156, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 157: Non-uniform knots, order 4, x=0.625
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.6252631578947369
    ileft = 1
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 157, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 158: Non-uniform knots, order 4, x=0.729
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.7294736842105263
    ileft = 2
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 158, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 159: Non-uniform knots, order 4, x=0.834
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.8336842105263158
    ileft = 3
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 159, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 160: Non-uniform knots, order 4, x=0.938
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 0.9378947368421053
    ileft = 4
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 160, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 161: Non-uniform knots, order 4, x=1.042
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.0421052631578949
    ileft = 5
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 161, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 162: Non-uniform knots, order 4, x=1.146
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.1463157894736842
    ileft = 6
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 162, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 163: Non-uniform knots, order 4, x=1.251
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.2505263157894737
    ileft = 7
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 163, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 164: Non-uniform knots, order 4, x=1.355
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.3547368421052632
    ileft = 8
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 164, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 165: Non-uniform knots, order 4, x=1.459
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.4589473684210525
    ileft = 8
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 165, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 166: Non-uniform knots, order 4, x=1.563
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.563157894736842
    ileft = 8
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 166, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 167: Non-uniform knots, order 4, x=1.667
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.6673684210526316
    ileft = 8
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 167, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 168: Non-uniform knots, order 4, x=1.772
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    t(15) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.7715789473684211
    ileft = 9
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 168, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 169: Non-uniform knots, order 4, x=1.876
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    t(15) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.8757894736842107
    ileft = 9
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 169, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 170: Non-uniform knots, order 4, x=1.980
    t = 0.0
    t(1) = 0.0
    t(2) = 0.5
    t(3) = 0.7
    t(4) = 0.8
    t(5) = 0.9
    t(6) = 1.0
    t(7) = 1.1
    t(8) = 1.2
    t(9) = 1.3
    t(10) = 1.7
    t(11) = 2.0
    t(12) = 2.0
    t(13) = 2.0
    t(14) = 2.0
    t(15) = 2.0
    jhigh = 4
    index_val = 1
    x = 1.98
    ileft = 9
    vnikx_size = 5
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 170, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 171: Non-uniform knots, order 2, x=0.000
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 171, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 172: Non-uniform knots, order 2, x=0.261
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 0.2605263157894737
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 172, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 173: Non-uniform knots, order 2, x=0.521
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 0.5210526315789474
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 173, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 174: Non-uniform knots, order 2, x=0.782
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 0.7815789473684212
    ileft = 0
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 174, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 175: Non-uniform knots, order 2, x=1.042
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 1.0421052631578949
    ileft = 2
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 175, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 176: Non-uniform knots, order 2, x=1.303
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 1.3026315789473686
    ileft = 2
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 176, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 177: Non-uniform knots, order 2, x=1.563
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 1.5631578947368423
    ileft = 2
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 177, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 178: Non-uniform knots, order 2, x=1.824
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 1.823684210526316
    ileft = 2
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 178, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 179: Non-uniform knots, order 2, x=2.084
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 2.0842105263157897
    ileft = 4
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 179, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 180: Non-uniform knots, order 2, x=2.345
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 2.3447368421052635
    ileft = 4
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 180, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 181: Non-uniform knots, order 2, x=2.605
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 2.605263157894737
    ileft = 4
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 181, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 182: Non-uniform knots, order 2, x=2.866
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 2.865789473684211
    ileft = 4
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 182, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 183: Non-uniform knots, order 2, x=3.126
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 3.1263157894736846
    ileft = 6
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 183, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 184: Non-uniform knots, order 2, x=3.387
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 3.3868421052631583
    ileft = 6
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 184, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 185: Non-uniform knots, order 2, x=3.647
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 3.647368421052632
    ileft = 6
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 185, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 186: Non-uniform knots, order 2, x=3.908
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 2
    index_val = 1
    x = 3.9078947368421058
    ileft = 6
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 186, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 187: Non-uniform knots, order 2, x=4.168
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    t(11) = 5.0
    t(12) = 5.0
    jhigh = 2
    index_val = 1
    x = 4.1684210526315795
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 187, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 188: Non-uniform knots, order 2, x=4.429
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    t(11) = 5.0
    t(12) = 5.0
    jhigh = 2
    index_val = 1
    x = 4.428947368421053
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 188, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 189: Non-uniform knots, order 2, x=4.689
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    t(11) = 5.0
    t(12) = 5.0
    jhigh = 2
    index_val = 1
    x = 4.689473684210527
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 189, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 190: Non-uniform knots, order 2, x=4.950
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    t(11) = 5.0
    t(12) = 5.0
    jhigh = 2
    index_val = 1
    x = 4.95
    ileft = 8
    vnikx_size = 3
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 190, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 191: Non-uniform knots, order 3, x=0.000
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 0.0
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 191, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 192: Non-uniform knots, order 3, x=0.261
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 0.2605263157894737
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 192, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 193: Non-uniform knots, order 3, x=0.521
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 0.5210526315789474
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 193, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 194: Non-uniform knots, order 3, x=0.782
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 0.7815789473684212
    ileft = 0
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 194, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 195: Non-uniform knots, order 3, x=1.042
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 1.0421052631578949
    ileft = 2
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 195, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 196: Non-uniform knots, order 3, x=1.303
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 1.3026315789473686
    ileft = 2
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 196, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 197: Non-uniform knots, order 3, x=1.563
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 1.5631578947368423
    ileft = 2
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 197, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 198: Non-uniform knots, order 3, x=1.824
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 1.823684210526316
    ileft = 2
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 198, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 199: Non-uniform knots, order 3, x=2.084
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 2.0842105263157897
    ileft = 4
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 199, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

    ! Test 200: Non-uniform knots, order 3, x=2.345
    t = 0.0
    t(1) = 0.0
    t(2) = 1.0
    t(3) = 1.0
    t(4) = 2.0
    t(5) = 2.0
    t(6) = 3.0
    t(7) = 3.0
    t(8) = 4.0
    t(9) = 4.0
    t(10) = 5.0
    jhigh = 3
    index_val = 1
    x = 2.3447368421052635
    ileft = 4
    vnikx_size = 4
    vnikx = 0.0
    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)
    write(*,'(A,I5,A)', advance='no') 'TEST_', 200, '_RESULT:'
    do j = 1, jhigh
        write(*,'(1X,ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)

end program test_bsplvn_all

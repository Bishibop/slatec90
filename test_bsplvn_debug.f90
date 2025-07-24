program test_bsplvn_debug
    use bsplvn_module, only: bsplvn
    implicit none
    
    integer :: i, j
    real :: t(100), vnikx(30), x
    integer :: jhigh, index, ileft
    
    ! Set up uniform knots
    t = 0.0
    do i = 1, 10
        t(i) = real(i-1)
    end do
    
    ! Test with order 2 (linear B-splines)
    write(*,*) "Testing order 2 (linear) B-splines:"
    jhigh = 2
    index = 1
    x = 0.5
    ileft = 0
    
    ! Initialize vnikx
    do j = 1, 30
        vnikx(j) = 0.0
    end do
    
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,*) "x =", x, ", ileft =", ileft
    write(*,*) "Results:"
    do j = 1, jhigh+1
        write(*,'(A,I2,A,ES20.13)') "vnikx(", j, ") = ", vnikx(j)
    end do
    
    ! Test with order 3 (quadratic B-splines)
    write(*,*)
    write(*,*) "Testing order 3 (quadratic) B-splines:"
    jhigh = 3
    index = 1
    x = 1.5
    ileft = 1
    
    ! Initialize vnikx
    do j = 1, 30
        vnikx(j) = 0.0
    end do
    
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,*) "x =", x, ", ileft =", ileft
    write(*,*) "Results:"
    do j = 1, jhigh+1
        write(*,'(A,I2,A,ES20.13)') "vnikx(", j, ") = ", vnikx(j)
    end do
    
    ! Test order 1 again with detailed output
    write(*,*)
    write(*,*) "Testing order 1 (constant) B-splines with debug:"
    jhigh = 1
    index = 1
    x = 0.5
    ileft = 0
    
    ! Initialize vnikx
    do j = 1, 30
        vnikx(j) = -999.0  ! Use sentinel value
    end do
    
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,*) "x =", x, ", ileft =", ileft
    write(*,*) "Results:"
    do j = 1, 5
        write(*,'(A,I2,A,ES20.13)') "vnikx(", j, ") = ", vnikx(j)
    end do
    
end program test_bsplvn_debug
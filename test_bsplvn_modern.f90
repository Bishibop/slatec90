program test_bsplvn_modern
    use bsplvn_module, only: bsplvn
    implicit none
    
    integer :: i, j
    real :: t(100), vnikx(30), x
    integer :: jhigh, index, ileft, vnikx_size
    
    ! Test 1: Uniform knots, order 1, x=0.0
    t = 0.0
    do i = 1, 10
        t(i) = real(i-1)
    end do
    jhigh = 1
    index = 1
    x = 0.0
    ileft = 0
    vnikx_size = 2
    
    do j = 1, vnikx_size
        vnikx(j) = 0.0
    end do
    
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I5,A)', advance='no') 'TEST_', 1, '_RESULT:'
    do j = 1, vnikx_size
        write(*,'(ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)
    
    ! Test 2: Uniform knots, order 1, x=0.5
    jhigh = 1
    index = 1
    x = 0.5
    ileft = 0
    
    do j = 1, vnikx_size
        vnikx(j) = 0.0
    end do
    
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I5,A)', advance='no') 'TEST_', 2, '_RESULT:'
    do j = 1, vnikx_size
        write(*,'(ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)
    
    ! Test 3: Uniform knots, order 1, x=1.0
    jhigh = 1
    index = 1
    x = 1.0
    ileft = 1
    
    do j = 1, vnikx_size
        vnikx(j) = 0.0
    end do
    
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I5,A)', advance='no') 'TEST_', 3, '_RESULT:'
    do j = 1, vnikx_size
        write(*,'(ES20.13)', advance='no') vnikx(j)
    end do
    write(*,*)
    
end program test_bsplvn_modern
program test_index_continuation
    use bsplvn_module
    implicit none
    
    real :: t(10) = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 0.0]
    integer :: jhigh = 2
    real :: x = 0.5
    integer :: ileft = 1
    real :: vnikx(10)
    integer :: i
    
    print *, "Testing INDEX=1 followed by INDEX=2"
    print *, "jhigh =", jhigh
    
    ! Initialize
    vnikx = 0.0
    
    ! First call with INDEX=1
    call bsplvn(t, jhigh, 1, x, ileft, vnikx)
    print *, "After INDEX=1:"
    do i = 1, 5
        if (vnikx(i) /= 0.0) print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    ! Second call with INDEX=2 (continuation)
    call bsplvn(t, jhigh, 2, x, ileft, vnikx)
    print *, "After INDEX=2:"
    do i = 1, 5
        if (vnikx(i) /= 0.0) print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
    ! Try another INDEX=2 call
    call bsplvn(t, jhigh, 2, x, ileft, vnikx)
    print *, "After second INDEX=2:"
    do i = 1, 5
        if (vnikx(i) /= 0.0) print *, "  vnikx(", i, ") =", vnikx(i)
    end do
    
end program test_index_continuation
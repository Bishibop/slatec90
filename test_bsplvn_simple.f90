program test_bsplvn_simple
    use bsplvn_module
    implicit none
    
    real :: t(4) = [0.0, 1.0, 2.0, 3.0]  ! Simple knot sequence
    real :: x = 0.5                       ! Evaluation point
    real :: vnikx(2)                      ! Output array for K=2
    integer :: ileft = 0                  ! Left index (0-based)
    integer :: jhigh = 2                  ! Order K=2
    integer :: i
    
    ! Initialize output array
    vnikx = 0.0
    
    print *, "Testing BSPLVN with simple case:"
    print *, "Knots:", t(1:3)
    print *, "x =", x
    print *, "ileft =", ileft
    print *, "jhigh =", jhigh
    print *, ""
    
    ! Call with INDEX=1
    call bsplvn(t, jhigh, 1, x, ileft, vnikx)
    
    print *, "After INDEX=1 call:"
    print *, "vnikx =", vnikx
    print *, ""
    
    ! For debugging, let's add some prints inside the subroutine
    
end program test_bsplvn_simple
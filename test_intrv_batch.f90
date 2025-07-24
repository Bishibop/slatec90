program test_intrv_batch
    use intrv_module
    implicit none
    
    ! Maximum array size
    integer, parameter :: max_size = 100
    
    ! Test inputs
    real :: xt(max_size)
    integer :: lxt
    real :: x
    integer :: ilo_in, ilo_out
    integer :: ileft, mflag
    
    ! File I/O
    integer :: test_number
    integer :: i
    character(len=200) :: line
    
    ! Read test parameters from stdin
    do
        read(*, *, iostat=i) test_number
        if (i /= 0) exit
        
        ! Read array size
        read(*, *) lxt
        
        ! Read array values
        do i = 1, lxt
            read(*, *) xt(i)
        end do
        
        ! Read x value and initial ilo
        read(*, *) x, ilo_in
        
        ! Run INTRV
        ilo_out = ilo_in
        call intrv(xt, lxt, x, ilo_out, ileft, mflag)
        
        ! Output: test_number,ileft,mflag,ilo_out
        write(*, '(I0,",",I0,",",I0,",",I0)') test_number, ileft, mflag, ilo_out
    end do
    
end program test_intrv_batch
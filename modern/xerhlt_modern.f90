module xerhlt_module
    implicit none
    private
    public :: xerhlt

contains

    subroutine xerhlt(messg)
        ! Abort program execution and print error message
        ! Modern F90 version of SLATEC XERHLT routine
        
        ! Arguments
        character(len=*), intent(in) :: messg
        
        ! Execute error stop
        ! In modern Fortran, we use 'error stop' instead of just 'stop'
        ! This provides better error handling and allows the message to be passed
        ! However, for compatibility with the original behavior, we use simple STOP
        stop
        
    end subroutine xerhlt

end module xerhlt_module
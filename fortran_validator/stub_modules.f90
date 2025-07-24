! Stub modules for missing Phase 0 functions
! These provide placeholder implementations when the real modern versions don't exist yet

module fdump_module
    implicit none
    private
    public :: fdump
contains
    subroutine fdump()
        ! Stub implementation - just print a message
        print *, 'FDUMP stub called - modern implementation not available'
    end subroutine fdump
end module fdump_module

module lsame_module
    implicit none
    private
    public :: lsame
contains
    logical function lsame(ca, cb)
        character, intent(in) :: ca, cb
        ! Stub implementation - always return false to trigger failure
        lsame = .false.
        print *, 'LSAME stub called - modern implementation not available'
    end function lsame
end module lsame_module

module i1mach_module
    implicit none
    private
    public :: i1mach
contains
    integer function i1mach(i)
        integer, intent(in) :: i
        ! Stub implementation - return invalid value to trigger failure
        i1mach = -999999
        print *, 'I1MACH stub called - modern implementation not available'
    end function i1mach
end module i1mach_module

module r1mach_module
    implicit none
    private
    public :: r1mach
contains
    real function r1mach(i)
        integer, intent(in) :: i
        ! Stub implementation - return invalid value to trigger failure
        r1mach = -999999.0
        print *, 'R1MACH stub called - modern implementation not available'
    end function r1mach
end module r1mach_module

module d1mach_module
    implicit none
    private
    public :: d1mach
contains
    real(8) function d1mach(i)
        integer, intent(in) :: i
        ! Stub implementation - return invalid value to trigger failure
        d1mach = -999999.0d0
        print *, 'D1MACH stub called - modern implementation not available'
    end function d1mach
end module d1mach_module
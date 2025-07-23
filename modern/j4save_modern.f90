module j4save_module
    implicit none
    private
    public :: j4save
    
    ! Static storage for error handling parameters
    ! These values persist between calls (equivalent to SAVE in F77)
    integer, save :: iparam(9) = [0, 2, 0, 10, 1, 0, 0, 0, 0]
    
contains
    
    integer function j4save(iwhich, ivalue, iset) result(old_value)
        implicit none
        integer, intent(in) :: iwhich  ! Which parameter (1-9)
        integer, intent(in) :: ivalue  ! Value to set (if iset is true)
        logical, intent(in) :: iset    ! True to set, False to just retrieve
        
        ! Return the old value
        old_value = iparam(iwhich)
        
        ! If iset is true, update the parameter
        if (iset) then
            iparam(iwhich) = ivalue
        end if
        
    end function j4save
    
end module j4save_module
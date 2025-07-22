module lsame_module
  implicit none
  private
  public :: lsame

contains

  pure logical function lsame(ca, cb) result(same)
    implicit none
    character(len=1), intent(in) :: ca, cb
    
    ! Local variables
    integer :: ioff
    
    ! Calculate the offset between 'a' and 'A'
    ioff = ichar('a') - ichar('A')
    
    ! Test if the characters are equal or equivalent
    ! CA is compared with CB where CB is assumed to be uppercase
    ! If CA is lowercase, we check if CA - offset equals CB
    same = (ca == cb) .or. (ichar(ca) - ioff == ichar(cb))
    
  end function lsame

end module lsame_module
module cdiv_module
  implicit none
  private
  public :: cdiv

contains

  pure subroutine cdiv(ar, ai, br, bi, cr, ci)
    implicit none
    real, intent(in) :: ar, ai, br, bi
    real, intent(out) :: cr, ci
    
    real :: s, ars, ais, brs, bis
    
    ! Scale to avoid overflow
    s = abs(br) + abs(bi)
    ars = ar / s
    ais = ai / s
    brs = br / s
    bis = bi / s
    
    ! Compute division
    s = brs**2 + bis**2
    cr = (ars*brs + ais*bis) / s
    ci = (ais*brs - ars*bis) / s
    
  end subroutine cdiv

end module cdiv_module
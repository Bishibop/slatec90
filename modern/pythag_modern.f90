module pythag_module
  implicit none
  private
  public :: pythag

contains

  pure function pythag(a, b) result(res)
    implicit none
    real, intent(in) :: a, b
    real :: res
    real :: p, q, r, s, t

    ! Set p to the larger absolute value and q to the smaller.
    p = max(abs(a), abs(b))
    q = min(abs(a), abs(b))
    
    if (q == 0.0) then
      res = p
      return
    end if

    ! Loop until convergence condition is met.
    do
      r = (q / p)**2
      t = 4.0 + r
      if (t == 4.0) exit
      s = r / t
      p = p + 2.0 * p * s
      q = q * s
    end do

    res = p
  end function pythag

end module pythag_module
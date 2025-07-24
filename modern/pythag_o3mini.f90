module slatec_pythag_module
  implicit none
contains

  pure function PYTHAG(a, b) result(res)
    implicit none
    real, intent(in) :: a, b
    real :: res
    real :: p, q, r, s, t

    ! Assign maximum and minimum absolute values to p and q
    p = max(abs(a), abs(b))
    q = min(abs(a), abs(b))

    if (q == 0.0) then
      res = p
      return
    end if

    ! Iteratively refine p until corrections become negligible
    do
      r = (q / p)**2
      t = 4.0 + r
      if (t == 4.0) exit
      s = r / t
      p = p + 2.0 * p * s
      q = q * s
    end do

    res = p
  end function PYTHAG

end module slatec_pythag_module

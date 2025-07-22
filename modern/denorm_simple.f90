module denorm_module
  implicit none
  integer, parameter :: dp = selected_real_kind(15, 307)
  private
  public :: denorm, denorm_f77

contains

  function denorm(n, x) result(norm)
    implicit none
    integer, intent(in) :: n
    real(dp), intent(in) :: x(:)
    real(dp) :: norm
    integer :: i
    real(dp) :: s1, s2, s3, x1max, x3max, xabs, floatn, agiant
    real(dp), parameter :: one = 1.0_dp, zero = 0.0_dp
    real(dp), parameter :: rdwarf = 3.834e-20_dp, rgiant = 1.304e19_dp

    s1 = zero
    s2 = zero
    s3 = zero
    x1max = zero
    x3max = zero
    floatn = real(n, dp)
    agiant = rgiant / floatn

    do i = 1, n
      xabs = abs(x(i))
      if ( xabs > rdwarf .and. xabs < agiant ) then
        ! Intermediate components.
        s2 = s2 + xabs**2
      else if ( xabs <= rdwarf ) then
        ! Small components.
        if ( xabs > x3max ) then
          if ( xabs /= zero ) then
            s3 = one + s3 * (x3max/xabs)**2
            x3max = xabs
          end if
        else
          if ( x3max /= zero .and. xabs /= zero ) then
            s3 = s3 + (xabs/x3max)**2
          end if
        end if
      else
        ! Large components (xabs >= agiant).
        if ( xabs > x1max ) then
          if ( x1max == zero ) then
            s1 = one
          else
            s1 = one + s1 * (x1max/xabs)**2
          end if
          x1max = xabs
        else
          if ( x1max /= zero ) then
            s1 = s1 + (xabs/x1max)**2
          end if
        end if
      end if
    end do

    if ( s1 /= zero ) then
      norm = x1max * sqrt( s1 + s2/(x1max*x1max) )
    else
      if ( s2 /= zero ) then
        if ( s2 >= x3max ) then
          norm = sqrt( s2 * ( one + (x3max/s2) * (x3max*s3) ) )
        else
          norm = sqrt( x3max * ( (s2/x3max) + (x3max*s3) ) )
        end if
      else
        norm = x3max * sqrt(s3)
      end if
    end if

  end function denorm

  ! F77-compatible wrapper function.
  function denorm_f77(n, x) result(norm)
    implicit none
    integer, intent(in) :: n
    real(dp), intent(in) :: x(*)
    real(dp) :: norm
    norm = denorm(n, x(1:n))
  end function denorm_f77

end module denorm_module

! F77 wrapper outside module
double precision function denorm(n, x)
  use denorm_module, only: denorm_f77
  implicit none
  integer n
  double precision x(*)
  denorm = denorm_f77(n, x)
end function denorm
module BSplineModule
  implicit none
  
  ! Module variables to maintain state between calls
  integer, save :: j = 1
  real, dimension(20), save :: deltam = 0.0
  real, dimension(20), save :: deltap = 0.0
  
  public :: BSPLVN

contains

  subroutine BSPLVN(t, jhigh, index, x, ileft, vnikx)
    implicit none
    ! Arguments
    integer, intent(in) :: jhigh
    integer, intent(in) :: index
    integer, intent(in) :: ileft
    real,    intent(in) :: x
    real,    intent(in) :: t(*)  ! Changed from t(:) to t(*)
    real,    intent(inout) :: vnikx(*)  ! Changed from vnikx(:) to vnikx(*)

    ! Local variables
    integer :: ipj, imjp1, jp1, JP1ML, L
    real :: vmprev, vm

    select case (index)
      case (1)
        ! Initialization call: start with j = 1 and set first basis function
        j = 1
        vnikx(1) = 1.0
        if (j >= jhigh) then
          return
        end if
        ! Fall through to computation
      case (2)
        ! Continuation call: state maintained in module variable j
        ! Continue with saved state
      case default
        return
    end select

    ! Main computation loop
    do while (j < jhigh)
      ipj = ileft + j
      deltap(j) = t(ipj) - x

      imjp1 = ileft - j + 1
      deltam(j) = x - t(imjp1)

      vmprev = 0.0
      jp1 = j + 1

      do L = 1, j
        JP1ML = jp1 - L
        vm = vnikx(L) / (deltap(L) + deltam(JP1ML))
        vnikx(L) = vm * deltap(L) + vmprev
        vmprev = vm * deltam(JP1ML)
      end do
      vnikx(jp1) = vmprev
      j = jp1
    end do

    return
  end subroutine BSPLVN

end module BSplineModule
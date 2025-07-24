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
    real,    intent(in) :: t(:)
    real,    intent(inout) :: vnikx(:)

    ! Local variables
    integer :: ipj, imjp1, jp1, JP1ML, L
    real :: vmprev, vm, denom

    select case (index)
      case (1)
        ! Initialization call: start with j = 1 and set first basis function
        j = 1
        vnikx(1) = 1.0
        if (j >= jhigh) then
          return
        end if
      case (2)
        ! Continuation call: state maintained in module variable j
        ! No initialization needed
      case default
        print *, "Error in BSPLVN: Invalid index value (", index, ") encountered. Expected index=1 or 2."
        return
    end select

    do while (j < jhigh)
      ipj = ileft + j
      ! Compute the forward difference: distance between knot at position ipj and x
      deltap(j) = t(ipj) - x

      imjp1 = ileft - j + 1
      ! Compute the backward difference: distance between x and knot at position imjp1
      deltam(j) = x - t(imjp1)

      vmprev = 0.0
      jp1 = j + 1

      do L = 1, j
        JP1ML = jp1 - L
        denom = deltap(L) + deltam(JP1ML)
        ! It is assumed that denom is non-zero; otherwise, division-by-zero issues may occur.
        vm = vnikx(L) / denom
        vnikx(L) = vm * deltap(L) + vmprev
        vmprev = vm * deltam(JP1ML)
      end do
      vnikx(jp1) = vmprev
      j = jp1
    end do

    return
  end subroutine BSPLVN

end module BSplineModule

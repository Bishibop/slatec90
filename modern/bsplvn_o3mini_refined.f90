module bsplvn_module
  implicit none
  private
  public :: bsplvn
  
  ! Module variables to maintain state between calls
  integer, save :: j = 1
  real, dimension(20), save :: deltam = 0.0
  real, dimension(20), save :: deltap = 0.0

contains

  subroutine bsplvn(t, jhigh, index, x, ileft, vnikx)
    implicit none
    ! Arguments with assumed size arrays
    integer, intent(in) :: jhigh
    integer, intent(in) :: index
    integer, intent(in) :: ileft
    real,    intent(in) :: x
    real,    intent(in) :: t(*)
    real,    intent(inout) :: vnikx(*)

    ! Local variables
    integer :: ipj, imjp1, jp1, JP1ML, L
    real :: vmprev, vm, denom

    ! Handle the call type: initialization (index==1) or continuation (index==2)
    if (index == 1) then
      ! Initialize the state variables
      j = 1
      vnikx(1) = 1.0
      if (j >= jhigh) return  ! Early return if already at desired order
    else if (index == 2) then
      ! Continuation call; state (j, deltam, deltap) already stored
      ! No reinitialization required
    else
      return  ! Invalid index
    end if

    ! Compute B-spline values for orders up to JHIGH
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
        ! Assumes denom is non-zero as in the original algorithm
        vm = vnikx(L) / denom
        vnikx(L) = vm * deltap(L) + vmprev
        vmprev = vm * deltam(JP1ML)
      end do
      vnikx(jp1) = vmprev
      j = jp1
    end do

    return
  end subroutine bsplvn

end module bsplvn_module
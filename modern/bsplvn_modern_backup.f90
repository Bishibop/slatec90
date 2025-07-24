module bsplvn_module
    implicit none
    private
    public :: bsplvn
    
    ! Module variables to replace SAVE variables
    integer :: j_saved = 1
    real :: deltam_saved(20) = 0.0
    real :: deltap_saved(20) = 0.0
    logical :: first_call = .true.
    
contains
    
    subroutine bsplvn(t, jhigh, index, x, ileft, vnikx)
        ! Calculates the value of all possibly nonzero B-splines at X of
        ! order MAX(JHIGH,(J+1)(INDEX-1)) on T.
        
        ! Arguments
        real, intent(in) :: t(*)          ! Knot sequence
        integer, intent(in) :: jhigh      ! Order of B-spline
        integer, intent(in) :: index      ! 1 = initialize, 2 = continue
        real, intent(in) :: x             ! Evaluation point
        integer, intent(in) :: ileft      ! Left index
        real, intent(inout) :: vnikx(*)   ! Output B-spline values
        
        ! Local variables
        integer :: j, ipj, imjp1, jp1, l, jp1ml
        real :: vmprev, vm
        real :: deltam(20), deltap(20)
        
        ! IMPORTANT: The input ileft is 0-based (from test data)
        ! but F77 arrays are 1-based, so we need to add 1
        
        ! Handle INDEX parameter
        if (index == 1) then
            ! Initialize
            j = 1
            vnikx(1) = 1.0
            if (j >= jhigh) then
                j_saved = j
                return
            end if
            ! Initialize saved arrays for first call
            deltam_saved = 0.0
            deltap_saved = 0.0
        else if (index == 2) then
            ! Continue from saved state
            j = j_saved
            deltam = deltam_saved
            deltap = deltap_saved
        else
            ! Invalid index
            return
        end if
        
        ! Main computation loop - execute at least once if we get here
        computation_loop: do
            ipj = (ileft + 1) + j  ! Convert ileft to 1-based
            deltap(j) = t(ipj) - x
            imjp1 = (ileft + 1) - j + 1  ! Convert ileft to 1-based
            deltam(j) = x - t(imjp1)
            vmprev = 0.0
            jp1 = j + 1
            
            do l = 1, j
                jp1ml = jp1 - l
                vm = vnikx(l) / (deltap(l) + deltam(jp1ml))
                vnikx(l) = vm * deltap(l) + vmprev
                vmprev = vm * deltam(jp1ml)
            end do
            
            vnikx(jp1) = vmprev
            j = jp1
            if (j >= jhigh) exit computation_loop
        end do computation_loop
        
        ! Save state for next call
        j_saved = j
        deltam_saved = deltam
        deltap_saved = deltap
        
    end subroutine bsplvn
    
end module bsplvn_module
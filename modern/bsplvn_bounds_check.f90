module bsplvn_module
    implicit none
    private
    public :: bsplvn
    
    ! Module variables to emulate F77 SAVE statement
    integer :: j_save = 1
    real :: deltam_save(20) = 0.0
    real :: deltap_save(20) = 0.0
    
contains
    
    subroutine bsplvn(t, jhigh, index, x, ileft, vnikx)
        ! Calculates the value of all possibly nonzero B-splines at X of
        ! order MAX(JHIGH,(J+1)(INDEX-1)) on T.
        
        real, intent(in) :: t(*)          ! Knot sequence
        integer, intent(in) :: jhigh      ! Order of B-spline
        integer, intent(in) :: index      ! 1 = initialize, 2 = continue
        real, intent(in) :: x             ! Evaluation point
        integer, intent(in) :: ileft      ! Left index (1-based)
        real, intent(inout) :: vnikx(*)   ! Output B-spline values
        
        ! Local variables
        integer :: j, ipj, imjp1, jp1, l, jp1ml
        real :: vmprev, vm, denom
        real :: deltam(20), deltap(20)
        real :: t_safe  ! For safe array access
        
        ! Emulate F77 computed GOTO: GO TO (10,20),INDEX
        if (index == 1) then
            ! Reset saved state when starting new computation
            j_save = 1
            deltam_save = 0.0
            deltap_save = 0.0
            goto 10
        else if (index == 2) then
            goto 20
        else
            return
        end if
        
10      continue
        ! Initialize for new evaluation
        j = 1
        vnikx(1) = 1.0
        if (j >= jhigh) goto 99
        ! Fall through to computation
        
20      continue
        ! Load saved state if INDEX=2
        if (index == 2) then
            j = j_save
            deltam(1:20) = deltam_save(1:20)
            deltap(1:20) = deltap_save(1:20)
        else
            ! INDEX=1, use local j from label 10
            deltam = 0.0
            deltap = 0.0
        end if
        
        ! Main computation loop
25      continue
        ! ileft is already 1-based from the input
        ipj = ileft + j
        deltap(j) = t(ipj) - x
        
        imjp1 = ileft - j + 1
        ! Handle potential out-of-bounds access
        if (imjp1 < 1) then
            ! When imjp1 < 1, we're beyond the left boundary
            ! The F77 code might be relying on undefined behavior
            ! For now, assume t(imjp1) = t(1) for imjp1 < 1
            t_safe = t(1)
        else
            t_safe = t(imjp1)
        end if
        deltam(j) = x - t_safe
        
        vmprev = 0.0
        jp1 = j + 1
        
        ! Cox-de Boor recursion
        do l = 1, j
            jp1ml = jp1 - l
            denom = deltap(l) + deltam(jp1ml)
            
            if (abs(denom) < 1e-10) then
                ! Handle division by zero - set to 0
                vm = 0.0
            else
                vm = vnikx(l) / denom
            end if
            
            vnikx(l) = vm * deltap(l) + vmprev
            vmprev = vm * deltam(jp1ml)
        end do
        
        vnikx(jp1) = vmprev
        j = jp1
        if (j < jhigh) goto 25
        
99      continue
        ! Save state
        j_save = j
        deltam_save(1:20) = deltam(1:20)
        deltap_save(1:20) = deltap(1:20)
        return
        
    end subroutine bsplvn
    
end module bsplvn_module
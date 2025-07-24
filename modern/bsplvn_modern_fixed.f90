module bsplvn_module
    implicit none
    private
    public :: bsplvn
    
    ! Module variables to emulate F77 SAVE statement
    ! These need to be reset appropriately to avoid cross-test contamination
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
        integer, intent(in) :: ileft      ! Left index (0-based from input)
        real, intent(inout) :: vnikx(*)   ! Output B-spline values
        
        ! Local variables
        integer :: j, ipj, imjp1, jp1, l, jp1ml
        real :: vmprev, vm
        real :: deltam(20), deltap(20)
        
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
        ! CRITICAL: ileft is 0-based in input, but F77 arrays are 1-based
        ! So we need to add 1 when accessing array t
        ipj = ileft + j
        deltap(j) = t(ipj + 1) - x      ! +1 for 1-based array access
        imjp1 = ileft - j + 1
        deltam(j) = x - t(imjp1 + 1)    ! +1 for 1-based array access
        vmprev = 0.0
        jp1 = j + 1
        
        ! Cox-de Boor recursion
        do l = 1, j
            jp1ml = jp1 - l
            vm = vnikx(l) / (deltap(l) + deltam(jp1ml))
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
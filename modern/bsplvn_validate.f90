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
        real :: sum_check
        integer :: k
        
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
        ! Handle potential array bounds issue
        if (imjp1 < 1) then
            ! Beyond left boundary - set deltam to large value
            deltam(j) = x
        else
            deltam(j) = x - t(imjp1)
        end if
        vmprev = 0.0
        jp1 = j + 1
        
        ! Cox-de Boor recursion
        do l = 1, j
            jp1ml = jp1 - l
            ! Handle division by zero: if denominator is 0, use convention 0/0 = 0
            denom = deltap(l) + deltam(jp1ml)
            if (abs(denom) < 1e-30) then
                vm = 0.0
            else
                vm = vnikx(l) / denom
            end if
            vnikx(l) = vm * deltap(l) + vmprev
            vmprev = vm * deltam(jp1ml)
        end do
        
        vnikx(jp1) = vmprev
        
        ! VALIDATION: Check for invalid values after each iteration
        do k = 1, jp1
            if (vnikx(k) < 0.0 .or. vnikx(k) > 1.0) then
                print *, "WARNING at j=", j, ": vnikx(", k, ")=", vnikx(k), "is invalid!"
            end if
        end do
        
        j = jp1
        if (j < jhigh) goto 25
        
99      continue
        ! Final validation
        sum_check = sum(vnikx(1:jhigh))
        if (abs(sum_check - 1.0) > 1e-6) then
            print *, "WARNING: Partition of unity violated! Sum=", sum_check
        end if
        
        ! Save state
        j_save = j
        deltam_save(1:20) = deltam(1:20)
        deltap_save(1:20) = deltap(1:20)
        return
        
    end subroutine bsplvn
    
end module bsplvn_module
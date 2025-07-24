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
        integer, intent(in) :: ileft      ! Left index (0-based from input)
        real, intent(inout) :: vnikx(*)   ! Output B-spline values
        
        ! Local variables
        integer :: j, ipj, imjp1, jp1, l, jp1ml
        real :: vmprev, vm
        real :: deltam(20), deltap(20)
        
        print *, "BSPLVN called with:"
        print *, "  jhigh =", jhigh
        print *, "  index =", index
        print *, "  x =", x
        print *, "  ileft =", ileft
        
        ! Emulate F77 computed GOTO: GO TO (10,20),INDEX
        if (index == 1) then
            print *, "Going to label 10 (INDEX=1)"
            goto 10
        else if (index == 2) then
            print *, "Going to label 20 (INDEX=2)"
            goto 20
        else
            print *, "Invalid INDEX, returning"
            return
        end if
        
10      continue
        print *, "At label 10: Initialize"
        ! Initialize for new evaluation
        j = 1
        vnikx(1) = 1.0
        print *, "Set j=1, vnikx(1)=1.0"
        print *, "Checking: j=", j, "jhigh=", jhigh
        if (j >= jhigh) then
            print *, "j >= jhigh, going to 99"
            goto 99
        end if
        print *, "j < jhigh, continuing to computation"
        ! Fall through to label 20
        
20      continue
        print *, "At label 20"
        ! Load saved state if INDEX=2
        if (index == 2) then
            print *, "Loading saved state (INDEX=2)"
            j = j_save
            deltam = deltam_save
            deltap = deltap_save
        end if
        
        ! Main computation loop
25      continue
        print *, "At label 25: Main computation, j=", j
        ! CRITICAL: ileft is 0-based in input, but F77 arrays are 1-based
        ! So we need to add 1 when accessing array t
        ipj = ileft + j
        deltap(j) = t(ipj + 1) - x      ! +1 for 1-based array access
        imjp1 = ileft - j + 1
        deltam(j) = x - t(imjp1 + 1)    ! +1 for 1-based array access
        
        print *, "  ipj=", ipj, "t(ipj+1)=", t(ipj+1)
        print *, "  imjp1=", imjp1, "t(imjp1+1)=", t(imjp1+1)
        print *, "  deltap(", j, ")=", deltap(j)
        print *, "  deltam(", j, ")=", deltam(j)
        
        vmprev = 0.0
        jp1 = j + 1
        
        ! Cox-de Boor recursion
        print *, "  Starting Cox-de Boor recursion, j=", j
        do l = 1, j
            jp1ml = jp1 - l
            vm = vnikx(l) / (deltap(l) + deltam(jp1ml))
            print *, "    l=", l, "vm=", vm, "vnikx(l)=", vnikx(l)
            print *, "    deltap(l)=", deltap(l), "deltam(jp1ml)=", deltam(jp1ml)
            vnikx(l) = vm * deltap(l) + vmprev
            vmprev = vm * deltam(jp1ml)
            print *, "    Updated vnikx(", l, ")=", vnikx(l)
        end do
        
        vnikx(jp1) = vmprev
        print *, "  Set vnikx(", jp1, ")=", vmprev
        j = jp1
        print *, "  j incremented to", j
        if (j < jhigh) then
            print *, "  j < jhigh, going back to 25"
            goto 25
        end if
        print *, "  j >= jhigh, done with computation"
        
99      continue
        print *, "At label 99: Save state and return"
        ! Save state
        j_save = j
        deltam_save = deltam
        deltap_save = deltap
        print *, "Final vnikx values:"
        do l = 1, jhigh
            print *, "  vnikx(", l, ")=", vnikx(l)
        end do
        return
        
    end subroutine bsplvn
    
end module bsplvn_module
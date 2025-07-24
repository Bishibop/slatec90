module intrv_debug_module
    implicit none
    private
    public :: intrv_debug
    
contains
    
    subroutine intrv_debug(xt, lxt, x, ilo, ileft, mflag, test_num)
        real, intent(in) :: xt(*)
        integer, intent(in) :: lxt
        real, intent(in) :: x
        integer, intent(inout) :: ilo
        integer, intent(out) :: ileft
        integer, intent(out) :: mflag
        integer, intent(in) :: test_num
        
        ! Local variables
        integer :: ihi, istep, middle
        logical :: debug
        
        debug = (test_num == 254 .or. test_num == 255)
        
        if (debug) then
            print *, "=== DEBUG Test", test_num, "==="
            print *, "Initial ILO =", ilo
            print *, "X =", x
        end if
        
        ! Initialize ihi based on ilo
        ihi = ilo + 1
        if (debug) print *, "Initial IHI =", ihi
        
        ! Check if we need to adjust the initial range
        if (ihi >= lxt) then
            if (debug) print *, "IHI >= LXT check"
            if (x >= xt(lxt)) then
                if (debug) print *, "X >= XT(LXT), returning ILEFT=LXT"
                mflag = 1
                ileft = lxt
                return
            end if
            if (lxt <= 1) then
                if (debug) print *, "LXT <= 1, returning ILEFT=1"
                mflag = -1
                ileft = 1
                return
            end if
            ! Adjust to last interval
            ilo = lxt - 1
            ihi = lxt
            if (debug) print *, "Adjusted: ILO =", ilo, ", IHI =", ihi
        end if
        
        ! Check comparisons
        if (debug) then
            print *, "Checking: X >= XT(IHI)?"
            print *, "  X =", x, ", XT(", ihi, ") =", xt(ihi)
            print *, "  Result:", (x >= xt(ihi))
            print *, "Checking: X >= XT(ILO)?"
            print *, "  X =", x, ", XT(", ilo, ") =", xt(ilo)
            print *, "  Result:", (x >= xt(ilo))
        end if
        
        ! Check if x is already in the interval [xt(ilo), xt(ihi))
        if (x >= xt(ihi)) then
            if (debug) print *, "Taking upward search path"
            ! Need to search upward
            istep = 1
            do
                ilo = ihi
                ihi = ilo + istep
                if (ihi >= lxt) then
                    if (x >= xt(lxt)) then
                        mflag = 1
                        ileft = lxt
                        return
                    end if
                    ihi = lxt
                    exit
                end if
                if (x < xt(ihi)) exit
                istep = istep * 2
            end do
        else if (x >= xt(ilo)) then
            if (debug) print *, "X already in interval, returning ILEFT=ILO"
            ! Found the interval already
            mflag = 0
            ileft = ilo
            return
        else
            if (debug) print *, "Taking downward search path"
            ! Need to search downward
            istep = 1
            do
                ihi = ilo
                ilo = ihi - istep
                if (ilo <= 1) then
                    ilo = 1
                    if (x < xt(1)) then
                        mflag = -1
                        ileft = 1
                        return
                    end if
                    exit
                end if
                if (x >= xt(ilo)) exit
                istep = istep * 2
            end do
        end if
        
        ! Now we have xt(ilo) <= x < xt(ihi), narrow the interval with binary search
        if (debug) print *, "Binary search: ILO =", ilo, ", IHI =", ihi
        do while (.true.)
            middle = (ilo + ihi) / 2
            if (debug) print *, "  Middle =", middle
            if (middle == ilo) then
                if (debug) print *, "  Done! ILEFT =", ilo
                ! Interval is narrowed down
                mflag = 0
                ileft = ilo
                return
            end if
            if (x < xt(middle)) then
                if (debug) print *, "  X < XT(MIDDLE), IHI = MIDDLE"
                ihi = middle
            else
                if (debug) print *, "  X >= XT(MIDDLE), ILO = MIDDLE"
                ilo = middle
            end if
        end do
        
    end subroutine intrv_debug
    
end module intrv_debug_module
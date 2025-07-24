module intrv_module
    implicit none
    private
    public :: intrv
    
contains
    
    subroutine intrv(xt, lxt, x, ilo, ileft, mflag)
        ! Find interval in sorted array containing given value
        ! INTRV computes the largest integer ILEFT in 1 <= ILEFT <= LXT
        ! such that XT(ILEFT) <= X where XT(*) is a subdivision of the X interval
        
        real, intent(in) :: xt(*)
        integer, intent(in) :: lxt
        real, intent(in) :: x
        integer, intent(inout) :: ilo
        integer, intent(out) :: ileft
        integer, intent(out) :: mflag
        
        ! Local variables
        integer :: ihi, istep, middle
        
        ! Initialize ihi based on ilo
        ihi = ilo + 1
        
        ! Check if we need to adjust the initial range
        if (ihi >= lxt) then
            if (x >= xt(lxt)) then
                ! X is at or beyond the last element
                mflag = 1
                ileft = lxt
                return
            end if
            if (lxt <= 1) then
                ! Array has only one element and x < xt(1)
                mflag = -1
                ileft = 1
                return
            end if
            ! Adjust to last interval
            ilo = lxt - 1
            ihi = lxt
        end if
        
        ! Check if x is already in the interval [xt(ilo), xt(ihi))
        if (x >= xt(ihi)) then
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
            ! Found the interval already
            mflag = 0
            ileft = ilo
            return
        else
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
        do while (.true.)
            middle = (ilo + ihi) / 2
            if (middle == ilo) then
                ! Interval is narrowed down
                mflag = 0
                ileft = ilo
                return
            end if
            if (x < xt(middle)) then
                ihi = middle
            else
                ilo = middle
            end if
        end do
        
    end subroutine intrv
    
end module intrv_module
module cshch_module
    implicit none
    private
    public :: cshch
    
contains
    
    subroutine cshch(z, csh, cch)
        !***BEGIN PROLOGUE  CSHCH
        !***PURPOSE  Compute complex hyperbolic sine and cosine
        !***LIBRARY   SLATEC
        !***TYPE      ALL (CSHCH-A, ZSHCH-A)
        !***AUTHOR  Amos, D. E., (SNL)
        !***DESCRIPTION
        !
        !     CSHCH COMPUTES THE COMPLEX HYPERBOLIC FUNCTIONS CSH=SINH(X+I*Y)
        !     AND CCH=COSH(X+I*Y), WHERE I**2=-1.
        !
        !***END PROLOGUE  CSHCH
        
        ! Arguments
        complex, intent(in) :: z
        complex, intent(out) :: csh, cch
        
        ! Local variables
        real :: cchi, cchr, ch, cn, cshi, cshr, sh, sn, x, y
        real :: max_val
        real, parameter :: overflow_threshold = 88.0  ! log(huge(1.0))/2 ≈ 88
        
        ! Extract real and imaginary parts
        x = real(z)
        y = aimag(z)
        
        ! Handle overflow cases for large |x|
        if (abs(x) > overflow_threshold) then
            ! For very large |x|, we need to handle overflow carefully
            ! Get the maximum representable value
            max_val = huge(1.0) * 0.5  ! Leave some headroom
            
            ! Get trigonometric values
            sn = sin(y)
            cn = cos(y)
            
            ! For very large x, just return the maximum representable value
            ! scaled by the trigonometric functions
            if (x > 0) then
                ! Positive x
                cshr = max_val * cn
                cshi = max_val * sn
                cchr = max_val * cn
                cchi = max_val * sn
            else
                ! Negative x: sinh changes sign, cosh doesn't
                cshr = -max_val * cn
                cshi = -max_val * sn
                cchr = max_val * cn
                cchi = -max_val * sn
            end if
            
            csh = cmplx(cshr, cshi)
            cch = cmplx(cchr, cchi)
        else
            ! Normal case - no overflow
            ! Use the original F77 algorithm exactly
            sh = sinh(x)
            ch = cosh(x)
            sn = sin(y)
            cn = cos(y)
            
            ! Complex sinh components
            cshr = sh * cn
            cshi = ch * sn
            csh = cmplx(cshr, cshi)
            
            ! Complex cosh components
            cchr = ch * cn
            cchi = sh * sn
            cch = cmplx(cchr, cchi)
        end if
        
    end subroutine cshch
    
end module cshch_module
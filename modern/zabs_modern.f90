module zabs_module
    implicit none
    private
    public :: zabs

contains

    double precision function zabs(zr, zi)
        !***BEGIN PROLOGUE  ZABS
        !***SUBSIDIARY
        !***PURPOSE  Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and
        !            ZBIRY
        !***LIBRARY   SLATEC
        !***TYPE      ALL (ZABS-A)
        !***AUTHOR  Amos, D. E., (SNL)
        !***DESCRIPTION
        !
        !     ZABS COMPUTES THE ABSOLUTE VALUE OR MAGNITUDE OF A DOUBLE
        !     PRECISION COMPLEX VARIABLE CMPLX(ZR,ZI)
        !
        !***SEE ALSO  ZAIRY, ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZBIRY
        !***ROUTINES CALLED  (NONE)
        !***REVISION HISTORY  (YYMMDD)
        !   830501  DATE WRITTEN
        !   910415  Prologue converted to Version 4.0 format.  (BAB)
        !   Modern Fortran version preserving original algorithm
        !***END PROLOGUE  ZABS
        
        ! Arguments
        double precision, intent(in) :: zr, zi
        
        ! Local variables
        double precision :: u, v, q, s
        
        !***FIRST EXECUTABLE STATEMENT  ZABS
        u = abs(zr)
        v = abs(zi)
        s = u + v
        
        !-----------------------------------------------------------------------
        !     S*1.0D0 MAKES AN UNNORMALIZED UNDERFLOW ON CDC MACHINES INTO A
        !     TRUE FLOATING ZERO
        !-----------------------------------------------------------------------
        s = s * 1.0d+0
        
        if (s == 0.0d+0) then
            zabs = 0.0d+0
            return
        end if
        
        if (u > v) then
            q = v / u
            zabs = u * sqrt(1.0d+0 + q*q)
        else
            q = u / v
            zabs = v * sqrt(1.0d+0 + q*q)
        end if
        
    end function zabs

end module zabs_module
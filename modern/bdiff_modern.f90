module bdiff_module
    implicit none
    private
    public :: bdiff
    
contains
    
    subroutine bdiff(l, v)
        !***BEGIN PROLOGUE  BDIFF
        !***SUBSIDIARY
        !***PURPOSE  Subsidiary to BSKIN
        !***LIBRARY   SLATEC
        !***TYPE      SINGLE PRECISION (BDIFF-S, DBDIFF-D)
        !***AUTHOR  Amos, D. E., (SNLA)
        !***DESCRIPTION
        !
        !     BDIFF computes the sum of B(L,K)*V(K)*(-1)**K where B(L,K)
        !     are the binomial coefficients.  Truncated sums are computed by
        !     setting last part of the V vector to zero. On return, the binomial
        !     sum is in V(L).
        !
        !***SEE ALSO  BSKIN
        !***ROUTINES CALLED  (NONE)
        !***REVISION HISTORY  (YYMMDD)
        !   820601  DATE WRITTEN
        !   891214  Prologue converted to Version 4.0 format.  (BAB)
        !   900328  Added TYPE section.  (WRB)
        !   241223  Converted to modern Fortran 90
        !***END PROLOGUE  BDIFF
        
        ! Arguments
        integer, intent(in) :: l
        real, dimension(*), intent(inout) :: v
        
        ! Local variables
        integer :: i, j, k
        
        !***FIRST EXECUTABLE STATEMENT  BDIFF
        if (l == 1) return
        
        do j = 2, l
            k = l
            do i = j, l
                v(k) = v(k-1) - v(k)
                k = k - 1
            end do
        end do
        
    end subroutine bdiff
    
end module bdiff_module
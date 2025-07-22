module denorm_module
    implicit none
    private
    public :: denorm
    
    ! Constants for overflow/underflow protection
    double precision, parameter :: ONE = 1.0D0
    double precision, parameter :: ZERO = 0.0D0
    double precision, parameter :: RDWARF = 3.834D-20
    double precision, parameter :: RGIANT = 1.304D19
    
contains
    
    double precision function denorm(n, x)
        !***BEGIN PROLOGUE  DENORM
        !***PURPOSE  Calculate the Euclidean norm of a double precision vector
        !***DESCRIPTION
        !
        !     Given an N-vector X, this function calculates the
        !     Euclidean norm of X.
        !
        !     The Euclidean norm is computed by accumulating the sum of
        !     squares in three different sums. The sums of squares for the
        !     small and large components are scaled so that no overflows
        !     occur. Non-destructive underflows are permitted. Underflows
        !     and overflows do not occur in the computation of the unscaled
        !     sum of squares for the intermediate components.
        !     The definitions of small, intermediate and large components
        !     depend on two constants, RDWARF and RGIANT. The main
        !     restrictions on these constants are that RDWARF**2 not
        !     underflow and RGIANT**2 not overflow. The constants
        !     given here are suitable for every known computer.
        !
        !     The function statement is
        !
        !       DOUBLE PRECISION FUNCTION DENORM(N,X)
        !
        !     where
        !
        !       N is a positive integer input variable.
        !
        !       X is an input array of length N.
        !
        !***END PROLOGUE  DENORM
        
        ! Arguments
        integer, intent(in) :: n
        double precision, intent(in) :: x(*)
        
        ! Local variables
        integer :: i
        double precision :: agiant, floatn, s1, s2, s3
        double precision :: x1max, x3max, xabs
        
        ! Initialize sums and maxima
        s1 = ZERO
        s2 = ZERO
        s3 = ZERO
        x1max = ZERO
        x3max = ZERO
        
        ! Handle special case for N <= 0
        if (n <= 0) then
            denorm = ZERO
            return
        end if
        
        ! Compute AGIANT to prevent overflow
        floatn = n
        agiant = RGIANT / floatn
        
        ! Main loop over vector elements - matching F77 logic exactly
        do i = 1, n
            xabs = abs(x(i))
            
            ! Check for intermediate components first
            if (xabs > RDWARF .and. xabs < agiant) then
                ! Sum for intermediate components
                s2 = s2 + xabs**2
            else if (xabs <= RDWARF) then
                ! Sum for small components
                if (xabs > x3max) then
                    s3 = ONE + s3 * (x3max/xabs)**2
                    x3max = xabs
                else
                    if (xabs /= ZERO) s3 = s3 + (xabs/x3max)**2
                end if
            else
                ! Sum for large components (xabs >= agiant)
                if (xabs > x1max) then
                    s1 = ONE + s1 * (x1max/xabs)**2
                    x1max = xabs
                else
                    s1 = s1 + (xabs/x1max)**2
                end if
            end if
        end do
        
        ! Calculation of norm - matching F77 exactly
        if (s1 /= ZERO) then
            denorm = x1max * sqrt(s1 + (s2/x1max)/x1max)
        else if (s2 /= ZERO) then
            if (s2 >= x3max) then
                denorm = sqrt(s2 * (ONE + (x3max/s2) * (x3max * s3)))
            else
                denorm = sqrt(x3max * ((s2/x3max) + (x3max * s3)))
            end if
        else
            denorm = x3max * sqrt(s3)
        end if
        
    end function denorm
    
end module denorm_module
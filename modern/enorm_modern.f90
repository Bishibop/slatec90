module enorm_module
    implicit none
    private
    public :: enorm
    
    ! Constants from the original SLATEC implementation
    real, parameter :: ONE = 1.0E0
    real, parameter :: ZERO = 0.0E0
    real, parameter :: RDWARF = 3.834E-20  ! Threshold for small components
    real, parameter :: RGIANT = 1.304E19   ! Threshold for large components
    
contains
    
    real function enorm(n, x)
        !***BEGIN PROLOGUE  ENORM
        !***PURPOSE  Calculate the Euclidean norm of a vector with overflow/underflow protection
        !***LIBRARY   SLATEC (modernized)
        !***TYPE      SINGLE PRECISION
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
        !
        !***END PROLOGUE  ENORM
        
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: x
        
        ! Local variables
        integer :: i
        real :: agiant, floatn, s1, s2, s3, xabs, x1max, x3max
        
        ! Initialize sums and maxima
        s1 = ZERO
        s2 = ZERO
        s3 = ZERO
        x1max = ZERO
        x3max = ZERO
        
        ! Handle empty vector case
        if (n == 0) then
            enorm = ZERO
            return
        end if
        
        ! Scale the giant threshold by N to prevent overflow in intermediate sum
        floatn = real(n)
        agiant = RGIANT / floatn
        
        ! Main loop over vector components
        do i = 1, n
            xabs = abs(x(i))
            
            ! Check which range the component falls into
            if (xabs > RDWARF .and. xabs < agiant) then
                ! Intermediate components - no scaling needed
                s2 = s2 + xabs**2
            else if (xabs <= RDWARF) then
                ! Small components - scale to prevent underflow
                if (xabs <= x3max) then
                    if (xabs /= ZERO) s3 = s3 + (xabs/x3max)**2
                else
                    s3 = ONE + s3*(x3max/xabs)**2
                    x3max = xabs
                end if
            else
                ! Large components - scale to prevent overflow
                if (xabs <= x1max) then
                    s1 = s1 + (xabs/x1max)**2
                else
                    s1 = ONE + s1*(x1max/xabs)**2
                    x1max = xabs
                end if
            end if
        end do
        
        ! Calculate the norm based on which sums are non-zero
        if (s1 /= ZERO) then
            ! Large components dominate
            enorm = x1max * sqrt(s1 + (s2/x1max)/x1max)
        else if (s2 /= ZERO) then
            ! Intermediate components dominate
            if (s2 >= x3max) then
                enorm = sqrt(s2*(ONE + (x3max/s2)*(x3max*s3)))
            else
                enorm = sqrt(x3max*((s2/x3max) + (x3max*s3)))
            end if
        else
            ! Only small components
            enorm = x3max * sqrt(s3)
        end if
        
    end function enorm
    
end module enorm_module
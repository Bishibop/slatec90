!> Modern implementation of ENORM (Euclidean norm) from SLATEC
!>
!> This module provides overflow/underflow-safe computation of the Euclidean norm
!> using the sophisticated three-sum algorithm from the original SLATEC ENORM.
!>
!> The algorithm partitions vector elements into small, intermediate, and large
!> components to prevent numerical overflow/underflow while maintaining accuracy.
!>
!> @author Original SLATEC implementation (unknown), modernized for F2008+
!> @date 2025-01-22

module enorm_module
    use iso_fortran_env, only: real32, real64
    implicit none
    private

    ! Public interface - generic for all precisions
    public :: euclidean_norm

    ! Generic interface supporting both single and double precision
    interface euclidean_norm
        module procedure enorm_real32, enorm_real64
    end interface euclidean_norm

contains

    !> Compute Euclidean norm with overflow/underflow protection (single precision)
    !>
    !> Uses three-sum algorithm to prevent numerical issues:
    !> - Small components: scaled to prevent underflow
    !> - Intermediate components: computed directly 
    !> - Large components: scaled to prevent overflow
    !>
    !> @param[in] x Input vector
    !> @return Euclidean norm of x
    pure function enorm_real32(x) result(norm)
        real(real32), intent(in) :: x(:)
        real(real32) :: norm

        ! Algorithm constants (from original SLATEC)
        real(real32), parameter :: zero = 0.0_real32
        real(real32), parameter :: one = 1.0_real32
        real(real32), parameter :: rdwarf = 3.834e-20_real32  ! sqrt(tiny)
        real(real32), parameter :: rgiant = 1.304e19_real32   ! sqrt(huge)

        ! Local variables
        real(real32) :: agiant, floatn
        real(real32) :: s1, s2, s3          ! Three sums
        real(real32) :: x1max, x3max        ! Maximum values for scaling
        real(real32) :: xabs
        integer :: i, n

        n = size(x)
        
        ! Initialize sums and maxima
        s1 = zero
        s2 = zero  
        s3 = zero
        x1max = zero
        x3max = zero
        
        ! Adaptive threshold based on vector length
        floatn = real(n, real32)
        agiant = rgiant / floatn

        ! Process each vector element
        do i = 1, n
            xabs = abs(x(i))
            
            if (xabs > rdwarf .and. xabs < agiant) then
                ! Intermediate components - compute directly
                s2 = s2 + xabs**2
                
            else if (xabs <= rdwarf) then
                ! Small components - scale to prevent underflow
                if (xabs <= x3max) then
                    if (xabs /= zero) then
                        s3 = s3 + (xabs / x3max)**2
                    end if
                else
                    s3 = one + s3 * (x3max / xabs)**2
                    x3max = xabs
                end if
                
            else
                ! Large components - scale to prevent overflow
                if (xabs <= x1max) then
                    s1 = s1 + (xabs / x1max)**2
                else
                    s1 = one + s1 * (x1max / xabs)**2
                    x1max = xabs
                end if
            end if
        end do

        ! Combine the three sums to compute final norm
        if (s1 /= zero) then
            norm = x1max * sqrt(s1 + (s2 / x1max) / x1max)
        else if (s2 /= zero) then
            if (s2 >= x3max) then
                norm = sqrt(s2 * (one + (x3max / s2) * (x3max * s3)))
            else
                norm = sqrt(x3max * ((s2 / x3max) + (x3max * s3)))
            end if
        else
            norm = x3max * sqrt(s3)
        end if
    end function enorm_real32

    !> Compute Euclidean norm with overflow/underflow protection (double precision)
    !>
    !> Double precision version of the three-sum algorithm
    !>
    !> @param[in] x Input vector  
    !> @return Euclidean norm of x
    pure function enorm_real64(x) result(norm)
        real(real64), intent(in) :: x(:)
        real(real64) :: norm

        ! Algorithm constants (from original SLATEC DENORM)
        real(real64), parameter :: zero = 0.0_real64
        real(real64), parameter :: one = 1.0_real64
        real(real64), parameter :: rdwarf = 3.834d-20  ! sqrt(tiny)
        real(real64), parameter :: rgiant = 1.304d19   ! sqrt(huge)

        ! Local variables
        real(real64) :: agiant, floatn
        real(real64) :: s1, s2, s3          ! Three sums
        real(real64) :: x1max, x3max        ! Maximum values for scaling
        real(real64) :: xabs
        integer :: i, n

        n = size(x)
        
        ! Initialize sums and maxima
        s1 = zero
        s2 = zero
        s3 = zero
        x1max = zero
        x3max = zero
        
        ! Adaptive threshold based on vector length
        floatn = real(n, real64)
        agiant = rgiant / floatn

        ! Process each vector element
        do i = 1, n
            xabs = abs(x(i))
            
            if (xabs > rdwarf .and. xabs < agiant) then
                ! Intermediate components - compute directly
                s2 = s2 + xabs**2
                
            else if (xabs <= rdwarf) then
                ! Small components - scale to prevent underflow
                if (xabs <= x3max) then
                    if (xabs /= zero) then
                        s3 = s3 + (xabs / x3max)**2
                    end if
                else
                    s3 = one + s3 * (x3max / xabs)**2
                    x3max = xabs
                end if
                
            else
                ! Large components - scale to prevent overflow
                if (xabs <= x1max) then
                    s1 = s1 + (xabs / x1max)**2
                else
                    s1 = one + s1 * (x1max / xabs)**2
                    x1max = xabs
                end if
            end if
        end do

        ! Combine the three sums to compute final norm
        if (s1 /= zero) then
            norm = x1max * sqrt(s1 + (s2 / x1max) / x1max)
        else if (s2 /= zero) then
            if (s2 >= x3max) then
                norm = sqrt(s2 * (one + (x3max / s2) * (x3max * s3)))
            else
                norm = sqrt(x3max * ((s2 / x3max) + (x3max * s3)))
            end if
        else
            norm = x3max * sqrt(s3)
        end if
    end function enorm_real64

end module enorm_module
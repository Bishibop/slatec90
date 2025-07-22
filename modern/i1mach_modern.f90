module i1mach_module
  use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
  use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
  use, intrinsic :: iso_c_binding, only: c_sizeof
  implicit none
  private
  public :: i1mach
  
  ! This module provides machine constants using modern Fortran intrinsics
  ! Values are compatible with IEEE 754 standard systems (most modern computers)
  ! Original SLATEC I1MACH required manual configuration for each machine

contains

  integer function i1mach(i)
    implicit none
    integer, intent(in) :: i
    
    ! Local variables for computing machine constants
    integer :: int_bits, real_digits, real_minexp, real_maxexp
    integer :: dble_digits, dble_minexp, dble_maxexp
    
    select case(i)
      ! I/O unit numbers
      case(1)
        i1mach = input_unit      ! Standard input (usually 5, but system-dependent)
      case(2)
        i1mach = output_unit     ! Standard output (usually 6)
      case(3)
        i1mach = output_unit     ! Standard punch (obsolete, use output)
      case(4)
        i1mach = error_unit      ! Standard error
        
      ! Words
      case(5)
        i1mach = bit_size(1)     ! Bits per integer storage unit
      case(6)
        i1mach = 4               ! Characters per integer (assuming 32-bit int)
        
      ! Integers - assume 32-bit integers by default
      case(7)
        i1mach = 2               ! Base
      case(8)
        i1mach = 31              ! Number of base-2 digits (sign bit excluded)
      case(9)
        i1mach = huge(1)         ! Largest magnitude (2^31 - 1)
        
      ! Floating-Point Base
      case(10)
        i1mach = radix(1.0)      ! Base for floating-point (usually 2)
        
      ! Single-Precision
      case(11)
        i1mach = digits(1.0)     ! Number of base-radix digits in mantissa
      case(12)
        i1mach = minexponent(1.0) ! Minimum exponent
      case(13)
        i1mach = maxexponent(1.0) ! Maximum exponent
        
      ! Double-Precision
      case(14)
        i1mach = digits(1.0d0)   ! Number of base-radix digits in mantissa
      case(15)
        i1mach = minexponent(1.0d0) ! Minimum exponent
      case(16)
        i1mach = maxexponent(1.0d0) ! Maximum exponent
        
      case default
        ! Error: I out of bounds
        i1mach = 0
        error stop "I1MACH - I OUT OF BOUNDS"
    end select
    
  end function i1mach

end module i1mach_module
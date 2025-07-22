module r1mach_module
  use, intrinsic :: iso_fortran_env, only: real32
  implicit none
  private
  public :: r1mach
  
  ! This module provides single precision machine constants using modern Fortran intrinsics
  ! Original SLATEC R1MACH required manual configuration for each machine
  ! Our implementation automatically adapts to the current system

contains

  real function r1mach(i)
    implicit none
    integer, intent(in) :: i
    
    select case(i)
      case(1)
        ! R1MACH(1) = B**(EMIN-1), the smallest positive magnitude
        r1mach = tiny(1.0)
        
      case(2)
        ! R1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude
        r1mach = huge(1.0)
        
      case(3)
        ! R1MACH(3) = B**(-T), the smallest relative spacing
        ! For IEEE single precision, this is 2^-24 (half of machine epsilon)
        r1mach = epsilon(1.0) / 2.0
        
      case(4)
        ! R1MACH(4) = B**(1-T), the largest relative spacing
        ! For IEEE single precision, this is 2^-23 (machine epsilon)
        r1mach = epsilon(1.0)
        
      case(5)
        ! R1MACH(5) = LOG10(B), where B is the base (usually 2)
        r1mach = log10(real(radix(1.0)))
        
      case default
        ! Error: I out of bounds
        r1mach = 0.0
        error stop "R1MACH - I OUT OF BOUNDS"
    end select
    
  end function r1mach

end module r1mach_module
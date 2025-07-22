module d1mach_module
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private
  public :: d1mach
  
  ! This module provides double precision machine constants using modern Fortran intrinsics
  ! Original SLATEC D1MACH required manual configuration for each machine
  ! Our implementation automatically adapts to the current system

contains

  double precision function d1mach(i)
    implicit none
    integer, intent(in) :: i
    
    select case(i)
      case(1)
        ! D1MACH(1) = B**(EMIN-1), the smallest positive magnitude
        d1mach = tiny(1.0d0)
        
      case(2)
        ! D1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude
        d1mach = huge(1.0d0)
        
      case(3)
        ! D1MACH(3) = B**(-T), the smallest relative spacing
        ! For IEEE double precision, this is 2^-53 (half of machine epsilon)
        d1mach = epsilon(1.0d0) / 2.0d0
        
      case(4)
        ! D1MACH(4) = B**(1-T), the largest relative spacing  
        ! For IEEE double precision, this is 2^-52 (machine epsilon)
        d1mach = epsilon(1.0d0)
        
      case(5)
        ! D1MACH(5) = LOG10(B), where B is the base (usually 2)
        d1mach = log10(real(radix(1.0d0), kind=real64))
        
      case default
        ! Error: I out of bounds
        d1mach = 0.0d0
        error stop "D1MACH - I OUT OF BOUNDS"
    end select
    
  end function d1mach

end module d1mach_module
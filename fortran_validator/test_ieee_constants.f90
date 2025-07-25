program test_ieee_constants
  use iso_fortran_env, only: real32, real64
  implicit none
  
  ! Test what modern Fortran intrinsics return
  write(*,*) 'Modern IEEE constants from Fortran intrinsics:'
  write(*,*) 'R1MACH(1) = TINY(real32)  = ', tiny(1.0_real32)
  write(*,*) 'R1MACH(2) = HUGE(real32)  = ', huge(1.0_real32)
  write(*,*) 'R1MACH(3) = epsilon/2     = ', epsilon(1.0_real32)/2.0_real32
  write(*,*) 'R1MACH(4) = EPSILON       = ', epsilon(1.0_real32)
  write(*,*) 'R1MACH(5) = LOG10(2)      = ', log10(2.0_real32)
  write(*,*)
  write(*,*) 'D1MACH(1) = TINY(real64)  = ', tiny(1.0_real64)
  write(*,*) 'D1MACH(2) = HUGE(real64)  = ', huge(1.0_real64)
  write(*,*) 'D1MACH(3) = epsilon/2     = ', epsilon(1.0_real64)/2.0_real64
  write(*,*) 'D1MACH(4) = EPSILON       = ', epsilon(1.0_real64)
  write(*,*) 'D1MACH(5) = LOG10(2)      = ', log10(2.0_real64)
end program
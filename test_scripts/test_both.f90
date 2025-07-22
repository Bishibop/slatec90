program test_both
  implicit none
  integer :: n
  double precision :: x(3)
  double precision :: denorm  ! This will link to whichever version we compile with
  external denorm
  
  ! Test case
  n = 3
  x(1) = 3.0d0
  x(2) = 4.0d0
  x(3) = 0.0d0
  
  print *, 'DENORM([3,4,0]) =', denorm(n, x)
  print *, 'Expected: 5.0'
  
end program
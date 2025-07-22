program test_denorm_migration
  implicit none
  integer :: n
  double precision :: x(5), result_orig, result_modern
  double precision :: denorm
  external denorm
  
  ! Test case: 3-4-5 triangle
  n = 3
  x(1) = 3.0d0
  x(2) = 4.0d0
  x(3) = 0.0d0
  
  ! Call modern version (through F77 wrapper)
  result_modern = denorm(n, x)
  
  print *, 'Test: [3, 4, 0]'
  print *, 'Result:', result_modern
  print *, 'Expected: 5.0'
  print *, 'Pass:', abs(result_modern - 5.0d0) < 1.0d-10
  
  ! Test case 2: Large values
  n = 2
  x(1) = 1.0d19
  x(2) = 1.0d19
  
  result_modern = denorm(n, x)
  print *, ''
  print *, 'Test: [1e19, 1e19]'
  print *, 'Result:', result_modern
  print *, 'Expected:', sqrt(2.0d0) * 1.0d19
  
end program
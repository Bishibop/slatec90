program compare_versions
  implicit none
  integer :: n, i
  double precision :: x(5), result_f77, result_modern
  double precision :: denorm_f77, denorm_modern
  external denorm_f77, denorm_modern
  
  ! Multiple test cases
  print *, 'Comparing F77 vs Modern DENORM'
  print *, '================================'
  
  ! Test 1: Simple case
  n = 3
  x = [3.0d0, 4.0d0, 0.0d0, 0.0d0, 0.0d0]
  result_f77 = denorm_f77(n, x)
  result_modern = denorm_modern(n, x)
  
  print *, 'Test 1: [3, 4, 0]'
  print *, '  F77:   ', result_f77
  print *, '  Modern:', result_modern
  print *, '  Match: ', abs(result_f77 - result_modern) < 1.0d-15
  
  ! Test 2: Small values
  n = 2  
  x = [3.834d-20, 3.834d-20, 0.0d0, 0.0d0, 0.0d0]
  result_f77 = denorm_f77(n, x)
  result_modern = denorm_modern(n, x)
  
  print *, ''
  print *, 'Test 2: Small values'
  print *, '  F77:   ', result_f77
  print *, '  Modern:', result_modern
  print *, '  Match: ', abs(result_f77 - result_modern) < 1.0d-35
  
end program
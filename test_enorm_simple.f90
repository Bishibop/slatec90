program test_enorm_simple
    use enorm_module, only: enorm
    implicit none
    
    real :: result
    real :: x(3)
    integer :: n
    
    ! Test 1: Zero vector
    n = 3
    x = [0.0, 0.0, 0.0]
    result = enorm(n, x)
    print *, "Test 1 (zeros): ", result, " (expected: 0.0)"
    
    ! Test 2: 3-4-5 triangle
    n = 2
    result = enorm(n, [3.0, 4.0])
    print *, "Test 2 (3,4): ", result, " (expected: 5.0)"
    
    ! Test 3: Single element
    n = 1
    result = enorm(n, [5.0])
    print *, "Test 3 (5): ", result, " (expected: 5.0)"
    
end program test_enorm_simple
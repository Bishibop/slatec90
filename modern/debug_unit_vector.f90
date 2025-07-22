program debug_unit_vector
    use denorm_module
    implicit none
    
    double precision :: x(2), result
    
    ! Test case: Unit vector in 2D
    x(1) = 0.7071067811865475d0
    x(2) = 0.7071067811865475d0
    
    print *, "Testing unit vector case:"
    print *, "Input: x(1) =", x(1)
    print *, "       x(2) =", x(2)
    print *, "Expected:", sqrt(x(1)**2 + x(2)**2)
    
    result = denorm(2, x)
    print *, "DENORM result:", result
    
    ! Check what's in the output file
    print *, ""
    print *, "From output file, test index 1 result:", 0.99999999999999989d0
    print *, "But we're getting:", result
    
end program debug_unit_vector
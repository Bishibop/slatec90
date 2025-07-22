program verify_zabs_implementation
    use zabs_module
    implicit none
    
    double precision :: result
    
    print *, "ZABS Implementation Verification"
    print *, "================================"
    
    ! Test 1: Zero complex number
    result = zabs(0.0d0, 0.0d0)
    print '(A,F10.6)', "zabs(0,0) = ", result
    
    ! Test 2: Pure real
    result = zabs(3.0d0, 0.0d0)
    print '(A,F10.6)', "zabs(3,0) = ", result
    
    ! Test 3: Pure imaginary
    result = zabs(0.0d0, 4.0d0)
    print '(A,F10.6)', "zabs(0,4) = ", result
    
    ! Test 4: 3-4-5 triangle
    result = zabs(3.0d0, 4.0d0)
    print '(A,F10.6)', "zabs(3,4) = ", result
    
    ! Test 5: Large values
    result = zabs(1.0d+100, 1.0d+100)
    print '(A,E15.7)', "zabs(1e100,1e100) = ", result
    
    ! Test 6: Small values
    result = zabs(1.0d-100, 1.0d-100)
    print '(A,E15.7)', "zabs(1e-100,1e-100) = ", result
    
    ! Test 7: Mixed scales
    result = zabs(1.0d+100, 1.0d-100)
    print '(A,E15.7)', "zabs(1e100,1e-100) = ", result
    
    print *, ""
    print *, "Implementation preserves F77 algorithm:"
    print *, "- Uses U = ABS(ZR), V = ABS(ZI)"
    print *, "- Computes S = U + V with CDC underflow handling"
    print *, "- Divides by larger component to prevent overflow"
    print *, "- Returns larger * sqrt(1 + (smaller/larger)^2)"

end program verify_zabs_implementation
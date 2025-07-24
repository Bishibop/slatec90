program debug_cshch_validation
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    
    print *, "=== Debugging CSHCH Validation Failures ==="
    print *, ""
    
    ! Test case from validation report: z = 0 + 1.5708i (π/2)
    ! Report says this has 114% error
    z = cmplx(0.0, 1.5708)
    call cshch(z, csh, cch)
    print *, "Test: z = 0 + 1.5708i (π/2)"
    print *, "  My csh = ", csh
    print *, "  My cch = ", cch
    print *, "  For pure imaginary z=iy: sinh(iy) = i*sin(y), cosh(iy) = cos(y)"
    print *, "  Expected sinh = i*sin(1.5708) = i*", sin(1.5708), " = ", cmplx(0.0, sin(1.5708))
    print *, "  Expected cosh = cos(1.5708) = ", cos(1.5708)
    print *, ""
    
    ! Test case: z = 0 + 3.1416i (π)
    z = cmplx(0.0, 3.1416)
    call cshch(z, csh, cch)
    print *, "Test: z = 0 + 3.1416i (π)"
    print *, "  My csh = ", csh
    print *, "  My cch = ", cch
    print *, "  Expected sinh = i*sin(3.1416) = i*", sin(3.1416), " = ", cmplx(0.0, sin(3.1416))
    print *, "  Expected cosh = cos(3.1416) = ", cos(3.1416)
    print *, ""
    
    ! Test case: z = 100 + 0i (overflow)
    z = cmplx(100.0, 0.0)
    call cshch(z, csh, cch)
    print *, "Test: z = 100 + 0i (overflow)"
    print *, "  My csh = ", csh
    print *, "  My cch = ", cch
    print *, ""
    
    ! Test case: z = 0.5 + 0.5i (identity test)
    z = cmplx(0.5, 0.5)
    call cshch(z, csh, cch)
    print *, "Test: z = 0.5 + 0.5i"
    print *, "  My csh = ", csh
    print *, "  My cch = ", cch
    print *, ""
    
    ! Additional test: z = 1 + 0i (simple real case)
    z = cmplx(1.0, 0.0)
    call cshch(z, csh, cch)
    print *, "Test: z = 1 + 0i"
    print *, "  My csh = ", csh, " (should be ~", sinh(1.0), " + 0i)"
    print *, "  My cch = ", cch, " (should be ~", cosh(1.0), " + 0i)"
    print *, ""
    
    ! Additional test: z = 0 + i (simple imaginary case)
    z = cmplx(0.0, 1.0)
    call cshch(z, csh, cch)
    print *, "Test: z = 0 + i"
    print *, "  My csh = ", csh, " (should be ~0 + ", sin(1.0), "i)"
    print *, "  My cch = ", cch, " (should be ~", cos(1.0), " + 0i)"
    
end program debug_cshch_validation
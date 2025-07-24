program test_cshch_detailed
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    real :: x, y
    integer :: i
    
    ! Test cases that are failing according to the validation report
    
    ! Test 1: Special angle cases (pure imaginary)
    print *, "=== Special Angle Cases ==="
    
    ! z = 0 + π/2*i
    z = cmplx(0.0, 1.5708)
    call cshch(z, csh, cch)
    print *, "z = 0 + 1.5708i (π/2):"
    print *, "  csh = ", csh
    print *, "  cch = ", cch
    print *, "  cos(π/2) = ", cos(1.5708)
    print *, "  sin(π/2) = ", sin(1.5708)
    print *, ""
    
    ! z = 0 + π*i
    z = cmplx(0.0, 3.1416)
    call cshch(z, csh, cch)
    print *, "z = 0 + 3.1416i (π):"
    print *, "  csh = ", csh
    print *, "  cch = ", cch
    print *, "  cos(π) = ", cos(3.1416)
    print *, "  sin(π) = ", sin(3.1416)
    print *, ""
    
    ! Test 2: Overflow cases
    print *, "=== Overflow Cases ==="
    
    ! Large positive x
    z = cmplx(100.0, 0.0)
    call cshch(z, csh, cch)
    print *, "z = 100 + 0i:"
    print *, "  csh = ", csh
    print *, "  cch = ", cch
    print *, ""
    
    ! Large negative x
    z = cmplx(-100.0, 0.0)
    call cshch(z, csh, cch)
    print *, "z = -100 + 0i:"
    print *, "  csh = ", csh
    print *, "  cch = ", cch
    print *, ""
    
    ! Test 3: Identity test cases
    print *, "=== Identity Test Cases ==="
    
    ! z = 0.5 + 0.5i
    z = cmplx(0.5, 0.5)
    call cshch(z, csh, cch)
    print *, "z = 0.5 + 0.5i:"
    print *, "  csh = ", csh
    print *, "  cch = ", cch
    print *, ""
    
    ! z = 1.0 + 1.0i
    z = cmplx(1.0, 1.0)
    call cshch(z, csh, cch)
    print *, "z = 1.0 + 1.0i:"
    print *, "  csh = ", csh
    print *, "  cch = ", cch
    print *, ""
    
    ! Test 4: Check for numerical precision near special values
    print *, "=== Precision Test Near π ==="
    y = 3.141592653589793  ! More precise π
    z = cmplx(0.0, y)
    call cshch(z, csh, cch)
    print *, "z = 0 + πi (precise):"
    print *, "  csh = ", csh
    print *, "  cch = ", cch
    print *, "  sin(π) = ", sin(y)
    print *, "  cos(π) = ", cos(y)
    
end program test_cshch_detailed
# F77/F90 Integration Guide

## Overview

This document explains how F77 and F90 code can work together in the SLATEC migration project, specifically focusing on how the original F77 test suite can validate our modern F90 implementations.

## Key Concepts

### Modern Compilers Handle Both Standards

Modern Fortran compilers like `gfortran`, `ifort`, and others can compile both:
- Legacy F77 code (with appropriate flags)
- Modern F90+ code with modules, derived types, etc.

This eliminates the historical problem of mixing F77 and F90 compilers.

### No Wrapper Functions Required

A common misconception is that F77 code cannot call F90 module procedures. In reality:
- F90 modules export symbols that are callable from F77
- The linker resolves F77 calls to F90 module procedures
- No intermediate wrapper functions needed

## How It Works

### F90 Module Structure

```fortran
module pythag_module
  implicit none
  private
  public :: pythag  ! Exports 'pythag' symbol
  
contains

  pure function pythag(a, b) result(res)
    implicit none
    real, intent(in) :: a, b
    real :: res
    
    ! Modern implementation
    
  end function pythag

end module pythag_module
```

### F77 Code Calling F90

```fortran
C     Original F77 code
      PROGRAM TESTPYTH
      REAL PYTHAG, A, B, C
      EXTERNAL PYTHAG
      
      A = 3.0
      B = 4.0
      C = PYTHAG(A, B)
      PRINT *, 'Result:', C
      END
```

### Symbol Resolution

When compiled and linked:
1. The F90 module exports `pythag` as a public symbol
2. The F77 code declares `PYTHAG` as external
3. The linker matches the F77 call to the F90 export
4. Case variations (PYTHAG vs pythag) are handled by the compiler

## Compilation Process

### Step-by-Step Instructions

```bash
# 1. Compile F90 modules first
# This creates object files with exported symbols
gfortran -c modern/pythag_modern.f90
gfortran -c modern/cdiv_modern.f90
gfortran -c modern/i1mach_modern.f90

# 2. Compile F77 code with legacy support
# The -std=legacy flag allows old F77 constructs
gfortran -c src/svd.f -std=legacy
gfortran -c test04.f -std=legacy

# 3. Link everything together
# Order doesn't matter here - linker resolves all symbols
gfortran svd.o test04.o pythag_modern.o cdiv_modern.o i1mach_modern.o -o test_program

# 4. Run the test
./test_program
```

### Compilation Flags

#### For F77 Code
- `-std=legacy`: Allows old F77 constructs
- `-fno-automatic`: Makes local variables static (F77 behavior)
- `-finit-local-zero`: Initialize variables to zero (common F77 assumption)

#### For F90 Code
- Default flags are usually fine
- `-Wall -Wextra`: Enable warnings for better code quality
- `-O2`: Optimization for performance

## Examples

### Example 1: SLATEC Test Suite

Running TEST04 which tests complex arithmetic:

```bash
# Compile modern implementations
gfortran -c modern/cdiv_modern.f90
gfortran -c modern/pythag_modern.f90

# Compile test and dependencies
gfortran -c test04.f -std=legacy
gfortran -c cfnck.f -std=legacy

# Link and run
gfortran test04.o cfnck.o cdiv_modern.o pythag_modern.o -o test04
echo "0" | ./test04  # KPRINT=0
```

### Example 2: Mixed Dependencies

When F77 code uses both F77 and F90 functions:

```fortran
C     F77 code using both old and new
      SUBROUTINE MIXED
      REAL PYTHAG, OLDFU
      EXTERNAL PYTHAG, OLDFU
      
C     Calls F90 module function
      X = PYTHAG(3.0, 4.0)
      
C     Calls F77 function
      Y = OLDFU(X)
      END
```

Compilation:
```bash
gfortran -c modern/pythag_modern.f90
gfortran -c src/oldfu.f -std=legacy
gfortran -c mixed.f -std=legacy
gfortran mixed.o pythag_modern.o oldfu.o -o mixed_test
```

### Example 3: Testing Individual Functions

Create a simple F77 test for a migrated function:

```bash
# Create test
cat > test_enorm.f <<'EOF'
      PROGRAM TESTENOR
      REAL ENORM
      EXTERNAL ENORM
      REAL X(5)
      INTEGER N
      
      N = 5
      X(1) = 3.0
      X(2) = 4.0
      X(3) = 0.0
      X(4) = 0.0
      X(5) = 0.0
      
      PRINT *, 'ENORM result:', ENORM(N, X)
      PRINT *, 'Expected: 5.0'
      END
EOF

# Compile and run
gfortran -c modern/enorm_modern.f90
gfortran -c test_enorm.f -std=legacy
gfortran test_enorm.o enorm_modern.o -o test_enorm
./test_enorm
```

## Common Issues and Solutions

### Issue 1: Undefined Reference

**Error**: `undefined reference to 'pythag_'`

**Solution**: Ensure the F90 module exports the function as public:
```fortran
public :: pythag  ! This line is crucial
```

### Issue 2: Type Mismatches

**Error**: Type mismatch between F77 REAL and F90 real

**Solution**: Use consistent precision:
- F77 `REAL` = F90 `real` (single precision)
- F77 `DOUBLE PRECISION` = F90 `real(kind=8)` or `double precision`

### Issue 3: Array Passing

**F77 Style**:
```fortran
REAL X(*)  ! Assumed size
```

**F90 Style**:
```fortran
real, intent(in) :: x(:)  ! Assumed shape
```

**Solution**: F90 can accept F77-style arrays:
```fortran
real, intent(in) :: x(*)  ! Works with F77 callers
```

### Issue 4: Function vs Subroutine

Ensure the F90 implementation matches the F77 interface:
- If F77 has `REAL FUNCTION FOO`, F90 must be a function
- If F77 has `SUBROUTINE BAR`, F90 must be a subroutine

## Best Practices

1. **Match Interfaces Exactly**: Keep same function/subroutine type, argument order, and types
2. **Use Consistent Precision**: Single precision unless explicitly DOUBLE PRECISION
3. **Test Incrementally**: Validate each function individually before full test suite
4. **Document Differences**: If F90 version has improvements (e.g., better precision), document them
5. **Preserve Semantics**: Don't change algorithm behavior unless fixing bugs

## Validation Strategy

### Phase 1: Individual Function Tests
Create simple F77 programs to test each migrated function in isolation.

### Phase 2: Quick Check Routines
Run the SLATEC quick check routines (qc*.f files) that test specific functions.

### Phase 3: Full Test Programs
Run complete test programs (test01.f - test54.f) as dependencies are satisfied.

### Phase 4: Application Tests
Test with real applications that use SLATEC to ensure compatibility.

## Conclusion

The integration of F77 and F90 code is straightforward with modern compilers:
- No wrapper functions needed
- Direct calls work seamlessly
- Original test suite provides comprehensive validation
- Modern compiler features eliminate historical compatibility issues

This approach allows gradual migration while maintaining full compatibility and validation through the original SLATEC test suite.
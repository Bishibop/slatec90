# SLATEC F77 to Modern Fortran Migration

Manual migration of the SLATEC mathematical library from FORTRAN 77 to modern Fortran using comprehensive test-driven validation.

## Overview

SLATEC (Sandia, Los Alamos, Air Force Weapons Laboratory Technical Exchange Committee) is a comprehensive FORTRAN 77 library containing 736 mathematical and statistical routines (168,355 lines). This project migrates functions to modern Fortran (F90+) while preserving numerical accuracy through exhaustive testing.

## Current Status

✅ **Completed Migrations**:
- PYTHAG - Pythagorean sum (194 test cases, 100% pass)
- CDIV - Complex division (335 test cases, 100% pass)

📊 **Progress**: 2 of 169 zero-dependency functions migrated  
🎯 **Next**: 167 functions ready for migration (see `zero_dependency_functions.json`)

## Migration Approach

We use a manual, test-driven approach:
1. Generate comprehensive test cases (200+ per function)
2. Compile and run original F77 to get reference values
3. Implement modern Fortran version preserving the algorithm
4. Validate against all test cases (100% pass rate required)

See **MIGRATION_GUIDE.md** for complete instructions.

## Project Structure

```
slatec_test/
├── src/                    # Original SLATEC F77 source (736 files)
├── modern/                 # Modern Fortran implementations
│   ├── pythag_modern.f90   # Completed migration
│   └── cdiv_modern.f90     # Completed migration
├── test_data/              # Validated test cases with reference values
│   ├── pythag_tests.json   # 194 test cases
│   └── cdiv_tests.json     # 335 test cases
├── tree                    # Function dependency tree
├── zero_dependency_functions.json  # Functions ready to migrate
└── MIGRATION_GUIDE.md      # Comprehensive migration instructions
```

## Quick Example

Original F77 (PYTHAG):
```fortran
      REAL FUNCTION PYTHAG (A, B)
      REAL A,B,P,Q,R,S,T
      P = MAX(ABS(A),ABS(B))
      Q = MIN(ABS(A),ABS(B))
      IF (Q .EQ. 0.0E0) GO TO 20
   10 CONTINUE
         R = (Q/P)**2
         T = 4.0E0 + R
         IF (T .EQ. 4.0E0) GO TO 20
         S = R/T
         P = P + 2.0E0*P*S
         Q = Q*S
      GO TO 10
   20 PYTHAG = P
      RETURN
      END
```

Modern Fortran:
```fortran
module pythag_module
  implicit none
  private
  public :: pythag

contains

  pure function pythag(a, b) result(res)
    implicit none
    real, intent(in) :: a, b
    real :: res
    real :: p, q, r, s, t

    p = max(abs(a), abs(b))
    q = min(abs(a), abs(b))
    
    if (q == 0.0) then
      res = p
      return
    end if

    do
      r = (q / p)**2
      t = 4.0 + r
      if (t == 4.0) exit
      s = r / t
      p = p + 2.0 * p * s
      q = q * s
    end do

    res = p
  end function pythag

end module pythag_module
```

## Key Documentation

- **`MIGRATION_GUIDE.md`** - Complete migration instructions and strategies
- **`KNOWLEDGEBASE.md`** - General SLATEC knowledge and insights
- **`zero_dependency_functions.json`** - List of functions ready to migrate

## Technical Details

- **Compiler**: gfortran (tested with modern standards)
- **Validation**: Exact comparison with F77 results (1e-6 tolerance)
- **Test Coverage**: 100-500 test cases per function
- **Algorithm Preservation**: Keep original numerical methods

## Original SLATEC Info

- **Version**: 4.1 (July 1993)
- **Size**: 736 files, 168k lines
- **Categories**: 14 GAMS classifications
- **License**: Public domain
# SLATEC Function Migration Complete Guide

This is a comprehensive guide for migrating SLATEC functions from F77 to modern Fortran. It consolidates all migration-specific documentation into a single reference.

## Table of Contents
1. [Migration Status](#migration-status)
2. [Overview](#overview)
3. [Understanding SLATEC](#understanding-slatec)
4. [Function Dependencies](#function-dependencies)
5. [Migration Process](#migration-process)
6. [Test Generation Strategies](#test-generation-strategies)
7. [Implementation Guidelines](#implementation-guidelines)
8. [Validation and Quality](#validation-and-quality)
9. [Examples and References](#examples-and-references)

## Migration Status

### Summary
- **Total Zero-Dependency Functions**: 169
- **Completed**: 2
- **In Progress**: 0
- **Available**: 167

### Completed Migrations ✅

| Function | Test Cases | Date Completed | Notes |
|----------|------------|----------------|-------|
| PYTHAG | 194 | 2025-01-22 | Pythagorean sum with overflow protection |
| CDIV | 335 | 2025-01-22 | Complex division (a+bi)/(c+di) |

### In Progress 🚧

| Function | Developer | Started | Notes |
|----------|-----------|---------|-------|
| (none) | | | |

### Next Priority Functions 🎯

These are recommended based on simplicity and usefulness:

| Function | Description | Why Priority |
|----------|-------------|--------------|
| ENORM | Euclidean norm | Simple, widely used |
| DENORM | Double precision norm | Pair with ENORM |
| FDUMP | Error message dump | Part of error system |
| J4SAVE | Save/recall error state | Foundation function |
| LSAME | Compare characters (BLAS) | Simple utility |
| ZABS | Complex absolute value | Simple complex arithmetic |
| ISAMAX | Index of max abs value | BLAS utility |
| SASUM | Sum of absolute values | BLAS utility |

### Complete List of Available Functions (167)

All zero-dependency functions available for migration:

```
AAAAAA    BCRH      BDIFF     BNFAC     BNSLV     BSPDOC    BSPLVN    BSRH
BVDER     CDCST     CDNTP     CDPSC     CDSCL     CFOD      CHKPR4    CHKPRM
CHKSN4    CHKSNG    CMPTR3    CMPTRX    CNBDI     CPEVLR    CPROC     CPROCP
CPROD     CPRODP    CRATI     CS1S2     CSHCH     CUCHK     D1MPYQ    DBDIFF
DBNFAC    DBNSLV    DBVDER    DCFOD     DDANRM    DDATRP    DDAWTS    DDCST
DDNTP     DDPSC     DDSCL     DEFEHL    DENORM    DFEHL     DFSPVN    DHVNRM
DINTP     DINTRV    DINTYD    DJAIRY    DNBDI     DPLPFL    DPOLCF    DPOLVL
DQCHEB    DQFORM    DQMOMO    DQPSRT    DQRSLV    DQWGTC    DQWGTF    DQWGTS
DRSCO     DSOSSL    DSTOR1    DSVCO     DUSRMT    DVNRMS    DWNLT2    DWUPDT
DX        DX4       DXPSI     DXRED     DY        DY4       DYAIRY    ENORM
FDUMP     FFTDOC    FUNDOC    HVNRM     I1MACH    INDXA     INDXB     INDXC
INTRV     INTYD     INXCA     INXCB     INXCC     J4SAVE    JAIRY     LA05ED
LA05ES    LSAME     MC20AD    MC20AS    MINSO4    MINSOL    MPADD3    MPERR
MPMLP     MPSTR     ORTHO4    ORTHOG    PGSF      PIMACH    POLCOF    POLYVL
PPGSF     PPPSF     PPSGF     PPSPF     PROC      PROCP     PROD      PRODP
PSGF      QCHEB     QFORM     QMOMO     QPDOC     QPSRT     QRSOLV    QWGTC
QWGTF     QWGTS     R1MPYQ    RSCO      RWUPDT    SDANRM    SDATRP    SDAWTS
SDCST     SDNTP     SDPSC     SDSCL     SINTRP    SNBDI     SOSSOL    SPLPFL
STOR1     SVCO      TEVLC     TEVLS     TRI3      TRIDQ     TRIS4     TRISP
TRIX      USRMAT    VNWRMS    WNLT2     XERCNT    XERHLT    XPSI      XRED
YAIRY     ZABS      ZEXP      ZMLT      ZSHCH     ZUCHK
```

Key categories:
- **BLAS-like operations**: D1MPYQ, R1MPYQ, RWUPDT, DWUPDT, etc.
- **Complex arithmetic**: ZABS, ZEXP, ZMLT, ZSHCH, CSHCH, etc.
- **Norms**: ENORM, DENORM, HVNRM, DHVNRM, VNWRMS, DVNRMS, etc.
- **Machine constants**: I1MACH, PIMACH
- **Error handling**: FDUMP, J4SAVE, XERCNT, XERHLT
- **Polynomial/Spline**: POLCOF, DPOLVL, BSPLVN, DFSPVN, etc.
- **Special functions**: JAIRY, DJAIRY, YAIRY, DYAIRY (Airy functions)
- **Documentation**: AAAAAA, BSPDOC, FFTDOC, FUNDOC, QPDOC
- **Utilities**: LSAME, INTRV, DINTRV, etc.

### Notes
- ENORM has a modern/utilities/enorm_module.f90 from old approach but no test data - needs proper migration
- DENORM also exists but marked as migrated in old approach - needs verification
- Some functions like AAAAAA are just documentation and don't need migration
- Functions ending in 1/2/3/4 are often variants that might share implementation

## Overview

### Current Status
All migrations require 100% test pass rate. See the migration status table above for current progress.

### Key Files
- `tree` - Complete function dependency tree
- `src/` - Original F77 source files
- `modern/` - Modern Fortran implementations
- `test_data/` - Validated test cases with reference values

## Understanding SLATEC

### What is SLATEC?
- **736 FORTRAN 77 source files** containing highly optimized mathematical algorithms
- **~300 user-callable functions** (each with 2-4 precision variants)
- **440 subsidiary routines** (internal helpers not directly callable)
- **14 GAMS categories** covering everything from special functions to ODE solvers
- **54% documentation** - exceptionally well-documented legacy code

### SLATEC Philosophy
- **Quick check philosophy**: Tests designed to catch gross errors, not exhaustive validation
- **Portability**: Strict coding guidelines for portability across supercomputers
- **Error handling**: Sophisticated XERMSG system for all error conditions
- **Machine constants**: Uses I1MACH, R1MACH, D1MACH for platform independence

## Function Dependencies

### Dependency Hierarchy

#### Level 0: Foundation (No Dependencies)
Must be migrated first:
- **I1MACH** - Integer machine constants
- **R1MACH** - Single precision machine constants
- **D1MACH** - Double precision machine constants
- **J4SAVE** - Save/recall error handling state
- **FDUMP** - Dump error messages
- **PYTHAG** - Already migrated ✓
- **CDIV** - Already migrated ✓

#### Level 1: Error Handling System
Depends only on Level 0:
- **XGETUA** → J4SAVE
- **XERMSG** → I1MACH, XGETUA, FDUMP
- **XERPRN** → XERMSG
- **XERSVE** → J4SAVE
- **XERHLT** → (system dependent)
- **XERCNT** → J4SAVE

#### Level 2: Basic Utilities
- **ENORM** → R1MACH
- **DENORM** → D1MACH
- **Basic BLAS**: SCOPY, SSCAL, SDOT, SAXPY (no dependencies)

#### Level 3: Mathematical Building Blocks
Critical functions used by many others:
- **ALNGAM** → R1MACH, XERMSG (log gamma - VERY important)
- **GAMMA** → ALNGAM, R1MACH, XERMSG
- **ERF/ERFC** → R1MACH, XERMSG

### Finding Dependencies
To check a function's dependencies, look for:
1. CALL statements in the source
2. EXTERNAL declarations
3. The dependency tree file shows "(NONE)" for zero-dependency functions

## Migration Process

### Step 1: Select and Claim a Function

1. Check the Migration Status section above to see what's available
2. Choose a function that:
   - Is computational (not documentation like AAAAAA)
   - Has clear mathematical purpose
   - Exists in `src/` directory
   - Isn't already in progress or completed

3. Update this guide's "In Progress" table with your function and commit immediately to avoid conflicts

4. Read the function's source file to understand:
   - Its mathematical purpose (check the PURPOSE comment)
   - Input/output parameters
   - Any special algorithms or numerical considerations

### Step 2: Generate Comprehensive Test Cases

Create test cases based on the function type:

#### For Utility Functions (PYTHAG, ENORM, etc.)
```json
{
  "description": "Clear description of what this tests",
  "inputs": [input1, input2, ...],
  "expected": null  // Will be filled by F77 execution
}
```

Include:
1. **Basic functionality**: Simple cases with known results
2. **Edge cases**: 
   - Zero inputs (each parameter independently)
   - Negative values (if meaningful)
   - Very small values (near machine epsilon ~1.19e-7)
   - Very large values (near overflow)
3. **Numerical stability**:
   - Cases that might cause overflow/underflow
   - Extreme ratios between parameters
4. **Mathematical properties**:
   - Symmetry: f(a,b) = f(b,a)
   - Scaling: f(ka,kb) = k*f(a,b)
   - Known relationships (e.g., Pythagorean triples)

#### For Complex Arithmetic (CDIV, CMPLX, etc.)
- Real/imaginary special cases
- Unit complex numbers at various angles (0°, 30°, 45°, 60°, 90°, etc.)
- Division by conjugate
- Near-zero divisors

#### For Special Functions (BESI, GAMMA, ERF, etc.)
1. **Literature reference values** (Abramowitz & Stegun, DLMF)
2. **Algorithm regime transitions**:
   - Small arguments (series expansion)
   - Medium arguments (standard algorithm)  
   - Large arguments (asymptotic expansion)
3. **Special points**:
   - Zeros, poles, branch cuts
   - Integer/half-integer orders
4. **Recurrence relations** and mathematical identities

#### Number of Test Cases
- Minimum: 50-100 for simple functions
- Target: 200-500 for comprehensive coverage
- Include both systematic combinations and edge cases

### Step 3: Create F77 Test Program

Write an F77 program to get reference values:

For functions returning single value:
```fortran
      PROGRAM TEST_FUNC
      REAL FUNCNAME, ARG1, ARG2, RESULT
      EXTERNAL FUNCNAME
      
C     Test 1
      ARG1 = 1.0E0
      ARG2 = 2.0E0
      RESULT = FUNCNAME(ARG1, ARG2)
      WRITE(*,'(A,I3,A,E20.10)') 'TEST_', 1, '_RESULT: ', RESULT
      
      END
```

For subroutines with multiple outputs:
```fortran
      PROGRAM TEST_SUB
      REAL IN1, IN2, OUT1, OUT2
      EXTERNAL SUBNAME
      
C     Test 1  
      IN1 = 1.0E0
      IN2 = 2.0E0
      CALL SUBNAME(IN1, IN2, OUT1, OUT2)
      WRITE(*,'(A,I3,A,E20.10,A,E20.10)') 'TEST_', 1, 
     +    '_RESULT: ', OUT1, ', ', OUT2
      
      END
```

**Important**: If you have many test cases (>50), split into multiple programs to avoid F77 size limits.

### Step 4: Compile and Run F77 Tests

1. Save test program as `test_funcname.f`
2. Compile: `gfortran -o test_funcname test_funcname.f src/funcname.f`
3. Run: `./test_funcname > results.txt`
4. Parse results to extract reference values
5. Store complete test data in JSON format:

```json
{
  "function": "funcname",
  "signature": "FUNCTION FUNCNAME(ARG1, ARG2)",
  "description": "Brief description of what function does",
  "total_tests": 200,
  "test_cases": [
    {
      "description": "Test description",
      "inputs": [1.0, 2.0],
      "expected": [3.14159],  // From F77 execution
      "test_id": 1
    }
  ]
}
```

### Step 5: Create Modern Fortran Implementation

#### Module Structure
```fortran
module funcname_module
  implicit none
  private
  public :: funcname

contains

  pure function funcname(arg1, arg2) result(res)
    implicit none
    real, intent(in) :: arg1, arg2
    real :: res
    
    ! Implementation here
    
  end function funcname

end module funcname_module
```

#### Modern Fortran Guidelines
- Use `implicit none` always
- Add `intent(in)`, `intent(out)`, `intent(inout)` to all arguments
- Use `pure` or `elemental` for functions when possible
- Replace GOTO with structured constructs
- Keep the same algorithm - don't optimize yet
- Use same precision as original (usually single precision REAL)

#### Common F77 to Modern Conversions

**GOTO loops**:
```fortran
! F77:
   10 CONTINUE
      ... loop body ...
      IF (condition) GO TO 10

! Modern:
do while (.not. condition)
  ... loop body ...
end do
```

**Computed GOTO**:
```fortran
! F77:
      GO TO (10,20,30), INDEX

! Modern:
select case(index)
  case(1)
    ! Code for label 10
  case(2)
    ! Code for label 20
  case(3)
    ! Code for label 30
end select
```

**DATA statements**:
```fortran
! F77:
      DATA ONE,ZERO /1.0E0,0.0E0/

! Modern:
real, parameter :: one = 1.0, zero = 0.0
```

### Step 6: Test Modern Implementation

Create test program using the modern module:
```fortran
program test_modern
  use funcname_module, only: funcname
  implicit none
  
  ! Test code comparing to reference values
  
end program test_modern
```

Compile and test:
```bash
gfortran -o test_modern modern/funcname_modern.f90 test_modern.f90
./test_modern
```

Use relative tolerance of 1e-6 for single precision comparisons.

### Step 7: Validation Criteria

The migration is successful when:
- 100% of test cases pass (no exceptions)
- Results match F77 within numerical tolerance
- No compiler warnings
- Code follows modern Fortran standards

## Test Generation Strategies

### By Function Category

#### 1. Utility Functions (ENORM, PYTHAG, VNORM)
- **Overflow/underflow protection**: Test with huge/tiny values
- **Scaling properties**: Verify f(k*x) = k*f(x)
- **Mathematical identities**: Pythagorean triples, norm inequalities
- **Edge cases**: Empty, single element, all zeros

#### 2. Special Functions (BESI, GAMMA, ERF)
- **Reference values**: From Abramowitz & Stegun, DLMF
- **Regime transitions**: Small/medium/large argument algorithms
- **Special points**: Zeros, poles, integer arguments
- **Recurrence relations**: Verify mathematical identities

#### 3. Linear Algebra (SGESL, SGEFA)
- **Well-conditioned**: Identity, diagonal, tridiagonal matrices
- **Ill-conditioned**: Hilbert matrices, near-singular
- **Special structures**: Symmetric, banded, triangular
- **Pathological cases**: Wilkinson, Frank matrices

#### 4. Iterative Solvers (SNLS1, SNSQ)
- **Easy problems**: Fast convergence cases
- **Hard problems**: Slow convergence, poor conditioning
- **Failure cases**: Singular, inconsistent systems
- **Tolerance testing**: Various precision levels

#### 5. Integration/ODE (QAG, DASSL)
- **Smooth functions**: Polynomials, exponentials
- **Singularities**: Endpoints, interior poles
- **Oscillatory**: High-frequency sines/cosines
- **Discontinuous**: Step functions, absolute value

### Universal Edge Cases

Always test these for every function:
```fortran
! IEEE special values
0.0, -0.0, tiny(1.0), epsilon(1.0), huge(1.0)

! Powers of 2 (exact representation)
2**i for i = -126 to 127

! Near epsilon
1.0 ± i*epsilon(1.0) for i = 1 to 10

! Denormal range (if supported)
values < tiny(1.0)
```

## Implementation Guidelines

### File Organization
```
slatec_test/
├── src/funcname.f          # Original (unchanged)
├── modern/
│   └── funcname_modern.f90 # New implementation
├── test_data/
│   └── funcname_tests.json # Test cases with reference values
└── test_modern_funcname.f90 # Test program (can be deleted after validation)
```

### Common Pitfalls

1. **Division by zero**: Check algorithm for implicit assumptions
2. **Array bounds**: F77 often uses assumed-size arrays - be explicit
3. **Initialization**: F77 DATA statements vs modern initialization
4. **SAVE attribute**: F77 variables in DATA statements have implicit SAVE
5. **Function vs Subroutine**: Maintain the same interface type
6. **Precision**: Use same precision as original

### Troubleshooting

**Tests fail with small differences**:
- Check for accumulated rounding in iterative algorithms
- Verify order of operations matches original
- Consider if tolerance is appropriate

**Compilation errors**:
- Check all variables are declared
- Verify modern syntax is correct
- Ensure module name doesn't conflict

**Algorithm seems different**:
- Re-read original carefully - F77 can be dense
- Check for implicit behaviors (SAVE, initialization)
- Verify loop bounds and conditions

## Validation and Quality

### Migration Requirements Checklist

Before marking a function as complete:
- [ ] Selected function from zero-dependency list
- [ ] Read and understood original F77 code
- [ ] Generated comprehensive test cases (minimum 100, more for complex functions)
- [ ] Created F77 test program
- [ ] Compiled and ran F77 tests successfully
- [ ] Captured all reference values
- [ ] Test data saved in `test_data/funcname_tests.json`
- [ ] Implemented modern Fortran version in `modern/funcname_modern.f90`
- [ ] All tests pass with 100% success rate
- [ ] No compiler warnings
- [ ] Algorithm preserved from original
- [ ] Updated Migration Status table to show completion

### Commit Message Format
```
Migrate FUNCNAME to modern Fortran

- Generated N comprehensive test cases
- All tests pass with 100% accuracy
- Function computes [brief description]
```

## Examples and References

### Completed Migrations
Study these for reference:
1. **PYTHAG** (`modern/pythag_modern.f90`)
   - 194 test cases in `test_data/pythag_tests.json`
   - Computes sqrt(a²+b²) with overflow protection
   - Simple iterative algorithm

2. **CDIV** (`modern/cdiv_modern.f90`)
   - 335 test cases in `test_data/cdiv_tests.json`
   - Complex division (a+bi)/(c+di)
   - Scaling algorithm to avoid overflow

### Key Resources
- **Abramowitz & Stegun**: Mathematical reference values
- **DLMF**: Digital Library of Mathematical Functions
- **SLATEC Guide**: Original documentation (if available)
- **IEEE 754**: Floating point edge cases

Remember: Quality over quantity. One well-tested migration is better than several questionable ones.
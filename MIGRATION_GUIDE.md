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
- **Completed**: 3
- **In Progress**: 0
- **Available**: 166

### Completed Migrations ✅

| Function | Test Cases | Date Completed | Notes |
|----------|------------|----------------|-------|
| PYTHAG | 194 | 2025-01-22 | Pythagorean sum with overflow protection |
| CDIV | 335 | 2025-01-22 | Complex division (a+bi)/(c+di) |
| I1MACH | 16 | 2025-01-22 | Integer machine constants (IEEE values) |

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
FDUMP     FFTDOC    FUNDOC    HVNRM     INDXA     INDXB     INDXC
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
- **Version 4.1** (July 1993) - The version we're migrating
- **902 user-callable routines** plus subsidiary routines = 736 total files
- **290,907 lines** of highly optimized mathematical algorithms
- **14 GAMS categories** covering everything from special functions to ODE solvers
- **Public domain** - No distribution restrictions

### SLATEC Philosophy
- **Quick check philosophy**: Tests designed to catch gross errors, not exhaustive validation
- **Portability**: Primary goal was portable software for member sites' supercomputers
- **Error handling**: Sophisticated XERMSG system with 3 severity levels
- **Machine constants**: Uses I1MACH, R1MACH, D1MACH from Bell Labs' PORT Library
- **No printed output**: All information returned via arguments

### Key SLATEC Coding Standards (from official guide)
These standards explain why the F77 code looks the way it does:

1. **No COMMON blocks or SAVE variables** - Except for DATA loaded constants (obstructs multiprocessing)
2. **All UPPERCASE** - Except comments and character constants (F77 standard requirement)
3. **Strict prologue format** - Starts with `C***BEGIN PROLOGUE` for documentation extraction
4. **Error flag argument** - Required for all user-callable routines that can detect errors
5. **Machine constants only via I1MACH/R1MACH/D1MACH** - Never calculate or DATA load directly
6. **Build on existing routines** - E.g., use LINPACK/EISPACK rather than reimplementing

## Function Dependencies

### Dependency Hierarchy

#### Level 0: Foundation (No Dependencies)
Must be migrated first:
- **I1MACH** - Integer machine constants - Already migrated ✓
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

**Example: CDIV Test Pattern**
```python
# Test complex division at various angles
for angle in range(0, 360, 30):
    rad = math.radians(angle)
    ar, ai = 5 * math.cos(rad), 5 * math.sin(rad)
    br, bi = math.cos(math.radians(45)), math.sin(math.radians(45))
    test_case = {
        "description": f"Magnitude 5 at {angle}° / unit at 45°",
        "inputs": [ar, ai, br, bi],
        "expected": None  # F77 fills: [cr, ci]
    }
```

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
- **F77 Batch Limit**: F77 programs can only handle ~50 test cases per program due to size limits. The helper script automatically handles batching.

### Step 3: Generate Test Cases and Get Reference Values

Use the test helper script to generate test cases and automatically get reference values from F77:

```bash
python slatec_test_helper.py generate FUNCNAME
```

This will:
1. Generate test cases based on your implementation
2. Create F77 test programs (handling batch limits automatically)
3. Compile and run the F77 code
4. Parse the output and extract reference values
5. Save everything to `test_data/funcname_tests.json`

#### Adding Support for a New Function

To migrate a new function, you need to add it to `slatec_test_helper.py`:

1. **Add test case generation** in `_generate_FUNCNAME_tests()`:
```python
def _generate_funcname_tests(self):
    """Generate test cases for FUNCNAME"""
    tests = []
    
    # Think about what to test:
    # - Basic functionality
    # - Edge cases (0, tiny, huge values)
    # - Known mathematical properties
    # - Numerical stability cases
    
    tests.append({
        "description": "Basic test case",
        "inputs": [1.0, 2.0],
        "expected": None  # Will be filled by F77
    })
    
    return tests
```

2. **Add F77 program generation** in `_generate_FUNCNAME_f77()`:
```python
def _generate_funcname_f77(self, test_cases, start_index):
    """Generate F77 test program"""
    program = f"""      PROGRAM TEST_FUNCNAME
      REAL FUNCNAME, ARG1, ARG2, RESULT
      EXTERNAL FUNCNAME
      
"""
    for i, test in enumerate(test_cases):
        test_num = start_index + i + 1
        arg1, arg2 = test['inputs']
        program += f"""C     Test {test_num}
      ARG1 = {arg1:e}
      ARG2 = {arg2:e}
      RESULT = FUNCNAME(ARG1, ARG2)
      WRITE(*,'(A,I5,A,E20.10)') 'TEST_', {test_num}, '_RESULT: ', RESULT
      
"""
    program += "      END"
    return program
```

3. **Update the main generator** to call your functions
4. **Update output parsing** if your function returns multiple values

The helper handles all the tedious parts:
- **Compilation**: Automatic gfortran compilation with proper linking
- **Batch processing**: Splits tests into chunks of 50 (F77 program size limit)
- **Scientific notation parsing**: Handles F77 output format (e.g., `1.234567E+00`)
- **JSON formatting**: Structured test data with descriptions and expected values

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

Validate your modern implementation against the test data:

```bash
python slatec_test_helper.py validate FUNCNAME
```

This will:
1. Load the test data from `test_data/funcname_tests.json`
2. Compile your modern implementation from `modern/funcname_modern.f90`
3. Run all test cases through your implementation
4. Compare results with F77 reference values (using 1e-6 relative tolerance)
5. Report pass/fail statistics

The validation requires 100% pass rate. If any tests fail, the output will show:
- Which tests failed
- Expected vs actual values
- Relative error

You may need to add support for your function's modern test generation in `_generate_FUNCNAME_modern_test()` if it has a different signature than the examples.

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

**Example: PYTHAG Test Pattern**
```python
# Pythagorean triples at multiple scales
triples = [(3,4,5), (5,12,13), (8,15,17), (7,24,25)]
for a, b, c in triples:
    for scale in [0.01, 0.1, 1, 10, 100, 1000]:
        test_case = {
            "description": f"Pythagorean triple ({a},{b},{c}) scaled by {scale}",
            "inputs": [a*scale, b*scale],
            "expected": None  # F77 will fill this
        }
```

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

### SLATEC Error Handling Convention

When migrating functions that have error detection, be aware of SLATEC's error handling approach:

1. **Error Flag Parameter**: User-callable routines that can detect errors have an integer error flag (often named INFO, IER, or IERR)
2. **Return Values**:
   - 0 = Success
   - Positive = Warning or informational
   - Negative = Error condition
3. **XERMSG Calls**: If you see `CALL XERMSG(...)`, this is SLATEC's error reporting system
4. **Modern Approach**: In modern Fortran, you might:
   - Keep the error flag for compatibility
   - Use optional STAT parameters
   - Consider exceptions for fatal errors only

Example:
```fortran
! F77 with SLATEC error handling:
      IF (N .LT. 0) THEN
         IER = -1
         CALL XERMSG('SLATEC', 'MYFUNC', 'N must be non-negative', -1, 1)
         RETURN
      ENDIF

! Modern Fortran:
if (n < 0) then
   ier = -1
   ! Optionally: error stop "MYFUNC: N must be non-negative"
   return
end if
```

### Special Case: Machine Constants (I1MACH, R1MACH, D1MACH)

Machine constant functions are unique because they return system-dependent values:
- **F77 Challenge**: Original code has all constants commented out - users must manually configure
- **Testing Approach**: Use IEEE standard values as reference (most common on modern systems)
- **Modern Solution**: Use Fortran intrinsics that automatically detect system properties
- **Validation**: Values may differ between F77 and modern, but both are "correct" for their environment

Example:
```fortran
! F77 (hardcoded):
DATA IMACH(5) / 32 /      ! Always 32

! Modern (adaptive):
i1mach = bit_size(1)      ! 32 on 32-bit systems, 64 on 64-bit systems
```

### Handling Complex F77 Constructs

#### Functions with Many Parameters (>6)
F77 uses continuation lines for long parameter lists:
```fortran
! F77:
      SUBROUTINE FUNC(A, B, C, D, E, F,
     +                G, H, I, J, K, L)

! Modern:
subroutine func(a, b, c, d, e, f, &
                g, h, i, j, k, l)
```

#### COMMON Blocks
If you encounter COMMON blocks:
```fortran
! F77:
      COMMON /BLOCK1/ X, Y, Z

! Modern: Convert to module variables
module block1_module
  implicit none
  real :: x, y, z
end module
```

#### EXTERNAL Function Parameters
When a function takes another function as parameter:
```fortran
! F77:
      REAL FUNCTION INTEGRATE(F, A, B)
      EXTERNAL F

! Modern:
real function integrate(f, a, b)
  interface
    real function f(x)
      real, intent(in) :: x
    end function f
  end interface
```

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

3. **I1MACH** (`modern/i1mach_modern.f90`)
   - 16 test cases in `test_data/i1mach_tests.json`
   - Returns integer machine constants
   - Uses modern intrinsics instead of hardcoded values
   - Note: System-dependent, tested against IEEE standards

### Test Helper Script

A Python helper script `slatec_test_helper.py` is required to automate test generation and validation:

```bash
# Generate test cases and get reference values from F77
python slatec_test_helper.py generate PYTHAG

# Validate modern implementation against test data
python slatec_test_helper.py validate PYTHAG
```

The script handles:
- Test case generation based on function type
- Batch compilation and execution of F77 programs
- Parsing F77 output to extract reference values
- Validating modern implementations against test data

To add support for a new function, implement:
1. `_generate_FUNCNAME_tests()` - Test case generation
2. `_generate_FUNCNAME_f77()` - F77 test program generation
3. Update parsing logic if output format differs

### Key Resources
- **Abramowitz & Stegun**: Mathematical reference values
- **DLMF**: Digital Library of Mathematical Functions
- **SLATEC Guide**: Original documentation (if available)
- **IEEE 754**: Floating point edge cases

Remember: Quality over quantity. One well-tested migration is better than several questionable ones.
# SLATEC F77 to F90 Migration: Validation System Analysis and Strategy

**Date**: July 24, 2025  
**Project Leader**: Internal Document  
**Purpose**: Define validation strategy for mathematical correctness while modernizing to F90

## Executive Summary

The current Fortran validation system successfully validates simple mathematical functions but cannot handle approximately 60-70% of SLATEC's 738 functions due to various limitations. By focusing exclusively on mathematical correctness and relaxing non-mathematical constraints, we can validate ~95% of functions with an enhanced validator. This report details the challenges and solutions required.

## Current State

### Existing Infrastructure
1. **Pure Fortran Validator** (`fortran_validator/`)
   - Directly compares F77 and F90 implementations
   - Eliminates Python/JSON complexity
   - Simple text-based test format
   - Successfully validates basic functions (PYTHAG, CDIV, etc.)

2. **Test Generation**
   - Manual test case creation
   - Basic coverage of normal cases and edge conditions
   - No systematic approach for complex functions
   - Issues with mathematical constraint violations (54.5% of BSPLVN tests were invalid)

### Validation Limitations

#### 1. Stateful Functions (4 functions, 0.5%)
**Affected**: BSPLVN, BSPVN, DBSPVN, DFSPVN

These functions use INDEX parameter for stateful computation:
```fortran
! INDEX=1: Initialize
CALL BSPLVN(T, 1, 1, X, ILEFT, VNIKX)
! INDEX=2: Continue (requires previous state)
CALL BSPLVN(T, 2, 2, X, ILEFT, VNIKX)
```

**Problem**: Validator treats each test independently, cannot maintain state between calls.

#### 2. External Function Parameters (209 functions, 28%)
**Affected**: Integration routines (QAG*), ODE solvers (DDRIV*), optimization

These accept function pointers:
```fortran
CALL QAGI(F, BOUND, INF, EPSABS, EPSREL, RESULT, ABSERR, ...)
!         ^ External function parameter
```

**Problem**: No mechanism to pass test functions in validator.

#### 3. Variable Output Arrays (269 functions, 36%)
**Affected**: Linear algebra, solvers, work arrays

Functions with dynamically sized outputs:
```fortran
! Work array size depends on input parameters
! LWORK must be >= MAX(1, MN + MAX(MN, NRHS) * NB)
```

**Problem**: Test format uses fixed array sizes.

#### 4. Function-Specific Tolerances (249 functions, 34%)
**Affected**: Iterative methods, integration, special functions

Different algorithms need different comparison tolerances:
- Near-zero comparisons need absolute tolerance
- Large values need relative tolerance  
- Ill-conditioned problems need relaxed tolerance

**Problem**: Single global tolerance for all tests.

#### 5. Error Handling System (271 functions, 37%)
**Affected**: Most SLATEC functions use XERMSG, J4SAVE

Global error state affects behavior:
```fortran
CALL XERMSG('SLATEC', 'BESI', 'X IS NEGATIVE', 1, 2)
```

**Problem**: State contamination between tests, potential program termination.

#### 6. File I/O Operations (121 functions, 16%)
**Affected**: Functions with diagnostic output, FDUMP

**Problem**: Cannot capture or validate file output.

## Proposed Solution: Focus on Mathematical Correctness

### Core Philosophy
Migrate SLATEC as a **mathematical library**, not a software artifact:
- **Preserve**: Mathematical algorithms and numerical behavior
- **Modernize**: Everything else (syntax, error handling, I/O, memory management)
- **Validate**: Only mathematical correctness

### What We Can Safely Ignore

1. **Error Handling**
   - Error messages don't affect computation
   - Modern F90 can use exceptions instead of XERMSG
   - Still validate error flags (IER) as they're mathematical outputs

2. **File I/O**
   - Diagnostic output isn't mathematical
   - Can remove or modernize print statements

3. **Exact Workspace Contents**
   - Only final results matter, not intermediate arrays

4. **Iteration Counts**
   - Different convergence paths acceptable if same result

5. **Machine-Specific Behavior**
   - Use modern intrinsics instead of I1MACH/R1MACH

### Enhanced Validator Design

#### 1. Smart Array Management
```
ARRAY_SPEC: WORK
  SIZE_FORMULA: "MAX(3*N+1, 2*M)"
  PARAMS: N=10, M=5
  ! Automatically calculates and allocates WORK(31)
```

#### 2. Per-Test Tolerances
```
TEST_START
Ill-conditioned matrix
TOLERANCE:
  RELATIVE: 1e-10
  ABSOLUTE: 1e-14
  MODE: adaptive  ! Adjusts based on condition number
TEST_END
```

#### 3. Test Function Library
```fortran
MODULE slatec_test_functions
  ! Standard functions for EXTERNAL parameters
  CONTAINS
    REAL FUNCTION test_polynomial(x)
    REAL FUNCTION test_gaussian(x)
    SUBROUTINE test_ode_system(t, y, yprime)
END MODULE
```

#### 4. Sequential Test Support
```
TEST_SEQUENCE_START
Stateful B-spline evaluation

CALL_1:
  PARAMS: 1 1 2.5 3  ! INDEX=1
  EXPECTED: 0.5 0.5
  SAVE_STATE: yes
  
CALL_2:
  PARAMS: 2 2 2.5 3  ! INDEX=2  
  EXPECTED: 0.25 0.5 0.25
  USES_STATE: previous
  
TEST_SEQUENCE_END
```

#### 5. Property-Based Validation
```
PROPERTY_TEST
Function: DGESVD
Property: "U*U' = I"  ! Orthogonality
Property: "A = U*S*V'"  ! Reconstruction
TOLERANCE: 1e-14
END_PROPERTY_TEST
```

#### 6. Flexible Comparison Modes
```fortran
MODULE comparison_modes
  ! Different comparison strategies for diverse numerical scenarios
  
  LOGICAL FUNCTION compare_exact(f77_val, f90_val)
    ! Bit-for-bit equality (rarely used)
    
  LOGICAL FUNCTION compare_relative(f77_val, f90_val, tol)
    ! |f77 - f90| / |f77| < tol
    ! Good for: Normal-sized values away from zero
    
  LOGICAL FUNCTION compare_ulp(f77_val, f90_val, max_ulp)
    ! Units in Last Place comparison
    ! Good for: Tight tolerance needs, understanding floating-point
    
  LOGICAL FUNCTION compare_near_zero(f77_val, f90_val, abs_tol)
    ! |f77 - f90| < abs_tol
    ! Good for: Values near zero where relative comparison fails
    
  LOGICAL FUNCTION compare_adaptive(f77_val, f90_val, context)
    ! Chooses appropriate comparison based on:
    ! - Magnitude of values
    ! - Problem conditioning
    ! - Algorithm type (direct vs iterative)
END MODULE
```

**Usage in validator**:
- Small values near zero: Use absolute tolerance
- Large values: Use relative tolerance  
- Ill-conditioned problems: Relaxed ULP tolerance
- Special values (INF, NaN): Special handling
- Default: Adaptive comparison that switches based on magnitude

## Implementation Strategy

### Phase 1: Core Enhancements (Required)
1. **Array Size Calculator**
   - Parse function docs/signatures for size formulas
   - Generate correct allocations in test harness
   - Priority: HIGH (affects 269 functions)

2. **Tolerance System**
   - Implement per-test tolerance specifications
   - Add adaptive tolerance based on problem characteristics
   - Priority: HIGH (affects 249 functions)

3. **Test Function Library**
   - Create standard test functions for common cases
   - Polynomials, exponentials, ODEs with known solutions
   - Priority: MEDIUM (affects 209 functions)

### Phase 2: Advanced Features
1. **Sequential Test Framework**
   - Support for stateful function testing
   - State preservation between calls
   - Priority: LOW (affects 4 functions)

2. **Property Verification**
   - Mathematical invariant checking
   - Orthogonality, conservation laws, etc.
   - Priority: MEDIUM (improves confidence)

### Phase 3: Automation
1. **Test Generation System**
   - LLM-assisted test case generation
   - Constraint-aware input generation
   - Coverage analysis

2. **Migration Pipeline**
   - Automated F77 parsing
   - Modern F90 generation
   - Validation loop

## Test Case Generation Strategy

### Level 1: Immediate Implementation
- Configuration files with function signatures
- Basic parameter type generators
- Manual constraint specification

### Level 2: Intelligent Generation  
- F77 source parsing for constraints
- Domain-specific test patterns
- Relationship-aware generation

### Level 3: AI-Assisted (Future)
- LLM analysis of function behavior
- Experimental boundary discovery
- Self-improving test generation

## Practical Next Steps

### Immediate Actions (Week 1-2)
1. **Implement array size calculator**
   - Start with common patterns (LWORK, IWORK)
   - Test on linear algebra functions

2. **Add tolerance specifications to test format**
   - Extend parser to handle TOLERANCE blocks
   - Implement adaptive comparison

3. **Create initial test function library**
   - 5-10 common mathematical functions
   - Test with simple integration routines

### Short Term (Month 1)
1. **Validate enhanced system**
   - Test on 20-30 diverse functions
   - Refine based on results

2. **Build function categorization**
   - Group functions by validation needs
   - Prioritize migration order

3. **Document patterns**
   - Common size formulas
   - Tolerance requirements by function type

### Medium Term (Months 2-3)
1. **Scale validation**
   - Enhance validator for 200+ functions
   - Build comprehensive test suites

2. **Automate test generation**
   - Implement Level 2 intelligent generation
   - Reduce manual test creation

3. **Migration pipeline**
   - F77 to F90 conversion tools
   - Validation feedback loop

## Risk Assessment

### Technical Risks
1. **Hidden Dependencies**: Some mathematical behaviors might depend on non-obvious factors
   - Mitigation: Extensive testing, property verification

2. **Numerical Differences**: Modern optimizations might change floating-point behavior  
   - Mitigation: Appropriate tolerances, ULP-based comparison

3. **Complex Functions**: Some functions might have unique validation needs
   - Mitigation: Function-specific validators when needed

### Project Risks
1. **Scope Creep**: Trying to preserve too much non-mathematical behavior
   - Mitigation: Clear focus on mathematical correctness only

2. **Test Coverage**: Ensuring comprehensive test cases
   - Mitigation: Systematic generation, coverage tracking

## Success Metrics

1. **Validation Coverage**: 95%+ of functions validatable
2. **Mathematical Accuracy**: 100% pass rate on validated functions  
3. **Migration Velocity**: 10-20 functions/week once pipeline established
4. **Code Quality**: Modern F90 style, improved maintainability

## Conclusion

By focusing on mathematical correctness and relaxing non-mathematical constraints, the SLATEC migration becomes tractable. The enhanced validator will handle 95%+ of functions, with clear strategies for the remaining edge cases. The phased implementation allows controlled progress while building confidence in the approach.

The key insight: SLATEC contains timeless mathematical algorithms wrapped in dated infrastructure. We preserve the mathematics while modernizing everything else.
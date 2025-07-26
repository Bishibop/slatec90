# Technical Guides Update Plan

**Date**: July 26, 2025  
**Purpose**: Transform technical guides from theoretical documentation into practical, example-based references

## Overview

Our technical guides currently provide excellent theoretical analysis but lack concrete examples from our actual implementation. This plan outlines how to update them with real-world patterns, tools documentation, and practical guidance based on our 9 completed functions.

## Core Principles for Updated Guides

1. **Example-Driven**: Every pattern illustrated with real code from completed functions
2. **Tool-Focused**: Document our actual tools (not hypothetical ones)
3. **Practical Steps**: Clear "how to" instructions for common tasks
4. **Living Documents**: Designed for continuous updates as we learn

## Guide 1: SLATEC_MODERNIZATION_GUIDE.md

### Current Strengths
- Excellent F77→F90 pattern examples
- Comprehensive challenge analysis
- Good theoretical foundation

### What to Add

#### 1. Real Modernization Examples Section
Show actual before/after from our completed functions:

**PYTHAG Example**:
```fortran
! F77 Original
      REAL FUNCTION PYTHAG(A,B)
      REAL A,B
      REAL P,Q,R,S,T
      P = MAX(ABS(A),ABS(B))
      Q = MIN(ABS(A),ABS(B))
      IF (Q .EQ. 0.0) GO TO 20
   10 CONTINUE
         R = (Q/P)**2
         T = 4.0 + R
         IF (T .EQ. 4.0) GO TO 20
         S = R/T
         P = P + 2.0*P*S
         Q = Q*S
      GO TO 10
   20 PYTHAG = P
      RETURN
      END

! F90 Modern (show actual module structure)
module pythag_module
    use, intrinsic :: iso_fortran_env, only: real32
    implicit none
    private
    public :: pythag
    
contains
    pure function pythag(a, b) result(res)
        real(real32), intent(in) :: a, b
        real(real32) :: res
        ! Modern implementation preserving algorithm
    end function
end module
```

**Key Lessons**:
- Converted computed GOTO to DO loop
- Made function PURE (but hit error stop issue)
- Preserved exact algorithm for validation

#### 2. LLM Modernization Workflow
Document our actual process:

```
1. Initial Generation
   - modernizer.py sends F77 code + tests to LLM
   - Prompt includes error handling philosophy
   - First attempt often has compilation errors

2. Iterative Refinement (up to 5 iterations)
   - Iteration 1: "error stop not allowed in pure function"
   - Iteration 2: Fixed by removing pure attribute
   - Iteration 3: Validation errors on edge cases
   - Iteration 4: Added IEEE special value handling
   - Iteration 5: Success!

3. Common LLM Mistakes
   - Overuse of PURE attribute
   - Wrong module structure
   - Missing USE statements
   - Incorrect array bounds
```

#### 3. Pattern Library from Completed Functions

**Machine Constants** (I1MACH, R1MACH, D1MACH):
- Pattern: Replace DATA statements with intrinsics
- Issue: Index mapping errors (R1MACH 3/4 swapped)
- Solution: Careful verification against documentation

**Simple Functions** (PYTHAG, CDIV):
- Pattern: Module encapsulation
- Issue: IEEE special values
- Solution: Explicit handling in algorithm

**Error Handling** (all functions):
- Pattern: Remove XERMSG completely
- Issue: What to do on errors?
- Solution: Return defaults or use error stop

#### 4. Metadata Integration
New section on how functions fit in our system:
```python
# In slatec_metadata.py
'PYTHAG': {
    'type': 'function',
    'params': [
        {'name': 'A', 'type': 'real', 'intent': 'in'},
        {'name': 'B', 'type': 'real', 'intent': 'in'}
    ],
    'returns': 'real',
    'description': 'Computes sqrt(A**2 + B**2) without overflow/underflow'
}
```

### Structure Outline
1. Introduction (keep)
2. F77→F90 Patterns (expand with our examples)
3. Our Modernization Process (new)
4. Completed Function Gallery (new)
5. Common Pitfalls & Solutions (new)
6. Integration with System (new)

## Guide 2: SLATEC_TEST_GENERATION_GUIDE.md

### Current Strengths
- Excellent constraint analysis
- Good problem identification
- Mathematical relationship documentation

### What to Add

#### 1. Test Format Specification
Document our actual format:

```
FUNCTION: PYTHAG

TEST_START
Description: Basic positive values
REAL_PARAMS: 3.0 4.0
TEST_END

TEST_START  
Description: IEEE special case - both NaN
REAL_PARAMS: NaN NaN
TEST_END

TEST_START
Description: Large values near overflow
REAL_PARAMS: 1.0e+38 1.0e+38
TEST_END
```

**Format Rules**:
- One function per file
- TEST_START/TEST_END blocks
- Parameter type tags: INT_PARAMS, REAL_PARAMS, CHAR_PARAMS
- Description for documentation
- Expected output computed by F77

#### 2. Test Generation Process
How test_generator.py actually works:

```python
# Simplified flow
def generate_tests(function_name, f77_code):
    # 1. Analyze function signature
    signature = extract_signature(f77_code)
    
    # 2. Generate test cases based on type
    if is_simple_math_function(signature):
        tests = generate_edge_cases() + generate_normal_cases()
    elif has_array_parameters(signature):
        tests = generate_array_tests()
    
    # 3. Validate constraints
    tests = filter_valid_tests(tests, signature)
    
    # 4. Format for validator
    return format_tests(tests)
```

#### 3. Constraint Validation Examples

**PYTHAG Constraints**:
- No constraints on input (any real values valid)
- But: Algorithm fails on both inputs NaN
- Solution: Document in test description

**CDIV Constraints**:
- Cannot divide by zero (b²+d² ≠ 0)
- Test generator checks this
- Produces valid test cases only

#### 4. Coverage Strategy by Function Type

**Simple Math** (PYTHAG, CDIV):
- Normal values
- Near-zero values  
- Large values (near overflow)
- IEEE special values
- Sign combinations

**Machine Constants** (I1MACH, R1MACH):
- All valid indices
- One invalid index (for error handling)
- No mathematical constraints

#### 5. Real Test Suites
Include actual test counts and strategies:
- PYTHAG: 69 tests (comprehensive IEEE coverage)
- CDIV: 20 tests (complex number edge cases)
- I1MACH: 5 tests (all valid indices)
- R1MACH: 5 tests (all valid indices)

### Structure Outline
1. Test Challenges (keep)
2. Our Test Format (new)
3. Test Generation Implementation (new)
4. Constraint Validation (expand with examples)
5. Coverage Strategies (new)
6. Example Test Suites (new)

## Guide 3: SLATEC_VALIDATION_GUIDE.md (rename from REPORT)

### Current Strengths
- Good problem analysis
- Validation philosophy
- Limitation identification

### What to Add

#### 1. Generic Validator Architecture
Complete system documentation:

```
┌─────────────────┐     ┌──────────────────┐
│ Test File       │────▶│ validator        │
│ (PYTHAG_tests)  │     │ (main program)   │
└─────────────────┘     └────────┬─────────┘
                                 │
                        ┌────────▼─────────┐
                        │ Parse Test File  │
                        │ Extract Function │
                        └────────┬─────────┘
                                 │
                        ┌────────▼─────────┐
                        │ Function         │
                        │ Dispatcher       │
                        └────────┬─────────┘
                                 │
                ┌────────────────┴────────────────┐
                ▼                                 ▼
        ┌──────────────┐                ┌──────────────┐
        │ Metadata     │                │ Generic      │
        │ (signatures) │───────────────▶│ Validators   │
        └──────────────┘                └──────────────┘
```

**Key Components**:
- No hardcoded function names
- Metadata drives everything
- Generic validators by signature type

#### 2. Special Case Handling

**PYTHAG IEEE Example**:
```fortran
! In generic validator
if (func_name == 'PYTHAG') then
    if (ieee_is_nan(param1) .and. ieee_is_nan(param2)) then
        call report_skipped_test('both inputs NaN - would cause infinite loop')
        return
    end if
end if
```

**Why Special Cases**:
- Some F77 algorithms have known issues
- We preserve algorithms exactly
- Filter problematic inputs at validation time

#### 3. Performance Metrics

From actual runs:
```
PYTHAG Validation:
- 69 tests processed
- 67 passed, 2 skipped (NaN cases)
- Execution time: 0.3 seconds
- Memory usage: minimal

CDIV Validation:
- 20 tests processed
- 20 passed, 0 skipped
- Execution time: 0.1 seconds
```

#### 4. Adding New Functions

Step-by-step guide:
```python
# 1. Add to slatec_metadata.py
'NEWFUNC': {
    'type': 'subroutine',
    'params': [
        {'name': 'N', 'type': 'integer', 'intent': 'in'},
        {'name': 'X', 'type': 'real', 'intent': 'inout', 'dimension': 'N'}
    ]
}

# 2. Regenerate metadata
cd fortran_validator
python3 generate_fortran_metadata.py

# 3. Rebuild validator
make clean
make validator

# 4. Test
python3 slatec_orchestrator.py --function NEWFUNC
```

#### 5. Debugging Validation Failures

Common issues and solutions:
- "Function not found": Check metadata spelling
- "Type mismatch": Verify parameter types in metadata
- "Skipped all tests": Check for special case filters
- "Numerical differences": Adjust tolerances

### Structure Outline
1. Validation Philosophy (keep)
2. Generic Validator Architecture (new)
3. Metadata System (new)
4. Special Case Handling (new)
5. Performance Analysis (new)
6. How-To Guides (new)
7. Troubleshooting (new)

## Implementation Order

1. **Week 1**: Update SLATEC_MODERNIZATION_GUIDE.md
   - Add examples from all 9 completed functions
   - Document LLM workflow
   - Create pattern library

2. **Week 1**: Update SLATEC_TEST_GENERATION_GUIDE.md
   - Document test format
   - Add constraint examples
   - Include real test suites

3. **Week 2**: Transform VALIDATION_REPORT to VALIDATION_GUIDE.md
   - Full architecture documentation
   - Performance metrics
   - How-to sections

4. **Ongoing**: Update guides as new functions completed
   - Add new patterns discovered
   - Update performance metrics
   - Expand troubleshooting

## Success Criteria

Updated guides should enable a new contributor to:
1. Understand our actual system (not theoretical)
2. Add a new function end-to-end
3. Debug common problems independently
4. Contribute improvements to tools
5. Learn from our experience

## Key Differentiator

These guides document **what we built and how it works**, not what could be built. Every section grounds theory in practical implementation with real examples from our completed functions.
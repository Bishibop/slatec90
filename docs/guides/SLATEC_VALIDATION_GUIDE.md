# SLATEC Validation Guide: Generic Validator System

**Date**: July 26, 2025 (Updated January 26, 2025)  
**Purpose**: Practical guide for using and extending the generic SLATEC validator  
**Status**: Implemented and operational for 13 functions with array parameter support

## Overview

Our validation system is a metadata-driven, generic Fortran validator that compares F77 and F90 implementations for mathematical correctness. Unlike function-specific validators, it uses a central metadata registry to handle any SLATEC function.

## Generic Validator Architecture

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
- **validator.f90**: Main program that parses tests and dispatches
- **slatec_metadata.py**: Central function signature registry
- **function_dispatcher_module.f90**: Routes to appropriate validator
- **Generic validators**: Handle functions by signature type

## Metadata System

All function information lives in `fortran_validator/slatec_metadata.py`:

```python
SLATEC_FUNCTIONS = {
    'PYTHAG': {
        'type': 'function',
        'params': [
            {'name': 'A', 'type': 'real', 'intent': 'in'},
            {'name': 'B', 'type': 'real', 'intent': 'in'}
        ],
        'returns': 'real',
        'description': 'Computes sqrt(A**2 + B**2) without overflow/underflow'
    },
    
    'CDIV': {
        'type': 'subroutine',
        'params': [
            {'name': 'AR', 'type': 'real', 'intent': 'in'},
            {'name': 'AI', 'type': 'real', 'intent': 'in'},
            {'name': 'BR', 'type': 'real', 'intent': 'in'},
            {'name': 'BI', 'type': 'real', 'intent': 'in'},
            {'name': 'CR', 'type': 'real', 'intent': 'out'},
            {'name': 'CI', 'type': 'real', 'intent': 'out'}
        ],
        'returns': None,
        'description': 'Complex division (AR+i*AI)/(BR+i*BI) = CR+i*CI'
    }
}
```

This metadata drives:
- Fortran module generation
- Test dispatch
- Validation logic
- Documentation

## Adding a New Function

### Step 1: Add Metadata

Edit `fortran_validator/slatec_metadata.py`:

```python
'NEWFUNC': {
    'type': 'subroutine',  # or 'function'
    'params': [
        {'name': 'N', 'type': 'integer', 'intent': 'in'},
        {'name': 'X', 'type': 'real', 'intent': 'inout', 'dimension': 'N'}
    ],
    'returns': None,  # or return type for functions
    'description': 'What this function does'
}
```

### Step 2: Generate Fortran Metadata

```bash
cd fortran_validator
python3 generate_fortran_metadata.py
```

This creates `slatec_signatures_module.f90` with Fortran-accessible metadata.

### Step 3: Rebuild Validator

```bash
cd fortran_validator
make clean
make validator
```

### Step 4: Run Tests

```bash
cd ..
python3 slatec_orchestrator.py --function NEWFUNC
```

## Array Parameter Support

The validator now supports functions with array parameters. This enables validation of linear algebra routines, vector operations, and other array-based algorithms.

### Array Function Metadata Example

```python
'ENORM': {
    'type': 'function',
    'params': [
        {'name': 'N', 'type': 'integer', 'intent': 'in'},
        {'name': 'X', 'type': 'real', 'intent': 'in', 'dimension': 'N'}
    ],
    'returns': 'real',
    'description': 'Euclidean norm of a vector'
}
```

### Test File Format for Arrays

```
FUNCTION: ENORM
TEST_START
Description: Basic 3-element vector
INT_PARAMS: 3
ARRAY_SIZE: 3
REAL_ARRAY: 3.0 4.0 0.0
TEST_END
```

### Supported Array Signatures

- `SIG_REAL_FUNC_INT_REAL_ARRAY` (type 10): Functions like ENORM(N, X)
- Future: Integer arrays, complex arrays, multi-dimensional arrays

### Array Validation Flow

1. Test parser reads `ARRAY_SIZE` and allocates array
2. `REAL_ARRAY` values are parsed into allocated array
3. Dispatcher passes array to validation function
4. Both F77 and F90 implementations receive same array data
5. Results compared with standard tolerance

## Special Case Handling

Some functions need special handling due to algorithm limitations:

### PYTHAG IEEE Example

```fortran
! In generic_real_function_validator (validator_module.f90)
if (func_name == 'PYTHAG') then
    if (ieee_is_nan(param1) .and. ieee_is_nan(param2)) then
        call report_skipped_test('both inputs NaN - would cause infinite loop')
        return
    end if
end if
```

**Why Special Cases?**:
- Some F77 algorithms have known limitations
- We preserve algorithms exactly  
- Filter problematic inputs at validation time

### Current Special Cases

1. **PYTHAG**: Skip when both inputs are NaN (infinite loop)
2. **Machine constants**: Handle error paths for invalid indices
3. **CDIV**: Already handles division by zero correctly

## Performance Metrics

From actual validation runs:

```
PYTHAG Validation:
- Total tests: 69
- Passed: 67
- Skipped: 2 (NaN cases)
- Execution time: 0.3 seconds
- Comparison mode: Adaptive tolerance

CDIV Validation:
- Total tests: 20
- Passed: 20
- Skipped: 0
- Execution time: 0.1 seconds
- Comparison mode: Relative tolerance

I1MACH Validation:
- Total tests: 5
- Passed: 5
- Skipped: 0
- Execution time: < 0.1 seconds
- Comparison mode: Exact match
```

## Debugging Validation Failures

### Common Issues and Solutions

**1. "Function not found in metadata"**
```
Error: Function MYFUNC not found in metadata
```
- Check spelling in slatec_metadata.py
- Ensure metadata was regenerated
- Verify validator was rebuilt

**2. "Type mismatch"**
```
Error: Parameter type mismatch for X
```
- Verify parameter types in metadata match F77
- Check array dimensions
- Ensure character lengths match

**3. "Module not found"**
```
Error: Cannot find myfunc_module
```
- Module name should be `{function_name.lower()}_module`
- Check modernized F90 file exists
- Verify module was compiled

**4. "Numerical differences"**
```
Test 15 FAILED
  Expected: 1.234567890
  Got:      1.234567891
  Diff:     1.0e-9
```
- Check if difference is within acceptable tolerance
- Consider if algorithm changes evaluation order
- May need to adjust comparison tolerance

### Validation Output

The validator provides detailed output:

```
================================================================================
Validating function: PYTHAG
================================================================================

Running test 1:
  Description: Both inputs zero
  Validation: PASSED ✓

Running test 45:
  Description: IEEE special case - both NaN
  Validation: SKIPPED - both inputs NaN - would cause infinite loop

Summary for PYTHAG:
  Total tests: 69
  Passed: 67
  Failed: 0
  Skipped: 2
  Pass rate: 100.00% (excluding skipped)
```

## Comparison Modes

The validator uses adaptive comparison based on value magnitudes:

```fortran
! From numerical_utils_module.f90
function compare_values(v1, v2, func_name) result(match)
    real, intent(in) :: v1, v2
    character(len=*), intent(in) :: func_name
    logical :: match
    
    ! For values near zero, use absolute tolerance
    if (abs(v1) < 1.0e-10) then
        match = abs(v1 - v2) < 1.0e-14
    ! For normal values, use relative tolerance
    else
        match = abs(v1 - v2) / abs(v1) < 1.0e-6
    end if
end function
```

## Extending the Validator

### Adding New Validation Logic

To handle a new class of functions, add to the appropriate validator:

```fortran
! In validator_module.f90
subroutine validate_new_pattern(func_name, params, expected)
    ! Your validation logic here
    if (special_condition) then
        call report_skipped_test("Reason for skipping")
        return
    end if
    
    ! Normal validation
    call execute_and_compare(func_name, params, expected)
end subroutine
```

### Adding New Parameter Types

1. Update test parser in validator.f90:
```fortran
else if (line(1:13) == 'DOUBLE_PARAMS:') then
    call parse_double_params(line(14:))
```

2. Add storage in main program:
```fortran
double precision :: double_params(10)
integer :: num_double_params
```

3. Update dispatcher to pass new parameters

## Best Practices

1. **Keep Metadata Accurate**: It drives everything
2. **Document Special Cases**: Explain why they exist
3. **Use Descriptive Test Names**: Helps debugging
4. **Match F77 Interfaces**: Don't modernize signatures
5. **Preserve Algorithms**: Validation assumes identical math

## Test Parameter Validation

As of January 2025, test parameters are automatically validated before reaching the Fortran validator:

**What happens**:
1. Test files are generated by LLM (may contain errors)
2. Python parameter validator checks and fixes common issues
3. Clean test file is passed to Fortran validator

**Common fixes applied**:
- Malformed scientific notation (`1e19e0` → `1e19`)
- Out-of-range indices (R1MACH index 10 → 5)
- Array size mismatches (padded or truncated)
- Dangerous combinations (PYTHAG with both NaN)

This prevents false validation failures from bad test data, allowing focus on real modernization issues.

## Troubleshooting Guide

### Build Issues

**Problem**: `make validator` fails
- Check gfortran version (need 8.0+)
- Verify all .f90 files compile individually
- Look for missing module dependencies

**Problem**: Metadata generation fails
- Ensure Python 3.6+ installed
- Check slatec_metadata.py syntax
- Verify write permissions

### Runtime Issues

**Problem**: Validator hangs
- Check for infinite loops in test function
- Verify test data is valid
- Add timeout to problematic tests

**Problem**: All tests fail
- Ensure F77 functions are compiled
- Check module paths are correct
- Verify test file format

### Validation Issues

**Problem**: False failures on correct code
- Review numerical tolerances
- Check for evaluation order differences
- Consider platform-specific floating point

**Problem**: Tests pass but code is wrong
- Ensure comprehensive test coverage
- Check for missing edge cases
- Verify special value handling

## Summary

The generic validator system successfully validates SLATEC functions by:
- Using metadata-driven dispatch
- Comparing F77 and F90 mathematical results
- Handling special cases gracefully
- Providing clear diagnostic output

With 9 functions complete and 100% validation success, the system is proven and ready for the remaining 729 SLATEC functions.
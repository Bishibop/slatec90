# Fortran-Based SLATEC Validator

A pure Fortran validation system that compares F77 and F90 implementations directly.

## Overview

This validator:
- Links both F77 and F90 implementations into a single executable
- Reads test cases from a simple text format
- Runs both versions with identical inputs
- Compares outputs with appropriate floating-point tolerance
- Reports detailed failure information

## Test Format

```
FUNCTION: PYTHAG

TEST_START
Simple 3-4-5 triangle
PARAMS: 3.0 4.0
TEST_END

FUNCTION: BSPLVN

TEST_START
Linear B-spline test
T_SIZE: 4
T_VALUES: 0.0 1.0 2.0 3.0
PARAMS: 2 1 0.5 1
TEST_END
```

## Building

```bash
make           # Build the validator
make test      # Run with test_cases.txt
make clean     # Clean build artifacts
```

## Usage

```bash
./validator < test_cases.txt
./validator < bsplvn_comprehensive_tests.txt
```

## Output

```
============================================================
VALIDATING FUNCTION: PYTHAG
============================================================
PASS: Simple 3-4-5 triangle
FAIL: Large values test
  F77 result:      1.00000000E+30
  Modern result:   9.99999999E+29
  Difference:      1.00000000E+20
  Relative err:     0.00%
```

## Benefits

1. **No Python/JSON complexity** - Pure Fortran validation
2. **Direct comparison** - Both implementations in same process
3. **Accurate floating-point** - No serialization errors
4. **Fast execution** - Compiled code throughout
5. **Simple test format** - Easy for LLMs to generate

## Next Steps

1. Extend to all 700 SLATEC functions
2. Generate comprehensive test suites via LLM
3. Integrate with iterative refinement loop
4. Build automated CI/CD pipeline
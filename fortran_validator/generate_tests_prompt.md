# Generate Test Cases for SLATEC Function Validation

You are a Fortran numerical computing expert. Generate comprehensive test cases for validating Fortran 77 to Fortran 90 modernization.

## Function to Test: [FUNCTION_NAME]

### Function Signature
```fortran
[SIGNATURE]
```

### Function Description
[DESCRIPTION]

## Test Format

Generate test cases in this exact format:
```
FUNCTION: [FUNCTION_NAME]

TEST_START
[Brief description of what this test checks]
[PARAMETERS based on function type - see examples below]
TEST_END
```

### Format Examples

For scalar functions like PYTHAG(A, B):
```
TEST_START
Simple 3-4-5 triangle
PARAMS: 3.0 4.0
TEST_END
```

For array functions like BSPLVN(T, JHIGH, INDEX, X, ILEFT, VNIKX):
```
TEST_START
Linear B-spline, uniform knots
T_SIZE: 4
T_VALUES: 0.0 1.0 2.0 3.0
PARAMS: 2 1 0.5 1
TEST_END
```

## Test Coverage Requirements

Generate test cases that cover:

1. **Basic functionality** - Normal use cases
2. **Edge cases**:
   - Zero values
   - Very small values (near underflow)
   - Very large values (near overflow)
   - Negative values (if applicable)
3. **Special values**:
   - IEEE special values (Inf, NaN) if applicable
   - Boundary conditions
4. **Algorithm-specific cases**:
   - For iterative algorithms: convergence tests
   - For array functions: different array sizes
   - For stateful functions: test INDEX=1 and INDEX=2 calls

## Additional Requirements

- Generate at least 20 test cases
- Include descriptive names that explain what each test validates
- For array inputs, vary the array sizes
- Test both common and uncommon input combinations
- For functions with multiple parameters, test interactions between parameters

Generate the test cases now:
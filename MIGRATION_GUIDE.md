# SLATEC Function Migration Guide

This guide provides step-by-step instructions for migrating SLATEC functions from F77 to modern Fortran. Follow these steps carefully to ensure consistent, high-quality migrations.

## Prerequisites

Before starting any migration:
1. Verify you have the dependency tree file (`tree`) 
2. Confirm the function exists in `src/` directory
3. Check that the function has no dependencies (marked with "(NONE)" in tree file)
4. Ensure gfortran is available for compilation

## Step 1: Select a Function

1. Open `zero_dependency_functions.json` to see remaining functions
2. Choose a function that:
   - Is computational (not documentation like AAAAAA)
   - Has clear mathematical purpose
   - Exists in `src/` directory

3. Read the function's source file to understand:
   - Its mathematical purpose (check the PURPOSE comment)
   - Input/output parameters
   - Any special algorithms or numerical considerations

## Step 2: Generate Comprehensive Test Cases

Create test cases that cover:

### 2.1 Basic Functionality
- Simple cases with known results
- Identity operations (if applicable)
- Inverse operations (if applicable)

### 2.2 Edge Cases
- Zero inputs (each parameter independently and all together)
- Negative values (if meaningful)
- Very small values (near machine epsilon: ~1.19e-7 for single precision)
- Very large values (near overflow)
- Special values (1, -1, 0.5, 2, 10, etc.)

### 2.3 Numerical Stability
- Cases that might cause overflow/underflow in naive implementations
- Extreme ratios between parameters
- Values that test scaling/normalization in the algorithm

### 2.4 Mathematical Properties
For each function type:

**For functions like PYTHAG (sqrt(a²+b²))**:
- Known mathematical relationships (Pythagorean triples)
- Symmetry tests: f(a,b) = f(b,a)
- Scaling: f(ka, kb) = k*f(a,b)

**For complex arithmetic (like CDIV)**:
- Real/imaginary special cases
- Unit complex numbers at various angles
- Division by conjugate
- Near-zero divisors

**For polynomial/special functions**:
- Known zeros and poles
- Asymptotic behavior
- Recurrence relations

### 2.5 Test Case Format
Create a JSON structure like:
```json
{
  "description": "Clear description of what this tests",
  "inputs": [input1, input2, ...],
  "expected": null  // Will be filled by F77 execution
}
```

### 2.6 Number of Test Cases
- Minimum: 50-100 for simple functions
- Target: 200-500 for comprehensive coverage
- Include both systematic combinations and random samples

## Step 3: Create F77 Test Program

Write an F77 program that:

1. Includes the necessary EXTERNAL declaration
2. For each test case:
   - Sets up input variables
   - Calls the function
   - Prints results in parseable format

Example for a function returning single value:
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

Example for subroutine with multiple outputs:
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

## Step 4: Compile and Run F77 Tests

1. Save test program as `test_funcname.f`
2. Compile: `gfortran -o test_funcname test_funcname.f src/funcname.f`
3. Run: `./test_funcname > results.txt`
4. Parse results to extract reference values
5. Store complete test data with inputs and expected outputs

**Important**: If you have many test cases (>50), split into multiple programs to avoid F77 size limits.

## Step 5: Create Modern Fortran Implementation

### 5.1 Module Structure
```fortran
module funcname_module
  implicit none
  private
  public :: funcname

contains

  ! Your function/subroutine here

end module funcname_module
```

### 5.2 Modern Fortran Guidelines
- Use `implicit none` always
- Add `intent(in)`, `intent(out)`, `intent(inout)` to all arguments
- Use `pure` or `elemental` for functions when possible
- Replace GOTO with structured constructs (do loops, if-then-else)
- Use modern intrinsics where appropriate
- Keep the same algorithm - don't optimize yet

### 5.3 Common Conversions

**F77 GOTO loops**:
```fortran
C     F77 style
   10 CONTINUE
      ... loop body ...
      IF (condition) GO TO 10
```

**Modern Fortran**:
```fortran
! Modern style
do while (.not. condition)
  ... loop body ...
end do
```

**F77 computed GOTO**:
```fortran
C     F77 style
      GO TO (10,20,30), INDEX
```

**Modern Fortran**:
```fortran
! Modern style
select case(index)
  case(1)
    ! Code for label 10
  case(2) 
    ! Code for label 20
  case(3)
    ! Code for label 30
end select
```

### 5.4 File Location
Save as `modern/funcname_modern.f90`

## Step 6: Test Modern Implementation

1. Create test program using the modern module:
```fortran
program test_modern
  use funcname_module, only: funcname
  implicit none
  
  ! Test code here
  
end program test_modern
```

2. Compile: `gfortran -o test_modern modern/funcname_modern.f90 test_modern.f90`
3. Run and compare outputs to F77 reference values
4. Use relative tolerance of 1e-6 for single precision comparisons

## Step 7: Validation Criteria

The migration is successful when:
- 100% of test cases pass (no exceptions)
- Results match F77 within numerical tolerance
- No compiler warnings
- Code follows modern Fortran standards

## Step 8: File Organization

After successful migration:
```
slatec_test/
├── src/funcname.f          # Original (unchanged)
├── modern/
│   └── funcname_modern.f90 # New implementation
├── test_data/
│   └── funcname_tests.json # Test cases with reference values
└── test_modern_funcname.f90 # Test program (can be deleted after validation)
```

## Step 9: Commit

When committing:
1. Include the modern implementation
2. Include the test data
3. Update any tracking files
4. Use clear commit message:
```
Migrate FUNCNAME to modern Fortran

- Generated N comprehensive test cases
- All tests pass with 100% accuracy
- Function computes [brief description]
```

## Common Pitfalls

1. **Division by zero**: Check algorithm for implicit assumptions
2. **Array bounds**: F77 often uses assumed-size arrays - be explicit in modern version
3. **Initialization**: F77 DATA statements vs modern initialization
4. **SAVE attribute**: F77 variables in DATA statements have implicit SAVE
5. **Function vs Subroutine**: Maintain the same interface type
6. **Precision**: Use same precision as original (usually single precision REAL)

## Troubleshooting

**Tests fail with small differences**:
- Check for accumulated rounding differences in iterative algorithms
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

## Example Migration Checklist

- [ ] Selected function from zero-dependency list
- [ ] Read and understood original F77 code
- [ ] Generated comprehensive test cases (>100)
- [ ] Created F77 test program
- [ ] Compiled and ran F77 tests successfully
- [ ] Captured all reference values
- [ ] Implemented modern Fortran version
- [ ] All tests pass with 100% success rate
- [ ] No compiler warnings
- [ ] Files organized correctly
- [ ] Ready to commit

Remember: Quality over quantity. One well-tested migration is better than several questionable ones.
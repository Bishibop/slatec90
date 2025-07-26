# SLATEC Test Generation: Comprehensive Guide and Strategy

**Date**: July 24, 2025  
**Purpose**: Document test generation challenges, solutions, and implementation strategy  
**Scope**: Generating mathematically valid test cases for 738 SLATEC functions

## Executive Summary

Test generation for SLATEC functions presents unique challenges due to complex parameter relationships, mathematical constraints, and stateful behaviors. The "Test Data Debacle" revealed that 54.5% of generated BSPLVN test cases violated fundamental mathematical constraints. This guide provides a comprehensive framework for generating valid, comprehensive test cases using F77 as the oracle.

## Test Generation Challenges

### 1. Mathematical Constraint Violations (Critical)

**The Problem**: Generated test data often violates mathematical requirements.

**Example - BSPLVN**:
```
Constraint: T[ILEFT] ≤ X < T[ILEFT+1]
Reality: 109 of 200 test cases had invalid ILEFT values
Result: Negative B-spline values (mathematically impossible)
```

**Example - CSHCH**:
```
Expected: 3.139164733e-07
F77 Actual: -4.37113883e-08
Issue: Wrong value AND wrong sign
```

**Impact**: Invalid test data leads to false validation failures and wasted debugging effort.

### 2. Complex Parameter Relationships

SLATEC functions have intricate parameter dependencies:

**Array Sizing**:
```fortran
! DBOLSM requires:
! RW(5*NCOLS), WW(NCOLS), SCL(NCOLS), IND(NCOLS)
! But NCOLS depends on other parameters and problem structure
```

**Matrix Dimensions**:
```fortran
! DGELS (least squares):
! A(LDA,N) where LDA ≥ max(1,M)
! B(LDB,NRHS) where LDB ≥ max(1,M,N)
! WORK(LWORK) where LWORK ≥ min(M,N) + max(1,M,N,NRHS)*NB
```

**Constraint Chains**:
```fortran
! Integration routines:
! LIMIT determines LENW (LENW ≥ LIMIT*4)
! LENW determines work array partitioning
! Work array sections must not overlap
```

### 3. Stateful Functions (INDEX Pattern)

Four functions maintain state between calls:
- BSPLVN, BSPVN, DBSPVN, DFSPVN

**Test Requirements**:
```
Call 1: BSPLVN(T, 1, 1, X, ILEFT, VNIKX)  ! INDEX=1, initialize
Call 2: BSPLVN(T, 2, 2, X, ILEFT, VNIKX)  ! INDEX=2, continue
```

**Challenge**: Cannot test calls independently; must maintain state.

### 4. External Function Parameters

209 functions accept function pointers:

**Integration**:
```fortran
CALL QAGI(F, BOUND, INF, EPSABS, EPSREL, RESULT, ...)
!         ^ User-defined integrand
```

**ODE Solvers**:
```fortran
CALL DDASSL(RES, NEQ, T, Y, YPRIME, ...)
!           ^ Residual function G(t,y,y')=0
```

**Challenge**: Need mathematically meaningful test functions with known properties.

### 5. Option Arrays and Control Parameters

Many functions use complex option arrays:

**DDASSL INFO Array**:
```
INFO(1) = 0/1    ! First call or continuation
INFO(2) = 0/1    ! Tolerances scalar or array
INFO(3) = 0/1    ! Stop at TOUT or integrate past
INFO(4) = 0/1    ! Can go past TOUT
... (15 total options)
```

**Challenge**: Options interact in complex ways; some combinations invalid.

### 6. Workspace Requirements

Functions have complex workspace formulas:

**Examples**:
```
DGELS:   LWORK ≥ max(1, MN + max(MN,NRHS)*NB)
DGEEV:   LWORK ≥ max(1, 3*N) for left eigenvectors
DGELSS:  LWORK ≥ 3*min(M,N) + max(2*min(M,N), max(M,N), NRHS)
```

**Challenge**: Must parse documentation/code to extract formulas.

### 7. Edge Cases and Boundary Conditions

**Numerical Boundaries**:
- Near underflow (1e-38)
- Near overflow (1e38)
- Subnormal numbers
- Zero crossings

**Algorithm Boundaries**:
- Matrix singularity
- Convergence failure points
- Branch points in algorithms

**Mathematical Boundaries**:
- Function discontinuities
- Domain boundaries (e.g., SQRT negative input)
- Special angles (0, π/2, π)

### 8. Precision and Scale Issues

**Cross-Scale Interactions**:
```
PYTHAG(1e30, 1e-30)  ! Extreme scale differences
DAXPY with α=1e-40   ! Near-zero scaling
```

**Challenge**: Test must cover full floating-point range without losing significance.

## Test Generation Strategy

### Level 1: Configuration-Based Generation

**Approach**: Manual configuration files for each function.

**Configuration Format**:
```json
{
  "DGESV": {
    "description": "Solve AX=B for general matrix",
    "parameters": {
      "N": {"type": "integer", "min": 1, "typical": [3, 10, 100]},
      "NRHS": {"type": "integer", "min": 1, "typical": [1, 5]},
      "A": {
        "type": "real_matrix",
        "dimensions": ["LDA", "N"],
        "patterns": ["identity", "random", "ill_conditioned"]
      },
      "LDA": {"type": "integer", "formula": "max(1, N)"},
      "B": {"type": "real_matrix", "dimensions": ["LDB", "NRHS"]},
      "LDB": {"type": "integer", "formula": "max(1, N)"}
    },
    "constraints": [
      "LDA >= N",
      "LDB >= N"
    ],
    "test_categories": [
      "basic_functionality",
      "singular_matrices",
      "ill_conditioned",
      "large_scale"
    ]
  }
}
```

**Test Generator Logic**:
```python
def generate_tests_level1(config):
    tests = []
    
    # Basic functionality tests
    for n in config["parameters"]["N"]["typical"]:
        for pattern in config["parameters"]["A"]["patterns"]:
            test = create_test_case(n, pattern)
            tests.append(test)
    
    # Edge cases
    tests.extend(generate_edge_cases(config))
    
    # Validate all constraints
    tests = [t for t in tests if validate_constraints(t, config)]
    
    return tests
```

### Level 2: Intelligent Pattern-Based Generation

**Approach**: Parse F77 source to extract constraints and patterns.

**Source Analysis**:
```python
class F77Analyzer:
    def extract_constraints(self, source_file):
        constraints = []
        
        # Parse parameter declarations
        params = self.parse_parameters(source_file)
        
        # Find size relationships
        for line in source_file:
            # "LWORK must be at least..."
            if match := re.search(r'(\w+).*at least\s*(.+)', line):
                constraints.append({
                    'param': match.group(1),
                    'formula': match.group(2)
                })
        
        # Find mathematical constraints
        if "ILEFT" in params and "T" in params:
            # B-spline constraint pattern
            constraints.append({
                'type': 'bspline',
                'constraint': 'T[ILEFT] <= X < T[ILEFT+1]'
            })
        
        return constraints
```

**Pattern Library**:
```python
DOMAIN_PATTERNS = {
    'linear_algebra': {
        'matrix_patterns': ['identity', 'diagonal', 'tridiagonal', 
                           'random', 'ill_conditioned', 'singular'],
        'test_properties': ['determinant', 'condition_number', 'rank']
    },
    'integration': {
        'test_functions': ['polynomial', 'gaussian', 'oscillatory',
                          'singular_endpoint', 'infinite_oscillation'],
        'test_properties': ['known_integral', 'convergence_rate']
    },
    'special_functions': {
        'test_points': ['zeros', 'poles', 'branch_cuts', 
                       'asymptotic_regions'],
        'test_properties': ['symmetries', 'identities', 'limits']
    }
}
```

### Level 3: AI-Assisted Adaptive Generation

**Approach**: LLM analyzes function behavior and generates comprehensive tests.

**Architecture**:
```python
class AdaptiveTestGenerator:
    def __init__(self, llm_client, f77_executor):
        self.llm = llm_client
        self.executor = f77_executor
        self.constraint_learner = ConstraintLearner()
    
    def generate_comprehensive_tests(self, function_name):
        # Phase 1: Initial analysis
        analysis = self.llm.analyze_function(function_name)
        
        # Phase 2: Exploratory generation
        candidates = self.generate_candidates(analysis)
        
        # Phase 3: Constraint discovery
        for test in candidates:
            result = self.executor.run(function_name, test)
            if result.failed:
                learned = self.constraint_learner.learn_from_failure(
                    test, result.error
                )
                analysis.constraints.update(learned)
        
        # Phase 4: Refined generation
        valid_tests = self.generate_with_constraints(analysis)
        
        # Phase 5: Coverage analysis
        coverage = self.analyze_coverage(valid_tests)
        
        # Phase 6: Gap filling
        while coverage.percentage < 95:
            gap_tests = self.generate_gap_tests(coverage.gaps)
            valid_tests.extend(gap_tests)
            coverage = self.analyze_coverage(valid_tests)
        
        return valid_tests
```

**Constraint Learning**:
```python
class ConstraintLearner:
    def learn_from_failure(self, test_case, error_message):
        if "ILEFT" in error_message and "out of range" in error_message:
            # Learn knot interval constraint
            return {
                'type': 'array_index',
                'constraint': f'1 <= ILEFT <= len(T) - JHIGH'
            }
        
        elif "singular matrix" in error_message:
            # Learn matrix conditioning requirement
            return {
                'type': 'matrix_property',
                'constraint': 'condition_number(A) < 1e15'
            }
```

## Our Actual Test Format (From Real Implementation)

### Test File Structure

Here's the actual format we use in our system:

**Example from `test_cases/pythag_tests.txt`**:
```
FUNCTION: PYTHAG

TEST_START
Description: Both inputs zero
REAL_PARAMS: 0.0 0.0
TEST_END

TEST_START
Description: A=0.0, B=1.0e-8 (small positive)
REAL_PARAMS: 0.0 1.0e-8
TEST_END

TEST_START
Description: Positive pair (3,4) should yield 5
REAL_PARAMS: 3.0 4.0
TEST_END

TEST_START
Description: Large values near overflow
REAL_PARAMS: 1.0e+38 1.0e+38
TEST_END

TEST_START
Description: IEEE special case - both NaN
REAL_PARAMS: NaN NaN
TEST_END
```

**Key Format Rules**:
1. `FUNCTION:` line declares which function to test
2. `TEST_START`/`TEST_END` blocks delimit individual tests
3. `Description:` provides human-readable test purpose
4. Parameter types:
   - `INT_PARAMS:` for integer parameters
   - `REAL_PARAMS:` for real/float parameters
   - `CHAR_PARAMS:` for character parameters
   - `ARRAY_SIZE:` followed by `REAL_ARRAY:` for arrays
5. No expected values - F77 provides the oracle

### Actual Test Generation Process

Our `test_generator.py` uses LLM to create comprehensive tests:

```python
prompt = f"""You are a Fortran testing expert. Generate COMPREHENSIVE test cases for a SLATEC function.

Function to test: {func_name}

Source code:
```fortran
{source_code}
```

REQUIREMENTS:
1. Generate AT LEAST 50-100 test cases for thorough coverage
2. Include EXTENSIVE boundary testing:
   - Values near machine epsilon
   - Values near overflow/underflow limits
   - Powers of 2 and 10
   - Values that differ by small amounts (1e-6, 1e-7, etc.)
3. Test all edge cases:
   - All combinations of zeros
   - All combinations of signs (+/+, +/-, -/+, -/-)
   - Special values if applicable (Inf, -Inf, NaN)
"""
```

### Real Test Suite Examples

**PYTHAG (69 tests generated)**:
- Zero handling: 5 tests
- Small values: 8 tests  
- Normal values: 10 tests
- Large values: 8 tests
- Sign combinations: 12 tests
- IEEE special values: 15 tests
- Near-overflow cases: 11 tests

**CDIV (20 tests generated)**:
- Basic division: 5 tests
- Division by small numbers: 4 tests
- Division by large numbers: 4 tests
- Complex edge cases: 7 tests

**I1MACH (5 tests)**:
- All valid indices (1-16): 4 tests
- Invalid index for error: 1 test

## Implementation Guide

### Our Actual Workflow

1. **Generate Tests with LLM**:
```bash
# Inside slatec_orchestrator.py
test_generator = TestGenerator(config)
test_cases = test_generator.generate(func_name, f77_source)
```

2. **Write Test File**:
```python
with open(f'test_cases/{func_name.lower()}_tests.txt', 'w') as f:
    f.write(test_content)
```

3. **Validate Format**:
The validator parses tests line by line:
```fortran
! In validator.f90
if (line(1:10) == 'FUNCTION: ') then
    function_name = trim(line(11:))
else if (line == 'TEST_START') then
    in_test = .true.
else if (line == 'TEST_END') then
    call run_generic_validation()
```

2. **Create Basic Generators**:
- Scalar generator: Multiple scales, special values
- Array generator: Various sizes and patterns
- Matrix generator: Well-conditioned, ill-conditioned, singular

3. **Implement Constraint Validator**:
- Parse constraint specifications
- Validate test cases before execution
- Report constraint violations clearly

### Constraint Validation in Practice

**PYTHAG Constraints**:
- No mathematical constraints on inputs
- But algorithm fails on both NaN (infinite loop)
- Solution: Special case handling in validator

**Real validation code from our system**:
```fortran
! In validator_module.f90
if (func_name == 'PYTHAG') then
    if (ieee_is_nan(real_params(1)) .and. ieee_is_nan(real_params(2))) then
        call report_skipped("Both inputs NaN - would cause infinite loop")
        return
    end if
end if
```

**CDIV Constraints**:
- Cannot divide by zero: BR²+BI² ≠ 0
- Test generator ensures this:

```python
# In test generation
if func_name == 'CDIV':
    # Ensure denominator is not zero
    if br == 0.0 and bi == 0.0:
        bi = 1e-10  # Make non-zero
```

### Coverage Strategy by Function Type

Based on our experience with 9 functions:

**1. Simple Math Functions (PYTHAG, PIMACH)**:
```
Coverage targets:
- Zero inputs: Both, one, neither
- Sign combinations: ++, +-, -+, --
- Scale variations: 1e-38 to 1e+38
- Special values: Inf, -Inf, NaN
- Edge cases: Subnormal numbers
```

**2. Machine Constants (I1MACH, R1MACH, D1MACH)**:
```
Coverage targets:
- All valid indices
- One invalid index (error path)
- No mathematical properties to test
- Focus on correct value mapping
```

**3. Complex Arithmetic (CDIV)**:
```
Coverage targets:
- Normal division cases
- Near-zero denominators
- Large numerator/small denominator
- Small numerator/large denominator
- Complex conjugate patterns
```

**4. Character Functions (LSAME)**:
```
Coverage targets:
- Same characters (various cases)
- Different characters
- Case mixing: aA, Aa, AA, aa
- Special characters if applicable
```

### LLM Test Generation Patterns

**Successful prompts we use**:

```python
# For mathematical functions
"Generate test cases covering:
1. All combinations of zero/non-zero inputs
2. Values at different scales: 1e-30, 1e-20, 1e-10, 1, 1e10, 1e20, 1e30
3. Values that differ by small amounts to test precision
4. Known mathematical relationships (e.g., pythag(3,4)=5)
5. IEEE special values where applicable"

# For machine constant functions  
"Generate test cases for all valid index values plus one invalid index.
For each index, verify the description matches the expected constant."

# For utility functions
"Focus on interface testing - various input combinations that exercise
all code paths without requiring deep mathematical properties."
```

### Phase 3: Adaptation (Weeks 5-6)

1. **Constraint Learning System**:
- Track test failures
- Extract constraint patterns
- Build constraint database

2. **Coverage Analysis**:
- Parameter range coverage
- Edge case coverage
- Mathematical property coverage

3. **Iterative Refinement**:
- Generate → Test → Learn → Refine
- Build comprehensive test suites
- Achieve 95%+ coverage

## Special Considerations

### For Stateful Functions

**Test Sequence Generation**:
```python
def generate_stateful_tests(function_name):
    sequences = []
    
    # Generate initialization parameters
    init_params = generate_init_call(function_name)
    
    # Generate continuation sequences
    for init in init_params:
        sequence = TestSequence()
        sequence.add_call(init, index=1)
        
        # Generate 2-5 continuation calls
        for i in range(2, random.randint(3, 6)):
            cont_params = generate_continuation(init, i)
            sequence.add_call(cont_params, index=2)
        
        sequences.append(sequence)
    
    return sequences
```

### For External Parameters

**Test Function Selection**:
```python
def select_test_functions(function_type):
    if function_type == "integration":
        return [
            ("polynomial", known_integral_polynomial),
            ("gaussian", known_integral_gaussian),
            ("oscillatory", known_integral_bessel),
            ("singular", known_integral_singular)
        ]
    elif function_type == "ode_solver":
        return [
            ("linear_system", exact_solution_linear),
            ("vanderpol", reference_solution_vanderpol),
            ("stiff_system", reference_solution_stiff)
        ]
```

### For Complex Constraints

**Multi-Parameter Validation**:
```python
def validate_complex_constraints(test_case):
    # BSPLVN constraints
    if test_case.function == "BSPLVN":
        t = test_case.params["T"]
        ileft = test_case.params["ILEFT"]
        x = test_case.params["X"]
        jhigh = test_case.params["JHIGH"]
        
        # Knot vector must be non-decreasing
        if not all(t[i] <= t[i+1] for i in range(len(t)-1)):
            return False, "Knot vector not non-decreasing"
        
        # ILEFT constraint
        if not (1 <= ileft <= len(t) - jhigh):
            return False, f"ILEFT {ileft} out of range"
        
        # X in correct interval
        if not (t[ileft-1] <= x < t[ileft]):
            return False, f"X {x} not in interval [{t[ileft-1]}, {t[ileft]})"
    
    return True, "Valid"
```

## Quality Assurance

### Validation Process

1. **Mathematical Correctness**:
- Run all tests through F77 implementation
- Verify no errors or warnings
- Check output validity (no NaN, no overflow)

2. **Coverage Verification**:
- Parameter range coverage
- Edge case inclusion
- Mathematical property coverage

3. **Constraint Compliance**:
- All generated tests satisfy constraints
- No invalid parameter combinations
- Proper workspace sizing

### Success Metrics

1. **Validity Rate**: 100% of generated tests must be mathematically valid
2. **Coverage**: 95%+ of parameter space covered
3. **Edge Cases**: All identified edge cases included
4. **Properties**: Key mathematical properties verified

## Lessons Learned from Our Implementation

### What Works Well

1. **LLM-Generated Comprehensive Coverage**:
   - GPT-4/o3-mini generates 50-100+ tests reliably
   - Excellent at identifying edge cases humans might miss
   - Good at systematic coverage (powers of 2, 10, etc.)

2. **Simple Text Format**:
   - Easy to parse in Fortran
   - Human-readable for debugging
   - No JSON complexity

3. **F77 as Oracle**:
   - No need to specify expected values
   - Eliminates human error in expectations
   - Handles complex computations automatically

### Real Challenges We Faced

**1. The BSPLVN Constraint Debacle**:
- 109 of 200 tests had invalid ILEFT values
- Caused negative B-spline values (impossible!)
- Solution: Better constraint documentation in prompts

**2. IEEE Special Values**:
- PYTHAG fails on both inputs NaN (infinite loop)
- Not a bug - F77 algorithm limitation
- Solution: Skip these tests with explanation

**3. Machine Constant Validation**:
- R1MACH indices 3 and 4 were swapped in docs
- Only discovered through careful testing
- Solution: Verify against multiple sources

**4. Test Volume Management**:
- 69 tests for PYTHAG seems excessive but found issues
- Balance thoroughness with execution time
- Solution: Categorize tests by priority

### Our Test Generation Evolution

**Version 1** (Manual):
```
# Hand-written basic tests
TEST_START
REAL_PARAMS: 3.0 4.0
TEST_END
```

**Version 2** (LLM-Assisted):
```
# Generated with categories
TEST_START  
Description: Basic positive values
REAL_PARAMS: 3.0 4.0
TEST_END
```

**Version 3** (Current):
```
# Comprehensive with IEEE handling
TEST_START
Description: IEEE special case - both NaN
REAL_PARAMS: NaN NaN
TEST_END
```

## Common Pitfalls and Solutions

### Pitfall 1: Trusting Documentation
**Problem**: SLATEC docs sometimes wrong (R1MACH 3/4 swap)
**Solution**: Verify with implementation and multiple sources

### Pitfall 2: Over-Testing Simple Functions
**Problem**: 100+ tests for PIMACH that just returns π
**Solution**: Match test complexity to function complexity

### Pitfall 3: Missing Algorithm Limitations
**Problem**: Not knowing PYTHAG loops on NaN inputs
**Solution**: Document known limitations in test descriptions

### Pitfall 4: Constraint Specification
**Problem**: LLM generates invalid test parameters
**Solution**: Explicit constraint documentation in prompts

## Tools and Infrastructure

### Required Components

1. **F77 Executor**: Run F77 functions with test inputs
2. **Constraint Validator**: Check mathematical constraints
3. **Coverage Analyzer**: Track parameter space coverage
4. **Test Formatter**: Output in validator-compatible format

### Recommended Libraries

- **NumPy**: For numerical test data generation
- **mpmath**: For high-precision arithmetic when needed
- **scipy**: For special function values and matrix generation

## Conclusion

Successful SLATEC test generation requires understanding both mathematical constraints and implementation details. The three-level approach provides a path from simple manual configuration to sophisticated AI-assisted generation. By using F77 as the oracle and validating all constraints, we can generate comprehensive test suites that ensure mathematical correctness during modernization.

The key insight: Test generation is not just about code coverage but about mathematical coverage - ensuring all numerically interesting cases are tested while respecting the complex constraints that make each SLATEC function work correctly.
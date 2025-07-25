# Phase 0 System Design: Automated Modernization Pipeline

**Date**: July 24, 2025
**Scope**: 7 ultra-trivial SLATEC functions with zero dependencies
**Goal**: Build automated F77→F90 modernization with LLM-driven iterative refinement

## Executive Summary

Phase 0 focuses on 7 ultra-trivial SLATEC functions to establish a robust, automated modernization pipeline. These functions have zero dependencies and represent the simplest possible test cases. The system will use direct LLM modernization, Fortran-based validation, and iterative refinement to achieve 100% correctness.

**Key Decision**: We are completely eliminating the XERMSG error handling system. All error handling functions will be removed from the modernized library, and mathematical functions will use modern Fortran patterns (optional status parameters, sensible defaults).

## Revised Phase Structure

### Phase 0: Ultra-Minimal (7 functions)
1. **PIMACH** - Returns π constant
2. **AAAAAA** - Returns version string
3. **LSAME** - Case-insensitive character comparison
4. **FDUMP** - Dummy subroutine (does nothing)
5. **I1MACH** - Integer machine constants (no XERMSG in original)
6. **R1MACH** - Real machine constants (XERMSG calls removed)
7. **D1MACH** - Double precision constants (XERMSG calls removed)

**Note**: J4SAVE, NUMXER, XGETUA, and other error handling functions are completely eliminated from the modern library.

### Phase 0.5: Simple Functions (15-20 functions)
- **Simple math**: PYTHAG, CSHCH (complex hyperbolic)
- **Array utilities**: Basic operations without BLAS dependencies
- **Character utilities**: Simple string operations
- **Others**: Functions with 1-2 XERMSG calls that are easily removed

### Phase 1: Functions with Dependencies (25-30 functions)
- **CSROOT** (depends on PYTHAG)
- **Array utilities**: S1MERG, D1MERG, I1MERG (depend on BLAS COPY)
- **BLAS Level 1**: SCOPY, DCOPY, SAXPY, etc.
- **Simple mathematical functions** with 1-2 dependencies

## Key Design Changes

### 1. Error Handling Modernization
**Critical Decision**: We are abandoning the XERMSG error handling system completely.

- Functions will NOT call XERMSG or use global error state
- Mathematical functions use optional status parameters and return sensible defaults
- Machine constant functions return 0 for invalid input (no error stops)
- Error handling functions (J4SAVE, NUMXER, XGETUA, etc.) are **completely eliminated**
- The LLM modernizer will be explicitly instructed to:
  - Remove all XERMSG calls
  - Add optional stat/errmsg parameters where appropriate
  - Return mathematical defaults for invalid input
  - Never use error stop for recoverable conditions

### 2. Phase 0 Characteristics
- **Zero dependencies** - No function calls to other SLATEC routines
- **No state** - Pure functions only (except I1MACH which may check machine at runtime)
- **Simple types** - Basic real, integer, double precision, character
- **Trivial algorithms** - Direct computation, constant returns, or simple lookups
- **Minimal arrays** - Only for machine constant tables
- **No XERMSG** - All error handling removed or replaced with defaults

## System Architecture

### Overview
```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   Function      │────▶│  Test Generator  │────▶│  Test Cases     │
│   Selector      │     │  (LLM-powered)   │     │  (.txt format)  │
└─────────────────┘     └──────────────────┘     └─────────────────┘
                                                           │
                                                           ▼
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│  Modernized     │◀────│  LLM Modernizer  │◀────│  F77 Source +   │
│  F90 Code       │     │  (Iterative)     │     │  Test Cases     │
└─────────────────┘     └──────────────────┘     └─────────────────┘
                                │                          │
                                ▼                          ▼
                        ┌──────────────────┐     ┌─────────────────┐
                        │ Fortran Validator │────▶│ Validation      │
                        │ (F77 vs F90)     │     │ Results         │
                        └──────────────────┘     └─────────────────┘
                                │                          │
                                └──────────────────────────┘
                                        Feedback Loop
```

### Component Details

#### 1. Function Selector & Orchestrator
```python
class Phase0Orchestrator:
    def __init__(self):
        self.functions = load_phase0_functions()
        self.completed = load_progress()
        self.test_gen = TestGenerator()
        self.modernizer = LLMModernizer()
        self.max_iterations = 5

    def process_function(self, func_name):
        print(f"Processing {func_name}...")

        # 1. Read F77 source
        f77_code = self.read_source(func_name)

        # 2. Generate test cases via LLM
        test_result = self.test_gen.generate_via_llm(func_name, f77_code)
        test_file = f"test_cases/{func_name.lower()}_tests.txt"
        with open(test_file, 'w') as f:
            f.write(test_result)

        # 3. Modernize with iteration
        modern_result = self.modernizer.modernize(func_name, f77_code, test_result)

        # Save initial modernization
        with open(f"modern/{func_name.lower()}_modern.f90", 'w') as f:
            f.write(modern_result["f90_code"])

        # Log the analysis
        with open(f"logs/{func_name.lower()}_analysis.json", 'w') as f:
            json.dump({
                "name": modern_result["name"],
                "description": modern_result["description"],
                "algorithm_analysis": modern_result["algorithm_analysis"],
                "modernization_notes": modern_result["modernization_notes"]
            }, f, indent=2)

        # 4. Iterative validation and refinement
        for iteration in range(self.max_iterations):
            validation_result = self.validate(func_name, test_file)

            if validation_result["pass_rate"] == 1.0:
                print(f"✓ {func_name} validated successfully!")
                self.save_completed(func_name, modern_result["f90_code"])
                return True

            # Refine based on errors
            if iteration < self.max_iterations - 1:
                print(f"  Iteration {iteration+1}: {validation_result['pass_rate']*100:.1f}% pass rate")
                refine_result = self.modernizer.refine(
                    func_name,
                    modern_result["f90_code"],
                    validation_result["errors"]
                )
                modern_result["f90_code"] = refine_result["f90_code"]

                # Save refined version
                with open(f"modern/{func_name.lower()}_modern.f90", 'w') as f:
                    f.write(refine_result["f90_code"])

        print(f"✗ {func_name} failed validation after {self.max_iterations} iterations")
        return False

    def process_all_parallel(self, max_workers=4):
        # Process functions in parallel
        with ThreadPoolExecutor(max_workers) as executor:
            futures = {
                executor.submit(self.process_function, func): func
                for func in self.remaining_functions
            }

            for future in as_completed(futures):
                func = futures[future]
                try:
                    success = future.result()
                    print(f"Completed {func}: {'Success' if success else 'Failed'}")
                except Exception as e:
                    print(f"Error processing {func}: {e}")
```

#### 2. Test Generator (o3-mini with JSON)
```python
class TestGenerator:
    def __init__(self, model="o3-mini"):
        self.client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        self.model = model

    def generate_for_function(self, func_name, source_code):
        # Always use LLM with structured output for consistency
        return self.generate_via_llm(func_name, source_code)

    def generate_via_llm(self, func_name, source_code):
        prompt = f"""You are a Fortran testing expert. Generate comprehensive test cases for a SLATEC function.

Function to test: {func_name}

Source code:
```fortran
{source_code}
```

Analyze this function and generate test cases that cover:
1. Basic functionality with typical inputs
2. Edge cases (zeros, empty arrays, boundary values)
3. Special values (if applicable)
4. Error conditions (invalid inputs)

Please respond with a JSON object containing:
{{
    "function_name": "{func_name}",
    "test_description": "Overview of test coverage strategy",
    "test_categories": ["list of test categories covered"],
    "test_cases": "Complete test cases in Fortran validator format",
    "special_considerations": "Any special testing requirements"
}}

The test_cases field should contain the complete test file content in this format:
FUNCTION: {func_name}

TEST_START
Description of test
PARAMS: value1 value2 ...
TEST_END

For array inputs use:
ARRAY_NAME_SIZE: n
ARRAY_NAME: val1 val2 ... valn

For Phase 0 functions, keep tests simple but thorough.
"""

        response = self.client.chat.completions.create(
            model=self.model,
            messages=[{"role": "user", "content": prompt}],
            response_format={"type": "json_object"},
            temperature=0.1
        )

        result = json.loads(response.choices[0].message.content)
        return result["test_cases"]  # Return just the test cases for validator
```

#### 3. LLM Modernizer (o3-mini with JSON)
```python
class LLMModernizer:
    def __init__(self, model="o3-mini"):
        self.client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        self.model = model

    def modernize(self, func_name, f77_code, test_cases):
        prompt = f"""You are a Fortran expert tasked with modernizing a SLATEC mathematical library function from Fortran 77 to modern Fortran 90/95.

Function to modernize: {func_name}

Original Fortran 77 source code:
```fortran
{f77_code}
```

Test inputs (blind testing - no expected outputs provided):
{test_cases}

Your task is to:
1. Analyze the F77 algorithm and understand its mathematical purpose
2. Convert it to modern Fortran 90/95 following these guidelines:
   - Use modules with 'implicit none'
   - Add intent specifications for all arguments
   - Replace GOTO with structured constructs (do while, select case, etc.)
   - Replace computed GOTO with select case
   - IMPORTANT: Remove ALL calls to XERMSG - we are abandoning this error system
   - For machine constant functions, return 0 or appropriate default for invalid input
   - Use optional status parameters only when mathematically meaningful
   - Prefer returning defaults over error stops
   - Replace DATA statements with initialization in declarations
   - Use 'pure' or 'elemental' when appropriate
   - Preserve the exact mathematical behavior and precision
   - Keep the same interface (function vs subroutine)

Please respond with a JSON object containing these fields:
{{
    "name": "{func_name}",
    "description": "Brief description of what this function does",
    "algorithm_analysis": "Your understanding of the algorithm and any special considerations",
    "modernization_notes": "Key changes made during modernization",
    "f90_code": "The complete modernized Fortran 90/95 code"
}}

Important: The f90_code should be a complete, compilable module that can replace the original function.
"""

        response = self.client.chat.completions.create(
            model=self.model,
            messages=[{"role": "user", "content": prompt}],
            response_format={"type": "json_object"},
            temperature=0.1
        )

        result = json.loads(response.choices[0].message.content)
        return result  # Return full result for logging

    def refine(self, func_name, current_code, validation_errors):
        prompt = f"""You are fixing validation errors in a modernized Fortran function.

Function: {func_name}

Current modernized code that has errors:
```fortran
{current_code}
```

Validation errors to fix:
{validation_errors}

Please analyze the errors and provide a corrected version.

Respond with a JSON object containing:
{{
    "name": "{func_name}",
    "error_analysis": "Your understanding of what went wrong",
    "fixes_applied": "Specific changes made to fix the errors",
    "f90_code": "The complete corrected Fortran 90/95 code"
}}

Keep all working parts unchanged. Only fix the specific issues identified.
"""

        response = self.client.chat.completions.create(
            model=self.model,
            messages=[{"role": "user", "content": prompt}],
            response_format={"type": "json_object"},
            temperature=0.1
        )

        result = json.loads(response.choices[0].message.content)
        return result
```

#### 4. Fortran Validator Integration
```fortran
! Phase 0 validator - currently supports PYTHAG, BSPLVN, and LSAME
! Needs enhancement for remaining Phase 0 functions: PIMACH, AAAAAA, FDUMP, I1MACH, R1MACH, D1MACH
program phase0_validator
  implicit none

  character(len=100) :: func_name, test_file
  logical :: status

  ! Get function name from command line
  call get_command_argument(1, func_name)
  call get_command_argument(2, test_file)

  select case(trim(func_name))
    case('PYTHAG')
      call validate_pythag(test_file, status)
    case('PIMACH')
      ! TODO: Implement validator for PIMACH
      print *, "PIMACH validator not yet implemented"
      status = .false.
    case('AAAAAA')
      ! TODO: Implement validator for AAAAAA
      print *, "AAAAAA validator not yet implemented"
      status = .false.
    case('LSAME')
      ! TODO: Implement validator for LSAME
      print *, "LSAME validator not yet implemented"
      status = .false.
    case('FDUMP')
      ! TODO: Implement validator for FDUMP
      print *, "FDUMP validator not yet implemented"
      status = .false.
    case('NUMXER')
      ! TODO: Implement validator for NUMXER
      print *, "NUMXER validator not yet implemented"
      status = .false.
  end select

end program
```

#### 5. Parallelization Strategy
```python
class ParallelProcessor:
    def __init__(self, max_workers=4):
        self.max_workers = max_workers
        self.queue = Queue()
        self.results = {}

    def process_batch(self, functions):
        # Group by estimated complexity
        groups = self.group_by_complexity(functions)

        # Process simplest first to debug pipeline
        for complexity, group in sorted(groups.items()):
            with ProcessPoolExecutor(max_workers=self.max_workers) as executor:
                futures = {
                    executor.submit(self.process_one, func): func
                    for func in group
                }

                for future in as_completed(futures):
                    func = futures[future]
                    try:
                        result = future.result()
                        self.results[func] = result
                    except Exception as e:
                        self.handle_error(func, e)
```

## Test Generation Strategy for Phase 0

### Function-Specific Patterns

#### Constants (PIMACH, I1MACH, R1MACH)
```
FUNCTION: PIMACH
TEST_START
Basic PI retrieval
PARAMS: 1.0
TEST_END
```

#### Array Merge Functions
```
FUNCTION: S1MERG
TEST_START
Merge two sorted arrays
AR1_SIZE: 3
AR1: 1.0 3.0 5.0
AR2_SIZE: 2
AR2: 2.0 4.0
PARAMS: 3 1.0 3.0 5.0 2 2.0 4.0
TEST_END
```

#### Error Handling Functions
```
FUNCTION: J4SAVE
TEST_START
Save and retrieve error parameter
PARAMS: 1 100 .TRUE.
TEST_END

TEST_START
Retrieve without setting
PARAMS: 1 0 .FALSE.
TEST_END
```

### Coverage Requirements
1. **Basic functionality** - Normal use cases
2. **Edge cases** - Empty inputs, zeros
3. **Boundary conditions** - Array limits
4. **Error conditions** - Invalid inputs
5. **State verification** - For J4SAVE and similar

## Modernization Patterns for Phase 0

### Pattern 1: Simple Constants
```fortran
! F77
      DOUBLE PRECISION FUNCTION PIMACH (DUM)
      DOUBLE PRECISION DUM
      PIMACH = 3.14159265358979324D0
      RETURN
      END

! F90
MODULE constants_module
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: pimach

  ! Use Fortran 2008 intrinsic
  REAL(REAL64), PARAMETER :: PI = ACOS(-1.0_REAL64)

CONTAINS

  PURE FUNCTION pimach(dum) RESULT(pi_value)
    REAL(REAL64), INTENT(IN) :: dum  ! Unused but kept for compatibility
    REAL(REAL64) :: pi_value

    pi_value = PI
  END FUNCTION pimach

END MODULE constants_module
```

### Pattern 2: Array Operations
```fortran
! F90 for merge functions
MODULE merge_utilities
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: s1merg

CONTAINS

  PURE SUBROUTINE s1merg(ar1, n1, ar2, n2, ar3)
    INTEGER, INTENT(IN) :: n1, n2
    REAL(REAL32), INTENT(IN) :: ar1(n1), ar2(n2)
    REAL(REAL32), INTENT(OUT) :: ar3(n1+n2)

    INTEGER :: i1, i2, i3

    ! Modern implementation with explicit loops
    i1 = 1; i2 = 1; i3 = 1

    DO WHILE (i1 <= n1 .AND. i2 <= n2)
      IF (ar1(i1) <= ar2(i2)) THEN
        ar3(i3) = ar1(i1)
        i1 = i1 + 1
      ELSE
        ar3(i3) = ar2(i2)
        i2 = i2 + 1
      END IF
      i3 = i3 + 1
    END DO

    ! Copy remaining elements
    ar3(i3:i3+n1-i1) = ar1(i1:n1)
    ar3(i3+n1-i1+1:) = ar2(i2:n2)

  END SUBROUTINE s1merg

END MODULE merge_utilities
```

### Pattern 3: Machine Constants Migration

```fortran
! F90 - Machine constants using modern intrinsics
MODULE machine_constants
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE, INTRINSIC :: IEEE_ARITHMETIC
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: i1mach, r1mach, d1mach

CONTAINS

  PURE FUNCTION i1mach(i) RESULT(val)
    INTEGER, INTENT(IN) :: i
    INTEGER :: val

    SELECT CASE(i)
      CASE(1); val = 5          ! Standard input unit
      CASE(2); val = OUTPUT_UNIT ! Standard output unit
      CASE(3); val = ERROR_UNIT  ! Standard error unit
      CASE(4); val = OUTPUT_UNIT ! Error message unit
      CASE(5); val = BIT_SIZE(0) ! Number of bits per integer
      CASE(6); val = 1          ! Number of characters per integer
      CASE(7); val = RADIX(0)   ! Base for integers
      CASE(8); val = DIGITS(0)  ! Number of base-radix digits
      CASE(9); val = HUGE(0)    ! Largest integer
      CASE(10); val = RADIX(1.0)     ! Base for reals
      CASE(11); val = DIGITS(1.0)    ! Digits for single precision
      CASE(12); val = MINEXPONENT(1.0) ! Min exponent single
      CASE(13); val = MAXEXPONENT(1.0) ! Max exponent single
      CASE(14); val = DIGITS(1.0D0)    ! Digits for double precision
      CASE(15); val = MINEXPONENT(1.0D0) ! Min exponent double
      CASE(16); val = MAXEXPONENT(1.0D0) ! Max exponent double
      CASE DEFAULT; val = 0  ! Invalid input - return 0
    END SELECT
  END FUNCTION i1mach

  PURE FUNCTION r1mach(i) RESULT(val)
    INTEGER, INTENT(IN) :: i
    REAL(REAL32) :: val

    SELECT CASE(i)
      CASE(1); val = TINY(1.0_REAL32)    ! Smallest positive number
      CASE(2); val = HUGE(1.0_REAL32)    ! Largest number
      CASE(3); val = EPSILON(1.0_REAL32) ! Smallest relative spacing
      CASE(4); val = EPSILON(1.0_REAL32) ! Largest relative spacing
      CASE(5); val = LOG10(REAL(RADIX(1.0_REAL32))) ! Log10 of base
      CASE DEFAULT; val = 0.0_REAL32      ! Invalid input - return 0
    END SELECT
  END FUNCTION r1mach

  PURE FUNCTION d1mach(i) RESULT(val)
    INTEGER, INTENT(IN) :: i
    REAL(REAL64) :: val

    SELECT CASE(i)
      CASE(1); val = TINY(1.0_REAL64)    ! Smallest positive number
      CASE(2); val = HUGE(1.0_REAL64)    ! Largest number
      CASE(3); val = EPSILON(1.0_REAL64) ! Smallest relative spacing
      CASE(4); val = EPSILON(1.0_REAL64) ! Largest relative spacing
      CASE(5); val = LOG10(REAL(RADIX(1.0_REAL64), REAL64)) ! Log10 of base
      CASE DEFAULT; val = 0.0_REAL64      ! Invalid input - return 0
    END SELECT
  END FUNCTION d1mach

END MODULE machine_constants
```

## Validation Enhancements for Phase 0

### Required Validator Updates

1. **Module Support**
   - Handle F90 module interfaces
   - Link module files correctly
   - Support USE statements

2. **Pure Function Testing**
   - Verify PURE/ELEMENTAL attributes
   - Check for side effects

3. **Error Handling Tests**
   - For functions being modernized away from XERMSG
   - Verify equivalent behavior with new approach

4. **State Management**
   - Special handling for J4SAVE and similar
   - Reset state between test groups

### Validation Success Criteria
- **100% test pass rate** - No exceptions
- **Numerical accuracy** - Appropriate tolerances
- **No performance regression** - Track timing
- **Clean compilation** - No warnings with strict flags

## Implementation Plan

### Week 1: Infrastructure Setup

**Day 1-2: Test Generation**
- [ ] Implement TestGenerator class
- [ ] Create function-specific patterns
- [ ] Test with 3 simple functions (PIMACH, CSROOT, LSAME)

**Day 3-4: LLM Integration**
- [ ] Set up LLM modernizer with proper prompts
- [ ] Implement iterative refinement loop
- [ ] Test with same 3 functions

**Day 5: Validator Enhancement**
- [ ] Update mega_validator for Phase 0 functions
- [ ] Add module compilation support
- [ ] Test complete pipeline

### Week 2: Expand to Phase 0.5

**Day 1-2: Enhance Validator**
- [ ] Add support for remaining Phase 0 functions
- [ ] Test with complex number types (for future phases)
- [ ] Add module compilation support

**Day 3-4: Process Phase 0.5 Functions**
- [ ] Run 10 Phase 0.5 functions through pipeline
- [ ] Document any new patterns discovered
- [ ] Refine prompts based on learnings

**Day 5: Prepare for Phase 1**
- [ ] Create dependency resolution system
- [ ] Plan for BLAS integration
- [ ] Document scaling strategy

### Success Metrics
1. **All 7 Phase 0 functions modernized** with 100% validation
2. **Fully automated pipeline** - single command execution
3. **Iterative refinement working** - LLM improves based on feedback
4. **Error handling eliminated** - No XERMSG calls remain
5. **Reusable patterns** - templates for Phase 0.5 and beyond
6. **Complete documentation** - ready for scaling

## Risk Mitigation

### Technical Risks
1. **LLM generates non-compiling code**
   - Mitigation: Compilation check before validation
   - Fallback: Syntax fix prompts

2. **Test generation misses edge cases**
   - Mitigation: Manual review of test suites
   - Fallback: Augment with hand-written tests

3. **Validator can't handle new patterns**
   - Mitigation: Iterative validator updates
   - Fallback: Manual validation for edge cases

### Process Risks
1. **LLM API rate limits**
   - Mitigation: Batch requests, add delays
   - Fallback: Queue system with retries

2. **Parallel processing conflicts**
   - Mitigation: Isolated working directories
   - Fallback: Sequential processing option

## Lessons Learned and Critical Insights

### 1. Error System Philosophy Change
- **Old SLATEC**: Sophisticated error handling with global state, multiple output units, error history
- **Modern Fortran**: Simple patterns - optional status, error stop for fatal, sensible defaults
- **Key insight**: Most "errors" in mathematical functions are just invalid input that should return defaults

### 2. Validator Reality Check
- Current validator only supports 3 functions (PYTHAG, BSPLVN, LSAME)
- Need to implement validators for: PIMACH, AAAAAA, FDUMP, I1MACH, R1MACH, D1MACH
- Validator architecture is solid but needs function-specific implementations

### 3. Dependency Analysis Critical
- Initial Phase 0 had hidden dependencies (NUMXER→J4SAVE)
- Machine constants (R1MACH/D1MACH) had XERMSG dependencies
- Must verify zero dependencies through actual code inspection

### 4. Test Data Quality
- Previous test data had mathematical violations (54.5% of BSPLVN tests)
- Need rigorous mathematical constraint validation
- Manual test creation for Phase 0 ensures quality

## Conclusion

Phase 0 has been carefully refined to 7 functions with truly zero dependencies. This ultra-minimal approach allows us to:
- Prove the LLM-driven approach with zero complexity
- Build robust infrastructure without dependency challenges
- Establish error handling migration patterns
- Achieve 100% automation for truly trivial functions
- Create a solid foundation for the remaining 731 functions

The complete elimination of the XERMSG error system simplifies modernization significantly and aligns with modern Fortran best practices.

## Summary of Key Decisions

1. **Ultra-minimal Phase 0**: 7 functions with zero dependencies
2. **Abandon XERMSG completely**: No global error state in modernized code
3. **Eliminate error functions**: J4SAVE, NUMXER, XGETUA removed entirely
4. **Machine constants included**: I1MACH, R1MACH, D1MACH in Phase 0
5. **Pure Fortran validation**: Build on existing validator infrastructure
6. **LLM with structured JSON**: Use o3-mini with JSON response format
7. **Iterative refinement**: Up to 5 iterations to achieve 100% validation
8. **Modern error patterns**: Optional status parameters, sensible defaults
9. **Phase structure**: 0 → 0.5 → 1 allows gradual complexity increase

## Next Steps

1. Enhance the Fortran validator to support all 7 Phase 0 functions
2. Create manual test cases for Phase 0 functions
3. Implement the test generator with o3-mini JSON output
4. Build the iterative modernization loop with clear error handling guidance
5. Process all 7 functions to prove the system
6. Document patterns for error handling migration
7. Move to Phase 0.5 with 15-20 slightly more complex functions

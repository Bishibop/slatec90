# SLATEC Comprehensive Testing Strategy

## Executive Summary

This document defines the testing strategy for migrating SLATEC's 736 FORTRAN 77 files (~300 user-callable functions with precision variants) to modern Fortran. It addresses the unique challenges of testing a large, interconnected mathematical library with complex dependency chains, multiple precision variants, and 440 subsidiary routines.

## Project Scale & Complexity

- **Total Files**: 736 FORTRAN 77 source files
- **User-Callable Functions**: ~300 (each with 2-4 precision variants)
- **Subsidiary Routines**: 440 internal helper functions
- **Precision Variants**: 
  - No prefix: Single precision real (e.g., `besi.f`)
  - `d` prefix: Double precision real (262 files, e.g., `dbesi.f`)
  - `c` prefix: Single precision complex (83 files, e.g., `cbesi.f`)
  - `z` prefix: Double precision complex (35 files, e.g., `zbesi.f`)
  - `s` prefix: Single precision solvers (79 files)
- **GAMS Categories**: 14 mathematical domains (A through Z)
- **Dependencies**: Complex hierarchical structure (e.g., BESI → ALNGAM, ASYIK, I1MACH, R1MACH, XERMSG)

## Test Directory Structure

```
slatec_test/
├── tests/                          # Main test directory
│   ├── framework/                  # Core testing infrastructure
│   │   ├── slatec_test_base.f90  # Base module for all tests
│   │   ├── dual_validator.f90     # F77 vs Modern comparison engine
│   │   ├── triple_validator.f90   # F77 vs Modern vs Reference (SciPy/mpmath)
│   │   ├── test_generator.f90     # Automated test case generation
│   │   ├── error_analyzer.f90     # ULP, relative, absolute error analysis
│   │   ├── dependency_tracker.f90 # Track and validate function dependencies
│   │   ├── precision_handler.f90  # Handle s/d/c/z precision variants
│   │   ├── report_generator.f90   # HTML/Markdown test reports
│   │   └── build_helper.f90       # Compilation and linking utilities
│   │
│   ├── reference/                  # External reference data
│   │   ├── generators/            # Reference value generation scripts
│   │   │   ├── generate_scipy_refs.py
│   │   │   ├── generate_mpmath_refs.py
│   │   │   ├── generate_mathematica_refs.py
│   │   │   └── generate_dlmf_refs.py
│   │   ├── literature/            # Published reference values
│   │   │   ├── abramowitz_stegun.dat
│   │   │   ├── dlmf_tables.dat
│   │   │   └── nist_handbook.dat
│   │   └── generated/             # Auto-generated reference data
│   │       ├── bessel/
│   │       ├── gamma/
│   │       ├── quadrature/
│   │       └── ...
│   │
│   ├── unit/                      # Function-level tests (by GAMS category)
│   │   ├── A_arithmetic/          # Arithmetic and error analysis
│   │   ├── C_special_functions/   # Special functions (highest priority)
│   │   │   ├── bessel/
│   │   │   │   ├── test_besi.f90    # Modified Bessel I
│   │   │   │   ├── test_besj.f90    # Bessel J
│   │   │   │   ├── test_besk.f90    # Modified Bessel K
│   │   │   │   ├── test_besy.f90    # Bessel Y
│   │   │   │   └── test_subsidiary.f90 # Test asyik, besknu, etc.
│   │   │   ├── gamma/
│   │   │   ├── error_functions/
│   │   │   └── ...
│   │   ├── D_linear_algebra/
│   │   ├── E_interpolation/
│   │   ├── F_nonlinear_equations/
│   │   ├── G_simulation/
│   │   ├── H_integration/
│   │   ├── I_differential_equations/
│   │   ├── J_transforms/
│   │   ├── K_approximation/
│   │   ├── L_statistics/
│   │   ├── N_data_handling/
│   │   ├── R_service_routines/    # Machine constants, error handling
│   │   │   ├── test_r1mach.f90
│   │   │   ├── test_d1mach.f90
│   │   │   ├── test_i1mach.f90
│   │   │   └── test_xermsg.f90
│   │   └── Z_miscellaneous/
│   │
│   ├── precision_variants/        # Test precision consistency
│   │   ├── test_single_double.f90
│   │   ├── test_real_complex.f90
│   │   └── test_api_consistency.f90
│   │
│   ├── subsidiary/                # Test internal helper functions
│   │   ├── test_subsidiary_access.f90
│   │   └── test_helper_accuracy.f90
│   │
│   ├── regression/                # Prevent regressions
│   │   ├── baseline/              # F77 baseline results
│   │   │   ├── capture_baseline.f90
│   │   │   └── *.baseline
│   │   ├── continuous/            # Continuous regression testing
│   │   └── reports/
│   │
│   ├── performance/               # Performance benchmarks
│   │   ├── micro/                # Individual function benchmarks
│   │   ├── macro/                # Full workflow benchmarks
│   │   ├── memory/               # Memory usage profiling
│   │   └── history/              # Performance over time
│   │
│   ├── integration/               # End-to-end tests
│   │   ├── dependency_chains/    # Test full dependency chains
│   │   ├── scientific_workflows/ # Real-world usage patterns
│   │   ├── error_propagation/    # Error handling through chains
│   │   └── mixed_precision/      # Mixed precision workflows
│   │
│   ├── compatibility/             # F77 interface compatibility
│   │   ├── test_f77_wrappers.f90
│   │   ├── test_argument_order.f90
│   │   └── test_common_blocks.f90
│   │
│   ├── slatec_original/          # Original SLATEC test suite
│   │   ├── downloaded/           # From slatec_chk.tgz
│   │   └── adapted/              # Modified for our framework
│   │
│   └── scripts/                  # Test automation
│       ├── run_tests.sh          # Master test runner
│       ├── test_function.sh      # Test single function
│       ├── test_category.sh      # Test GAMS category
│       ├── generate_report.py    # Generate HTML report
│       ├── check_coverage.py     # Coverage analysis
│       └── dependency_graph.py   # Visualize dependencies
```

## Core Testing Components

### 1. Triple Validation Framework

Unlike simple dual comparison, we use triple validation:

```fortran
module triple_validator
    use iso_fortran_env
    implicit none
    
    type :: validation_result
        real(real64) :: f77_value
        real(real64) :: modern_value
        real(real64) :: reference_value
        real(real64) :: error_f77_modern
        real(real64) :: error_modern_ref
        real(real64) :: error_f77_ref
        logical :: passed
        character(256) :: message
    end type

contains
    subroutine validate_function(func_name, inputs, result)
        character(*), intent(in) :: func_name
        real(real64), intent(in) :: inputs(:)
        type(validation_result), intent(out) :: result
        
        ! Get results from all three sources
        result%f77_value = call_f77_function(func_name, inputs)
        result%modern_value = call_modern_function(func_name, inputs)
        result%reference_value = get_reference_value(func_name, inputs)
        
        ! Compute errors
        result%error_f77_modern = compute_relative_error(result%f77_value, result%modern_value)
        result%error_modern_ref = compute_relative_error(result%modern_value, result%reference_value)
        result%error_f77_ref = compute_relative_error(result%f77_value, result%reference_value)
        
        ! Validation logic
        result%passed = result%error_f77_modern < tolerance_for_precision(func_name)
    end subroutine
end module
```

### 2. Dependency-Aware Testing

SLATEC functions have complex interdependencies. We must test in dependency order:

```fortran
module dependency_tracker
    implicit none
    
    type :: function_dependency
        character(32) :: name
        character(32), allocatable :: depends_on(:)
        logical :: tested
        logical :: migrated
    end type
    
    type(function_dependency), allocatable :: dependency_graph(:)

contains
    subroutine initialize_dependency_graph()
        ! Build from analysis of CALL statements
        ! Example: BESI depends on ALNGAM, ASYIK, I1MACH, R1MACH, XERMSG
    end subroutine
    
    function can_test_function(func_name) result(can_test)
        character(*), intent(in) :: func_name
        logical :: can_test
        type(function_dependency) :: func_dep
        integer :: i
        
        func_dep = find_function(func_name)
        can_test = .true.
        
        ! Check if all dependencies are tested/migrated
        do i = 1, size(func_dep%depends_on)
            if (.not. is_function_available(func_dep%depends_on(i))) then
                can_test = .false.
                exit
            end if
        end do
    end function
end module
```

### 3. Precision Variant Management

Most SLATEC functions have multiple precision variants that must be tested consistently:

```fortran
module precision_handler
    implicit none
    
    type :: precision_variant
        character(32) :: base_name
        logical :: has_single, has_double, has_complex, has_double_complex
        character(32) :: single_name, double_name, complex_name, dcomplex_name
    end type

contains
    subroutine test_all_precisions(base_name)
        character(*), intent(in) :: base_name
        type(precision_variant) :: variant
        
        variant = analyze_variants(base_name)
        
        ! Test each available variant
        if (variant%has_single) then
            call run_precision_tests(variant%single_name, 'single')
        end if
        
        if (variant%has_double) then
            call run_precision_tests(variant%double_name, 'double')
        end if
        
        if (variant%has_complex) then
            call run_precision_tests(variant%complex_name, 'complex')
        end if
        
        if (variant%has_double_complex) then
            call run_precision_tests(variant%dcomplex_name, 'dcomplex')
        end if
        
        ! Cross-validate precision consistency
        call validate_precision_consistency(variant)
    end subroutine
end module
```

### 4. Subsidiary Function Testing

440 of 736 files are subsidiary (internal helper) routines. These require special handling:

```fortran
module subsidiary_testing
    implicit none

contains
    subroutine test_subsidiary_function(name)
        character(*), intent(in) :: name
        
        select case(name)
        case('ASYIK')  ! Bessel asymptotic expansion
            ! Test through BESI, BESK which call ASYIK
            call test_through_parent('BESI', test_asyik_regime)
            call test_through_parent('BESK', test_asyik_regime)
            
        case('BESKNU') ! Bessel K subsidiary
            call test_through_parent('BESK', test_besknu_regime)
            
        case('ALNGAM') ! Log gamma (used by many)
            ! Test directly if exposed, or through parents
            call test_through_multiple_parents(['BESI  ', 'GAMMA ', 'BETA  '])
        end select
    end subroutine
    
    subroutine test_through_parent(parent_func, test_regime)
        character(*), intent(in) :: parent_func
        interface
            subroutine test_regime(x)
                real, intent(in) :: x(:)
            end subroutine
        end interface
        
        ! Design inputs to exercise the subsidiary function
        ! through the parent function's call path
    end subroutine
end module
```

## Test Case Generation Strategy

### Function Classification System

```fortran
module function_classifier
    implicit none
    
    enum, bind(c)
        enumerator :: FUNC_UTILITY = 1      ! ENORM, PYTHAG
        enumerator :: FUNC_SPECIAL = 2      ! BESI, GAMMA, ERF
        enumerator :: FUNC_LINEAR = 3       ! SGESL, SGEFA
        enumerator :: FUNC_SOLVER = 4       ! SNLS1, SNSQ
        enumerator :: FUNC_INTEGRATION = 5  ! QAG, DASSL
        enumerator :: FUNC_SUBSIDIARY = 6   ! Internal helpers
    end enum

contains
    function classify_function(name) result(ftype)
        character(*), intent(in) :: name
        integer :: ftype
        
        ! Check if subsidiary first
        if (is_subsidiary(name)) then
            ftype = FUNC_SUBSIDIARY
            return
        end if
        
        ! Pattern matching for user functions
        if (index(name, 'NORM') > 0 .or. name == 'PYTHAG') then
            ftype = FUNC_UTILITY
        else if (any(name(1:3) == ['BES', 'GAM', 'ERF', 'AIR'])) then
            ftype = FUNC_SPECIAL
        else if (any(name(1:2) == ['SG', 'DG', 'CG', 'ZG'])) then
            ftype = FUNC_LINEAR
        else if (index(name, 'SQ') > 0 .or. index(name, 'NLS') > 0) then
            ftype = FUNC_SOLVER
        else if (name(1:1) == 'Q' .or. index(name, 'ODE') > 0) then
            ftype = FUNC_INTEGRATION
        end if
    end function
end module
```

### Test Value Generation by Type

#### Special Functions (GAMS Category C)
Most critical for scientific computing:

```fortran
subroutine generate_special_function_tests(func_name, test_suite)
    character(*), intent(in) :: func_name
    type(test_suite), intent(out) :: test_suite
    
    select case(func_name)
    case('BESI', 'DBESI', 'CBESI', 'ZBESI')
        ! Modified Bessel I function
        ! Test regimes: series expansion, asymptotic, transitions
        call add_test_regime(test_suite, 'small_argument', generate_small_x())
        call add_test_regime(test_suite, 'medium_argument', generate_medium_x())
        call add_test_regime(test_suite, 'large_argument', generate_large_x())
        call add_test_regime(test_suite, 'order_variation', generate_orders())
        
        ! Reference values from DLMF
        call add_reference_values(test_suite, 'DLMF_bessel_i')
        
        ! Edge cases
        call add_edge_case(test_suite, 'x=0', [0.0_dp])
        call add_edge_case(test_suite, 'negative_order', negative_orders())
        
    case('GAMMA', 'DGAMMA', 'CGAMMA', 'ZGAMMA')
        ! Gamma function - watch for poles
        call add_test_regime(test_suite, 'positive_real', positive_reals())
        call add_test_regime(test_suite, 'near_poles', near_integers())
        call add_test_regime(test_suite, 'half_integers', half_integers())
        
        ! Special values
        call add_exact_value(test_suite, 'Gamma(0.5)', 0.5_dp, sqrt(pi))
        call add_exact_value(test_suite, 'Gamma(1)', 1.0_dp, 1.0_dp)
    end select
end subroutine
```

## Build System Integration

### Dependency-Aware Compilation

```makefile
# Makefile with automatic dependency resolution

# Foundation layer (no dependencies)
FOUNDATION = i1mach.o j4save.o xgetua.o fdump.o

# Error handling (depends on foundation)
ERROR_HANDLING = xermsg.o xerprn.o xersve.o xerhlt.o xercnt.o

# Machine constants
MACHINE_CONSTANTS = r1mach.o d1mach.o

# Core utilities
UTILITIES = enorm.o denorm.o pythag.o dpythg.o

# Special functions with dependencies
besi.o: src/besi.f $(MACHINE_CONSTANTS) $(ERROR_HANDLING) alngam.o asyik.o
	$(FC) $(FFLAGS_F77) -c $< -o $@

dbesi.o: src/dbesi.f $(MACHINE_CONSTANTS) $(ERROR_HANDLING) dalngam.o dasyik.o
	$(FC) $(FFLAGS_F77) -c $< -o $@

# Pattern rules for precision variants
%_test: test_%.f90 %.o d%.o c%.o z%.o
	$(FC) $(FFLAGS_MODERN) $^ -o $@
```

### Mixed F77/F90+ Compilation

```bash
#!/bin/bash
# build_mixed.sh - Handle mixed F77/Modern compilation

# Compiler settings
FC=gfortran-15
FFLAGS_F77="-O2 -std=legacy"
FFLAGS_MODERN="-O2 -std=f2018"

# Compile F77 original
$FC $FFLAGS_F77 -c src/${FUNCTION}.f -o ${FUNCTION}_f77.o

# Compile modern version
$FC $FFLAGS_MODERN -c modern/${FUNCTION}_modern.f90 -o ${FUNCTION}_modern.o

# Link for validation testing
$FC $FFLAGS_MODERN test_${FUNCTION}.f90 \
    ${FUNCTION}_f77.o \
    ${FUNCTION}_modern.o \
    -o validate_${FUNCTION}
```

## Migration Workflow Integration

### Complete Function Migration Pipeline

```bash
#!/bin/bash
# migrate_function.sh - End-to-end migration workflow

FUNCTION=$1
echo "=== Migrating $FUNCTION ==="

# 1. Dependency check
echo "Checking dependencies..."
./tests/scripts/check_dependencies.sh $FUNCTION || {
    echo "ERROR: Dependencies not satisfied"
    exit 1
}

# 2. Generate comprehensive test suite
echo "Generating test cases..."
./tests/scripts/generate_tests.sh $FUNCTION

# 3. Capture F77 baseline behavior
echo "Capturing baseline..."
./tests/scripts/capture_baseline.sh $FUNCTION

# 4. Generate reference values
echo "Generating reference values..."
python3 tests/reference/generators/generate_scipy_refs.py $FUNCTION

# 5. Create modern implementation
echo "Ready for modern implementation in modern/"
echo "Template created at: modern/${FUNCTION}_modern.f90"

# 6. Validate implementation
echo "Running validation..."
./tests/scripts/validate_function.sh $FUNCTION

# 7. Test all precision variants
echo "Testing precision variants..."
./tests/scripts/test_variants.sh $FUNCTION

# 8. Performance comparison
echo "Benchmarking..."
./tests/scripts/benchmark.sh $FUNCTION

# 9. Generate comprehensive report
echo "Generating report..."
python3 tests/scripts/generate_report.py $FUNCTION > reports/${FUNCTION}_migration.html

echo "=== Migration complete for $FUNCTION ==="
```

## Continuous Integration

### GitHub Actions Workflow

```yaml
name: SLATEC Migration Tests

on: [push, pull_request]

jobs:
  test-foundation:
    name: Test Foundation Layer
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup Fortran
        uses: fortran-lang/setup-fortran@v1
        with:
          compiler: gcc
          version: 13
      - name: Test machine constants
        run: |
          cd tests
          ./scripts/test_category.sh R

  test-mathematical:
    name: Test Mathematical Functions
    needs: test-foundation
    strategy:
      matrix:
        category: [C, D, E, F, G, H, I, J, K, L]
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Test GAMS Category ${{ matrix.category }}
        run: |
          cd tests
          ./scripts/test_category.sh ${{ matrix.category }}

  regression:
    name: Regression Tests
    runs-on: ubuntu-latest
    steps:
      - name: Run regression suite
        run: |
          cd tests/regression
          ./run_regression_tests.sh
```

## Test Execution Patterns

### Single Function Test

```bash
# Test a specific function with all variants
./tests/scripts/test_function.sh BESI

# Output:
# Testing BESI family...
# ✓ BESI  (single precision real)      PASSED: 1000/1000 tests
# ✓ DBESI (double precision real)      PASSED: 1000/1000 tests  
# ✓ CBESI (single precision complex)   PASSED: 1000/1000 tests
# ✓ ZBESI (double precision complex)   PASSED: 1000/1000 tests
# ✓ Precision consistency              PASSED
# ✓ Performance comparison             PASSED (Modern 5% faster)
```

### Category Test

```bash
# Test entire GAMS category
./tests/scripts/test_category.sh C

# Tests all special functions in dependency order
```

### Full Validation Suite

```bash
# Run complete validation
make test-all

# Generates comprehensive report in reports/
```

## Error Analysis and Reporting

### Comprehensive Error Metrics

```fortran
module error_analyzer
    use iso_fortran_env
    implicit none
    
    type :: error_metrics
        real(real64) :: absolute_error
        real(real64) :: relative_error
        integer :: ulp_error
        real(real64) :: condition_number
        logical :: backward_stable
    end type

contains
    function analyze_error(computed, reference, input) result(metrics)
        real(real64), intent(in) :: computed, reference
        real(real64), intent(in) :: input(:)
        type(error_metrics) :: metrics
        
        metrics%absolute_error = abs(computed - reference)
        metrics%relative_error = metrics%absolute_error / abs(reference)
        metrics%ulp_error = compute_ulp_error(computed, reference)
        metrics%condition_number = estimate_condition_number(input)
        metrics%backward_stable = check_backward_stability(computed, input)
    end function
end module
```

### HTML Report Generation

```python
# generate_report.py
def generate_migration_report(function_name):
    """Generate comprehensive HTML report for function migration"""
    
    report = MigrationReport(function_name)
    
    # Add sections
    report.add_summary(get_test_summary(function_name))
    report.add_precision_comparison(get_precision_results(function_name))
    report.add_error_distribution(plot_error_distribution(function_name))
    report.add_performance_comparison(get_benchmarks(function_name))
    report.add_dependency_graph(generate_dependency_viz(function_name))
    
    # Generate HTML
    report.save(f"reports/{function_name}_migration.html")
```

## Key Challenges and Solutions

### 1. Dependency Management
**Challenge**: Functions have complex interdependencies
**Solution**: Dependency tracker ensures correct testing order

### 2. Precision Variants
**Challenge**: 2-4 variants per function need consistent testing
**Solution**: Precision handler automates variant testing

### 3. Subsidiary Functions
**Challenge**: 440 internal functions not directly callable
**Solution**: Test through parent functions with targeted inputs

### 4. Build Complexity
**Challenge**: Mixed F77/F90+ compilation
**Solution**: Sophisticated makefile with dependency tracking

### 5. Scale
**Challenge**: 736 files to test systematically
**Solution**: Automated test generation and CI/CD pipeline

## Success Criteria

1. **Numerical Accuracy**
   - Single precision: < 1e-6 relative error
   - Double precision: < 1e-12 relative error
   - Complex: component-wise validation

2. **Test Coverage**
   - 100% of user-callable functions tested
   - All precision variants validated
   - Key subsidiary functions tested through parents

3. **Performance**
   - Modern version within 20% of F77 performance
   - No memory leaks or excessive allocation

4. **Compatibility**
   - F77 interfaces preserved
   - Existing code can link with modern library

5. **Reliability**
   - Zero regressions in nightly tests
   - All edge cases handled correctly

## Next Steps

1. Download `slatec_chk.tgz` for original test suite
2. Set up triple validation framework
3. Begin with Category R (service routines)
4. Systematically migrate by dependency order
5. Generate comprehensive documentation

This testing strategy ensures safe, systematic migration of the entire SLATEC library while maintaining its legendary numerical reliability.
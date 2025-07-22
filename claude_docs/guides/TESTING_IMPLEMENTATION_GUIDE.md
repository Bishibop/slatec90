# SLATEC Testing Implementation Guide

## Purpose
This guide helps AI assistants understand and implement tests for the SLATEC migration project. It provides concrete steps and code examples for testing any of the 736 SLATEC functions during their migration from FORTRAN 77 to modern Fortran.

## Quick Start Checklist

When asked to test a SLATEC function:

1. **Identify the function category**: `grep "GAMS" src/FUNCTION.f`
2. **Check dependencies**: `grep "CALL" src/FUNCTION.f | grep -v "^C"`
3. **Determine precision variants**: Check for s/d/c/z versions
4. **Generate test cases**: `./tests/scripts/generate_tests.sh FUNCTION`
5. **Run validation**: `./tests/scripts/validate_function.sh FUNCTION`

## Understanding SLATEC Structure

### File Count and Types
- **Total Files**: 736 FORTRAN 77 source files
- **User-Callable**: ~300 functions (what users directly call)
- **Subsidiary**: 440 internal helper functions
- **Precision Variants**: Most functions have 2-4 versions:
  - No prefix: Single precision real (e.g., `besi.f`)
  - `d`: Double precision real (e.g., `dbesi.f`)
  - `c`: Complex single precision (e.g., `cbesi.f`)
  - `z`: Complex double precision (e.g., `zbesi.f`)

### Identifying Function Types

```bash
# Check if a function is subsidiary
grep -l "SUBSIDIARY" src/FUNCTION.f

# Find all variants of a function
ls src/*FUNCNAME*.f

# Example: Find all BESI variants
ls src/*besi*.f
# Output: src/besi.f src/dbesi.f src/cbesi.f src/zbesi.f
```

## Step-by-Step Testing Process

### Step 1: Analyze the Function

```bash
#!/bin/bash
# analyze_function.sh
FUNCTION=$1

echo "=== Analyzing $FUNCTION ==="

# 1. Check if subsidiary
if grep -q "SUBSIDIARY" src/${FUNCTION}.f; then
    echo "WARNING: This is a subsidiary function"
    echo "Find parent functions:"
    grep -l "CALL.*${FUNCTION}" src/*.f | head -5
fi

# 2. Extract dependencies
echo -e "\nDependencies:"
grep "CALL" src/${FUNCTION}.f | grep -v "^C" | awk '{print $2}' | sort -u

# 3. Find precision variants
echo -e "\nPrecision variants:"
BASE=$(echo $FUNCTION | sed 's/^[dczs]//')
ls src/*${BASE}*.f 2>/dev/null | xargs -n1 basename

# 4. Check GAMS category
echo -e "\nGAMS Category:"
grep "GAMS" src/${FUNCTION}.f | head -1
```

### Step 2: Generate Test Cases

```fortran
! test_generator_example.f90
module test_generator_for_besi
    use iso_fortran_env
    implicit none

contains
    subroutine generate_besi_tests(test_file)
        character(*), intent(in) :: test_file
        integer :: unit, i, j
        real(real64) :: x, nu
        
        open(newunit=unit, file=test_file, status='replace')
        
        ! Header
        write(unit, '(a)') '# Test cases for BESI (Modified Bessel I)'
        write(unit, '(a)') '# Format: nu, x, expected_value, tolerance'
        
        ! Test different regimes
        
        ! 1. Small argument regime (series expansion)
        do i = 0, 5
            nu = real(i, real64)
            do j = 1, 10
                x = 0.1_real64 * j
                write(unit, '(3es25.15e3, es12.3e3)') nu, x, compute_reference(nu, x), 1e-12_real64
            end do
        end do
        
        ! 2. Medium argument regime
        do i = 0, 5
            nu = real(i, real64)
            do j = 1, 10
                x = real(j, real64)
                write(unit, '(3es25.15e3, es12.3e3)') nu, x, compute_reference(nu, x), 1e-12_real64
            end do
        end do
        
        ! 3. Large argument regime (asymptotic expansion)
        do i = 0, 5
            nu = real(i, real64)
            do j = 1, 10
                x = 10.0_real64 + 10.0_real64 * j
                write(unit, '(3es25.15e3, es12.3e3)') nu, x, compute_reference(nu, x), 1e-10_real64
            end do
        end do
        
        ! 4. Edge cases
        write(unit, '(a)') '# Edge cases'
        write(unit, '(3es25.15e3, es12.3e3)') 0.0_real64, 0.0_real64, 1.0_real64, 1e-15_real64
        write(unit, '(3es25.15e3, es12.3e3)') 1.0_real64, 0.0_real64, 0.0_real64, 1e-15_real64
        
        close(unit)
    end subroutine
    
    function compute_reference(nu, x) result(value)
        real(real64), intent(in) :: nu, x
        real(real64) :: value
        ! This would call SciPy or use high-precision calculation
        ! For now, placeholder
        value = 0.0_real64
    end function
end module
```

### Step 3: Create Test Driver

```fortran
! test_besi_driver.f90
program test_besi_variants
    use iso_fortran_env
    use test_framework
    implicit none
    
    type(test_results) :: results
    character(len=100) :: test_name
    
    ! Initialize test framework
    call initialize_tests('BESI Function Family')
    
    ! Test each precision variant
    if (function_exists('besi')) then
        test_name = 'BESI - Single Precision Real'
        call test_single_precision('besi', test_name, results)
    end if
    
    if (function_exists('dbesi')) then
        test_name = 'DBESI - Double Precision Real'
        call test_double_precision('dbesi', test_name, results)
    end if
    
    if (function_exists('cbesi')) then
        test_name = 'CBESI - Single Precision Complex'
        call test_complex_single('cbesi', test_name, results)
    end if
    
    if (function_exists('zbesi')) then
        test_name = 'ZBESI - Double Precision Complex'
        call test_complex_double('zbesi', test_name, results)
    end if
    
    ! Cross-validate precision variants
    call validate_precision_consistency('besi', results)
    
    ! Generate report
    call generate_test_report('besi', results)
    
contains
    subroutine test_single_precision(func_name, test_name, results)
        character(*), intent(in) :: func_name, test_name
        type(test_results), intent(inout) :: results
        
        real :: x, nu, computed, expected
        real :: f77_result, modern_result
        integer :: n_tests, n_passed
        
        ! Read test cases
        call read_test_cases(func_name, test_name)
        
        ! Run each test
        do i = 1, n_tests
            ! Call F77 version
            f77_result = besi_f77(nu, x)
            
            ! Call modern version (if available)
            if (modern_exists(func_name)) then
                modern_result = besi_modern(nu, x)
            else
                modern_result = f77_result
            end if
            
            ! Validate
            call validate_result(f77_result, modern_result, expected, results)
        end do
    end subroutine
end program
```

### Step 4: Test Subsidiary Functions

Subsidiary functions (440 of 736) cannot be called directly. Test them through parent functions:

```fortran
! test_subsidiary_asyik.f90
module test_asyik_subsidiary
    ! ASYIK is used by BESI and BESK for large arguments
    implicit none

contains
    subroutine test_asyik_through_parents()
        real(real64) :: x, nu
        real(real64) :: besi_result, besk_result
        
        ! Test large argument regime where ASYIK is called
        do i = 1, 10
            x = 50.0_real64 + 10.0_real64 * i  ! Large x triggers ASYIK
            nu = 0.5_real64
            
            ! These calls will internally use ASYIK
            besi_result = dbesi(nu, x)
            besk_result = dbesk(nu, x)
            
            ! Validate against reference
            call validate_asymptotic_behavior(nu, x, besi_result, besk_result)
        end do
    end subroutine
    
    subroutine validate_asymptotic_behavior(nu, x, besi_val, besk_val)
        real(real64), intent(in) :: nu, x, besi_val, besk_val
        real(real64) :: expected_ratio
        
        ! For large x, I_nu(x) * K_nu(x) approaches 1/(2x)
        expected_ratio = 1.0_real64 / (2.0_real64 * x)
        
        if (abs(besi_val * besk_val - expected_ratio) > 1e-10_real64) then
            print *, "ASYIK validation failed at x=", x
        end if
    end subroutine
end module
```

### Step 5: Validate Dependencies

```bash
#!/bin/bash
# validate_dependencies.sh
FUNCTION=$1

# Check if all dependencies are available
MISSING_DEPS=()
for dep in $(grep "CALL" src/${FUNCTION}.f | grep -v "^C" | awk '{print $2}'); do
    # Check if original exists
    if [[ ! -f "src/${dep,,}.f" ]]; then
        # Check if it's a system routine
        if [[ ! " ${SYSTEM_ROUTINES[@]} " =~ " ${dep} " ]]; then
            MISSING_DEPS+=($dep)
        fi
    fi
done

if [ ${#MISSING_DEPS[@]} -gt 0 ]; then
    echo "ERROR: Missing dependencies for $FUNCTION:"
    printf '%s\n' "${MISSING_DEPS[@]}"
    exit 1
fi

echo "All dependencies satisfied for $FUNCTION"
```

## Test Execution Commands

### Basic Testing
```bash
# Test a single function
./tests/scripts/test_function.sh besi

# Test all variants of a function
./tests/scripts/test_variants.sh besi

# Test a complete GAMS category
./tests/scripts/test_category.sh C
```

### Advanced Testing
```bash
# Test with specific tolerances
TOLERANCE=1e-14 ./tests/scripts/test_function.sh dbesi

# Test subsidiary functions
./tests/scripts/test_subsidiary.sh asyik

# Generate comprehensive report
./tests/scripts/full_validation.sh besi > reports/besi_validation.html
```

## Common Testing Patterns

### Pattern 1: Special Function Testing
```fortran
! Special functions often have:
! 1. Different algorithms for different argument ranges
! 2. Special values (poles, zeros, branch cuts)
! 3. Recurrence relations to validate

subroutine test_special_function_pattern(func_name)
    ! Test small arguments (series expansion)
    call test_small_args(func_name)
    
    ! Test medium arguments (main algorithm)
    call test_medium_args(func_name)
    
    ! Test large arguments (asymptotic expansion)
    call test_large_args(func_name)
    
    ! Test special points
    call test_special_values(func_name)
    
    ! Validate recurrence relations
    call test_recurrence_relations(func_name)
end subroutine
```

### Pattern 2: Linear Algebra Testing
```fortran
! Linear algebra functions need:
! 1. Well-conditioned test cases
! 2. Ill-conditioned test cases
! 3. Special matrix structures
! 4. Error propagation analysis

subroutine test_linear_algebra_pattern(func_name)
    ! Test with identity matrix
    call test_identity_matrix(func_name)
    
    ! Test with random well-conditioned matrices
    call test_well_conditioned(func_name)
    
    ! Test with ill-conditioned matrices
    call test_ill_conditioned(func_name)
    
    ! Test special structures (symmetric, banded, etc.)
    call test_special_structures(func_name)
end subroutine
```

## Debugging Test Failures

### Common Issues and Solutions

1. **Precision Variant Inconsistency**
   ```bash
   # Compare implementations of different precisions
   diff -y src/besi.f src/dbesi.f | less
   ```

2. **Dependency Not Found**
   ```bash
   # Check if dependency is a subsidiary function
   grep -l "SUBSIDIARY" src/DEPENDENCY.f
   
   # Find where it's defined
   grep -l "FUNCTION DEPENDENCY\|SUBROUTINE DEPENDENCY" src/*.f
   ```

3. **Numerical Differences**
   ```fortran
   ! Use ULP (Units in Last Place) comparison
   function ulp_difference(a, b) result(ulps)
       real(real64), intent(in) :: a, b
       integer(int64) :: ulps
       integer(int64) :: ia, ib
       
       ia = transfer(a, ia)
       ib = transfer(b, ib)
       ulps = abs(ia - ib)
   end function
   ```

## Build System Integration

### Compiling Tests
```makefile
# Makefile fragment for tests
test_besi: test_besi.f90 besi.o dbesi.o cbesi.o zbesi.o
    $(FC) $(FFLAGS) -I$(TEST_FRAMEWORK) $^ -o $@

# Pattern rule for all tests
test_%: test_%.f90 %.o $(COMMON_DEPS)
    $(FC) $(FFLAGS) -I$(TEST_FRAMEWORK) $^ -o $@
```

### Mixed Compilation
```bash
# F77 original
gfortran -std=legacy -c src/besi.f -o besi_f77.o

# Modern version
gfortran -std=f2018 -c modern/besi_modern.f90 -o besi_modern.o

# Test driver (modern)
gfortran -std=f2018 test_besi.f90 besi_f77.o besi_modern.o -o test_besi
```

## Continuous Integration

### GitHub Actions Example
```yaml
name: Test SLATEC Function

on:
  workflow_dispatch:
    inputs:
      function:
        description: 'Function to test'
        required: true

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Fortran
        uses: fortran-lang/setup-fortran@v1
        
      - name: Check dependencies
        run: ./tests/scripts/validate_dependencies.sh ${{ github.event.inputs.function }}
        
      - name: Generate tests
        run: ./tests/scripts/generate_tests.sh ${{ github.event.inputs.function }}
        
      - name: Run validation
        run: ./tests/scripts/validate_function.sh ${{ github.event.inputs.function }}
        
      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: test-results-${{ github.event.inputs.function }}
          path: reports/
```

## Best Practices

1. **Always Test All Variants**: If testing BESI, also test DBESI, CBESI, ZBESI
2. **Check Dependencies First**: Use the dependency validation script
3. **Generate Comprehensive Test Cases**: Cover all argument regimes
4. **Use Triple Validation**: F77 vs Modern vs Reference (when available)
5. **Document Special Cases**: Note any algorithm changes between variants
6. **Test Subsidiary Functions**: Through their parent functions
7. **Maintain Backward Compatibility**: Ensure F77 interfaces work

## Quick Reference Card

```bash
# Most common commands for testing a function

# 1. Analyze function
./analyze_function.sh BESI

# 2. Check dependencies
./validate_dependencies.sh BESI

# 3. Generate tests
./generate_tests.sh BESI

# 4. Run tests
./test_function.sh BESI

# 5. Test all variants
./test_variants.sh BESI

# 6. Generate report
./generate_report.sh BESI

# 7. Full migration test
./migrate_and_test.sh BESI
```

## Troubleshooting

If tests fail:
1. Check error messages for missing dependencies
2. Verify all precision variants are tested
3. Look for subsidiary function issues
4. Check for numerical tolerance problems
5. Ensure proper build flags for F77/F90+

For help with specific functions, consult:
- `DEPENDENCY_MAP.md` for function relationships
- `SLATEC_TESTING_STRATEGY_V2.md` for detailed test patterns
- Original SLATEC documentation in source files
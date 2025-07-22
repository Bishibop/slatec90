# AI Assistant Onboarding for SLATEC Migration

## Welcome to the SLATEC Migration Project!

You're about to work on migrating a legendary mathematical library from FORTRAN 77 to modern Fortran. SLATEC (Sandia, Los Alamos, Air Force Weapons Laboratory Technical Exchange Committee) has been a cornerstone of scientific computing since 1993.

## Project Overview

### What is SLATEC?
- **736 FORTRAN 77 source files** containing highly optimized mathematical algorithms
- **~300 user-callable functions** (each with 2-4 precision variants)
- **440 subsidiary routines** (internal helpers not directly callable)
- **14 GAMS categories** covering everything from special functions to ODE solvers
- **54% documentation** - exceptionally well-documented legacy code

### Current State
- **Completed**: Analysis phase, Lambert W function (proof of concept), initial ENORM migration
- **In Progress**: Testing framework setup, foundation layer migration
- **Goal**: Systematic migration preserving numerical accuracy while gaining modern Fortran benefits

## Your First Steps

### 1. Read Core Documentation (In Order)
```bash
# Start here - understand the project structure
1. REPO_MAP.xml                    # Complete project overview and state
2. SLATEC_MIGRATION_PLAN.md        # Strategic approach
3. DEPENDENCY_MAP.md               # Critical for understanding function relationships
4. SLATEC_TESTING_STRATEGY_V2.md   # Comprehensive testing approach
5. TESTING_IMPLEMENTATION_GUIDE.md # Practical testing how-to
```

### 2. Understand the Code Organization
```bash
# Check current directory structure
ls -la

# Key directories:
src/        # Original SLATEC F77 source (736 files)
modern/     # Modern F90+ implementations go here
tests/      # Comprehensive test framework
validation/ # Existing validation examples
wrappers/   # F77 compatibility interfaces
```

### 3. Learn the Naming Conventions
```
Function naming patterns:
- No prefix: Single precision real (e.g., besi)
- d prefix: Double precision real (e.g., dbesi)  
- c prefix: Complex single (e.g., cbesi)
- z prefix: Complex double (e.g., zbesi)
- s prefix: Often single precision linear algebra

Example: BESI (Modified Bessel I) has 4 variants:
- besi.f   (single real)
- dbesi.f  (double real)
- cbesi.f  (complex single)
- zbesi.f  (complex double)
```

## Common Tasks You'll Be Asked to Do

### Task 1: Analyze a Function
```bash
# Quick analysis of any function
grep "SUBSIDIARY" src/FUNCTION.f  # Check if subsidiary
grep "CALL" src/FUNCTION.f | grep -v "^C"  # Find dependencies
ls src/*FUNCTION*.f  # Find all precision variants
```

### Task 2: Check Dependencies
```bash
# Before migrating any function, check its dependencies
./tests/scripts/check_dependencies.sh BESI

# Manual check
grep "CALL" src/besi.f | grep -v "^C" | awk '{print $2}'
```

### Task 3: Migrate a Function
```bash
# Full migration workflow
./migrate_function.sh ENORM

# This will:
# 1. Check dependencies are satisfied
# 2. Generate test cases
# 3. Capture F77 baseline behavior
# 4. Create modern template
# 5. Run validation tests
# 6. Test all precision variants
# 7. Generate report
```

### Task 4: Test a Function
```bash
# Test single function
./tests/scripts/test_function.sh BESI

# Test all variants
./tests/scripts/test_variants.sh BESI

# Test entire GAMS category
./tests/scripts/test_category.sh C
```

## Key Concepts to Remember

### 1. Dependencies Are Critical
- **Foundation layer** (machine constants, error handling) must be migrated first
- Functions can only be migrated after their dependencies
- Use DEPENDENCY_MAP.md to understand relationships

### 2. Precision Variants
- Most functions have 2-4 precision variants
- Each variant needs testing
- Variants should be numerically consistent

### 3. Subsidiary Functions
- 440 of 736 files are internal helpers
- Cannot be called directly
- Must be tested through parent functions

### 4. Testing Philosophy
- **Triple validation**: F77 vs Modern vs Reference (SciPy/mpmath)
- **Preserve numerical accuracy**: <1e-6 for single, <1e-12 for double
- **Test all variants**: Ensure consistency across precisions

## Build System Essentials

### Compilation Flags
```bash
# F77 compilation (original)
gfortran -std=legacy -O2 src/function.f

# Modern compilation  
gfortran -std=f2018 -O2 modern/function_modern.f90

# Mixed linking for validation
gfortran test.f90 function_f77.o function_modern.o
```

### Common Dependencies to Link
```bash
# Error handling chain (always needed)
XERMSG_DEPS="xermsg.o xerprn.o xersve.o xerhlt.o xercnt.o fdump.o"

# Machine constants
MACHINE_DEPS="i1mach.o j4save.o xgetua.o r1mach.o d1mach.o"

# Example compilation
gfortran -std=legacy test_besi.f besi.o alngam.o asyik.o $MACHINE_DEPS $XERMSG_DEPS
```

## Migration Priority Order

Based on dependencies, always migrate in this order:

1. **Foundation** (Level 0-1)
   - Machine constants (I1MACH, R1MACH, D1MACH)
   - Error handling (XERMSG system)

2. **Core Utilities** (Level 2)
   - ENORM, PYTHAG
   - Basic array operations

3. **Mathematical Foundations** (Level 3)
   - ALNGAM (critical - used by many)
   - Elementary special functions

4. **Complex Functions** (Level 4+)
   - Special functions (Bessel, Airy, etc.)
   - Numerical methods

## Common Commands Reference

```bash
# Analysis
./analyze_function.sh FUNCTION      # Complete analysis
./check_dependencies.sh FUNCTION    # Verify dependencies ready

# Testing
./test_function.sh FUNCTION         # Test single function
./test_variants.sh FUNCTION         # Test all precision variants
./test_category.sh C                # Test GAMS category

# Migration
./migrate_function.sh FUNCTION      # Full migration workflow
./capture_baseline.sh FUNCTION      # Capture F77 behavior
./validate_function.sh FUNCTION     # Validate modern vs F77

# Reporting
./generate_report.py FUNCTION       # Generate HTML report
./check_coverage.py                 # Overall progress
```

## When You Get Stuck

### Function Won't Compile
1. Check missing dependencies: `./check_dependencies.sh FUNCTION`
2. Verify all .o files are linked
3. Check for F77-specific constructs that need updating

### Tests Failing
1. Check numerical tolerances (single vs double precision)
2. Verify test data generation is appropriate for function
3. Look for algorithm differences between precision variants
4. Check if subsidiary functions are affecting results

### Can't Find Something
1. Subsidiary functions are in src/ but marked "SUBSIDIARY"
2. Test data goes in tests/reference/
3. Modern implementations go in modern/
4. Reports go in reports/

## Best Practices

1. **Always check dependencies first** - saves time and prevents errors
2. **Test incrementally** - validate as you develop
3. **Document special cases** - note any deviations from F77 behavior
4. **Use existing patterns** - look at completed migrations (LAMW, ENORM)
5. **Ask about subsidiaries** - they're tricky and need special handling

## Next Steps

1. **Familiarize yourself** with the codebase structure
2. **Try a simple task** like analyzing a function
3. **Look at examples**:
   - `src/lamw.f` - Successfully added new function
   - `modern/utilities/enorm_module.f90` - Modern implementation example
   - `validation/enorm_validation.f90` - Validation example

## Remember

- This is a large, complex project - take it one function at a time
- Numerical accuracy is paramount - when in doubt, validate more
- The existing code is battle-tested - preserve its reliability
- You're modernizing legendary code used by scientists worldwide!

Good luck with the migration! 🚀
# Immediate Test Enablement Plan

## Functions Ready for Testing NOW

Since we've already migrated:
- **LSAME**: Used by many BLAS Level 2/3 routines
- **ENORM**: Euclidean norm calculation
- **I1MACH, R1MACH, D1MACH**: Machine constants

## Zero-Dependency Functions to Migrate First

These BLAS Level 1 functions have NO dependencies and can be migrated immediately to enable test17 (BLAS tests):

### High Priority (tested by CHECK1 in test17)
1. **ISAMAX**: Find index of max absolute value element
2. **SASUM**: Sum of absolute values  
3. **SSCAL**: Scale a vector by a constant
4. **SNRM2**: Euclidean norm (complement to our ENORM)

### Also Zero-Dependency (tested by CHECK2)
5. **SAXPY**: y = a*x + y operation
6. **SCOPY**: Copy vector
7. **SDOT**: Dot product

## Minimal Error Handling Chain

To enable BLAS Level 2/3 tests that use XERBLA:

1. **FDUMP** (no dependencies) - Symbolic dump routine
2. **J4SAVE** (no dependencies) - Save/recall error parameters
3. **XERCNT** (depends on J4SAVE) - Error counter
4. **XERHLT** (depends on I1MACH ✅) - Error halt routine
5. **XERMSG** (depends on all above) - Main error message handler
6. **XERBLA** (depends on all above) - BLAS error handler

## Immediate Action Plan

### Week 1: Enable Basic BLAS Tests
1. Migrate 3-4 zero-dependency BLAS functions (ISAMAX, SASUM, SSCAL, SAXPY)
2. Create test harness to run specific routines from CHECK1/CHECK2
3. Validate against original F77 implementations

### Week 2: Complete Error Handling
1. Migrate FDUMP and J4SAVE (both zero-dependency)
2. Migrate remaining error chain (XERCNT, XERHLT, XERMSG, XERBLA)
3. Enable BLAS Level 2/3 tests that depend on XERBLA

### Week 3: Expand Test Coverage  
1. Complete remaining zero-dependency BLAS Level 1
2. Migrate AAAAAA to enable test01
3. Begin systematic migration of functions with simple dependencies

## Benefits

- Can start running original SLATEC tests within days
- Each migrated function immediately enables more tests
- Build confidence through passing original test suite
- Establish testing infrastructure early

## Compilation Strategy for Testing

### Running SLATEC Tests with Modern Implementations

Since we're using `gfortran` which understands both F77 and F90, we can compile and link everything together without wrappers:

#### Example: Testing BLAS Functions in TEST17

```bash
# Step 1: Compile already migrated F90 modules
gfortran -c modern/lsame_modern.f90
gfortran -c modern/enorm_modern.f90
gfortran -c modern/i1mach_modern.f90

# Step 2: Compile new BLAS migrations (once complete)
gfortran -c modern/isamax_modern.f90
gfortran -c modern/sasum_modern.f90

# Step 3: Compile F77 test programs
gfortran -c test17.f -std=legacy
gfortran -c blachk.f -std=legacy  # BLAS quick check routine

# Step 4: Link everything
gfortran test17.o blachk.o lsame_modern.o isamax_modern.o sasum_modern.o i1mach_modern.o -o test17

# Step 5: Run test
echo "0" | ./test17  # KPRINT=0 for quick check
```

### Key Points

1. **Compilation Order**: F90 modules must be compiled before F77 code that uses them
2. **No Wrappers**: F77 calls like `CALL LSAME(A,B)` directly invoke the F90 module function
3. **Legacy Flag**: Use `-std=legacy` for F77 code to handle old-style constructs
4. **Symbol Export**: F90 modules automatically export public functions with correct names

### Testing Individual Functions

Before running full test programs, validate individual functions:

```bash
# Create simple F77 test program
cat > test_isamax.f <<EOF
      PROGRAM TESTISAM
      INTEGER ISAMAX
      EXTERNAL ISAMAX
      REAL X(5)
      DATA X/1.0, -3.0, 2.0, -5.0, 4.0/
      PRINT *, 'ISAMAX result:', ISAMAX(5, X, 1)
      END
EOF

# Compile and run
gfortran -c modern/isamax_modern.f90
gfortran -c test_isamax.f -std=legacy
gfortran test_isamax.o isamax_modern.o -o test_isamax
./test_isamax
```

## Next Immediate Step

Start with **ISAMAX** or **SASUM** - both are simple, have zero dependencies, and will immediately enable parts of test17. Use the compilation strategy above to validate each function as it's migrated.
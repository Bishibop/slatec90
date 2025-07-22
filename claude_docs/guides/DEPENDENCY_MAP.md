# SLATEC Function Dependency Map

## Overview

This document maps the dependency relationships between SLATEC's 736 functions. Understanding these dependencies is critical for:
1. Determining migration order (dependencies must be migrated first)
2. Testing subsidiary functions through their parents
3. Identifying circular dependencies that need special handling
4. Building the library in correct compilation order

## Dependency Hierarchy

### Level 0: Foundation Layer (No Dependencies)
These functions have no dependencies and must be migrated first.

#### Machine Constants
- **I1MACH** - Integer machine constants
- **R1MACH** - Single precision machine constants  
- **D1MACH** - Double precision machine constants

#### Primitive Operations
- **J4SAVE** - Save/recall error handling state
- **FDUMP** - Dump error messages

### Level 1: Error Handling System
Depends only on Level 0 functions.

- **XGETUA** → J4SAVE
- **XERMSG** → I1MACH, XGETUA, FDUMP
- **XERPRN** → XERMSG
- **XERSVE** → J4SAVE
- **XERHLT** → (system dependent)
- **XERCNT** → J4SAVE

### Level 2: Basic Utilities
Depends on Level 0-1 functions.

#### Norms and Elementary Functions
- **ENORM** → R1MACH
- **DENORM** → D1MACH
- **PYTHAG** → (none)
- **DPYTHG** → (none)

#### Basic Array Operations
- **SCOPY** → (none)
- **SSCAL** → (none)
- **SDOT** → (none)
- **SAXPY** → (none)
- **ISAMAX** → (none)
- **SASUM** → (none)

### Level 3: Mathematical Building Blocks
Depends on Level 0-2 functions.

#### Gamma Functions (Critical - Used by Many)
- **ALNGAM** → R1MACH, XERMSG
- **DALNGAM** → D1MACH, XERMSG
- **GAMMA** → ALNGAM, R1MACH, XERMSG
- **DGAMMA** → DALNGAM, D1MACH, XERMSG

#### Elementary Special Functions
- **ERF** → R1MACH, XERMSG
- **DERF** → D1MACH, XERMSG
- **ERFC** → ERF, R1MACH, XERMSG
- **DERFC** → DERF, D1MACH, XERMSG

### Level 4: Complex Special Functions
Depends on Level 0-3 functions.

#### Bessel Functions Family
Each Bessel function has 4 precision variants (no prefix, D, C, Z).

**Modified Bessel I:**
- **BESI** → ALNGAM, ASYIK, I1MACH, R1MACH, XERMSG
- **DBESI** → DALNGAM, DASYIK, I1MACH, D1MACH, XERMSG
- **CBESI** → Complex equivalents
- **ZBESI** → Complex double equivalents

**Bessel J:**
- **BESJ** → ASYJY, I1MACH, R1MACH, XERMSG  
- **DBESJ** → DASYJY, I1MACH, D1MACH, XERMSG

**Modified Bessel K:**
- **BESK** → BESI, BESKNU, I1MACH, R1MACH, XERMSG
- **DBESK** → DBESI, DBSKNU, I1MACH, D1MACH, XERMSG

**Bessel Y:**
- **BESY** → BESJ, BESYNU, I1MACH, R1MACH, XERMSG
- **DBESY** → DBESJ, DBSYNU, I1MACH, D1MACH, XERMSG

#### Bessel Subsidiary Functions (Not User-Callable)
- **ASYIK** - Asymptotic expansion for I and K
- **ASYJY** - Asymptotic expansion for J and Y  
- **BESKNU** - K Bessel subsidiary
- **BESYNU** - Y Bessel subsidiary
- **BKIAS** - I and K asymptotic subsidiary
- **BKISR** - I and K series subsidiary
- **JAIRY** - Airy function subsidiary

### Level 5: Integration and ODE Solvers

#### QUADPACK Integration Family
- **QAG** → QAGE, XERMSG, R1MACH
- **QAGE** → QK15, QK21, QK31, QK41, QK51, QK61, R1MACH
- **QAGS** → QAGSE, XERMSG
- **QAGSE** → QK21, QELG, QPSRT, R1MACH
- **QNG** → XERMSG, R1MACH
- **QAWS** → QAWSE, XERMSG
- **QAWC** → QAWCE, XERMSG

#### Differential Equation Solvers
- **DASSL** → Multiple dependencies including linear algebra
- **LSODE** → Complex dependency chain
- **RKF45** → XERMSG, R1MACH, multiple subsidiaries

### Level 6: Linear Algebra
Complex interdependencies within this category.

#### LINPACK Routines
- **SGEFA** → SAXPY, SSCAL, ISAMAX
- **SGESL** → SAXPY, SDOT
- **SGECO** → SGEFA, SASUM, SAXPY, SSCAL, SDOT

#### EISPACK Routines  
- **SGEEV** → Multiple EISPACK subsidiaries
- **SSVDC** → SROT, SAXPY, SNRM2, SSCAL

## Circular Dependencies

Some functions have circular or mutual dependencies that require special handling:

1. **Bessel Functions**: Some Bessel functions call each other for specific argument ranges
2. **Linear Algebra**: Matrix decomposition routines often have mutual dependencies
3. **ODE Solvers**: Complex state machines with interconnected routines

## Subsidiary Function Parent Mapping

Mapping of key subsidiary functions to their parent functions (for testing):

| Subsidiary | Parent Functions | Test Through |
|------------|------------------|--------------|
| ASYIK | BESI, BESK | Large argument tests |
| ASYJY | BESJ, BESY | Large argument tests |
| ALNGAM | BESI, GAMMA, BETA, many others | Various special functions |
| BESKNU | BESK | Fractional order tests |
| BESYNU | BESY | Fractional order tests |
| QK21 | QAGE, QAGSE | Integration tests |
| QELG | QAGSE | Epsilon algorithm tests |

## Migration Priority Groups

Based on dependencies, functions should be migrated in this order:

### Priority 1: Foundation (Must be first)
1. Machine constants (I1MACH, R1MACH, D1MACH)
2. Error handling system (XERMSG and related)
3. Basic utilities (J4SAVE, FDUMP)

### Priority 2: Core Utilities
1. Vector operations (SCOPY, SSCAL, SDOT, SAXPY)
2. Norms (ENORM, PYTHAG)
3. Elementary functions

### Priority 3: Mathematical Foundations
1. Gamma functions (ALNGAM, GAMMA) - Critical for many functions
2. Error functions (ERF, ERFC)
3. Exponential integrals

### Priority 4: Special Functions
1. Bessel functions (complete families)
2. Airy functions
3. Elliptic integrals

### Priority 5: Numerical Methods
1. Integration (QUADPACK family)
2. ODE solvers
3. Linear algebra

## Dependency Analysis Commands

### Find all dependencies of a function:
```bash
#!/bin/bash
# get_dependencies.sh
FUNCTION=$1
grep "CALL" src/${FUNCTION}.f | grep -v "^C" | awk '{print $2}' | tr '[:upper:]' '[:lower:]' | sort -u
```

### Find all functions that depend on a given function:
```bash
#!/bin/bash  
# find_dependents.sh
FUNCTION=$1
grep -l "CALL.*${FUNCTION}" src/*.f | xargs -n1 basename | sed 's/\.f$//'
```

### Check circular dependencies:
```bash
#!/bin/bash
# check_circular.sh
FUNCTION=$1
DEPS=$(./get_dependencies.sh $FUNCTION)
for dep in $DEPS; do
    DEP_DEPS=$(./get_dependencies.sh $dep)
    if echo $DEP_DEPS | grep -q $FUNCTION; then
        echo "Circular: $FUNCTION <-> $dep"
    fi
done
```

## Building with Dependencies

### Makefile Dependency Rules

```makefile
# Foundation layer
i1mach.o: src/i1mach.f
j4save.o: src/j4save.f i1mach.o
xgetua.o: src/xgetua.f j4save.o

# Error handling
xermsg.o: src/xermsg.f i1mach.o xgetua.o
xerprn.o: src/xerprn.f xermsg.o

# Mathematical functions
alngam.o: src/alngam.f r1mach.o xermsg.o
gamma.o: src/gamma.f alngam.o r1mach.o xermsg.o

# Bessel functions
besi.o: src/besi.f alngam.o asyik.o i1mach.o r1mach.o xermsg.o
asyik.o: src/asyik.f  # Subsidiary - compiled but not user-callable
```

## Testing Considerations

### Testing Order
1. Test dependencies before dependents
2. Use validated dependencies to test new functions
3. For subsidiary functions, test through multiple parent functions

### Dependency Validation
```fortran
module dependency_validator
    implicit none
contains
    function validate_dependencies_available(func_name) result(available)
        character(*), intent(in) :: func_name
        logical :: available
        character(32), allocatable :: deps(:)
        integer :: i
        
        deps = get_function_dependencies(func_name)
        available = .true.
        
        do i = 1, size(deps)
            if (.not. is_function_tested(deps(i))) then
                print *, "WARNING: Dependency not tested:", deps(i)
                available = .false.
            end if
        end do
    end function
end module
```

## Key Insights

1. **Foundation First**: Machine constants and error handling must be migrated before anything else
2. **ALNGAM is Critical**: Used by many special functions - high priority
3. **Subsidiary Functions**: 440 functions that can only be tested through parents
4. **Precision Variants**: Dependencies are usually consistent across variants
5. **Complex Interdependencies**: Some function families have complex mutual dependencies

## Next Steps

1. Use this map to determine migration order
2. Always check dependencies before migrating a function
3. Test subsidiary functions through their documented parent functions
4. Update this map as new dependencies are discovered
5. Use automated tools to validate dependency compliance
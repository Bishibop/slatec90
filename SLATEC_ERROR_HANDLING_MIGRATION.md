# SLATEC Error Handling Migration Guide

**Date**: July 24, 2025  
**Scope**: Complete migration strategy for SLATEC error handling system  
**Goal**: Eliminate XERMSG and related functions in favor of modern Fortran patterns

## Executive Summary

The SLATEC error handling system, centered around XERMSG and J4SAVE, represents a sophisticated 1980s-era approach to error management. For modernization, we will completely eliminate this system in favor of modern Fortran patterns that are simpler, thread-safe, and more maintainable.

## Current Error System Architecture

### Core Components

1. **XERMSG** - Central error message handler (251 files, 1,103 calls)
   - Formats and prints error messages
   - Controls program flow (continue/abort)
   - Tracks error counts and history
   - Manages error levels and severity

2. **J4SAVE** - Global state storage (14 direct callers)
   - Stores 9 global parameters
   - Maintains error numbers, control flags, unit numbers
   - Uses SAVE statement for persistence

3. **Supporting Functions**:
   - **XERHLT** - Halts execution
   - **XERPRN** - Prints error messages
   - **XERSVE** - Saves error history
   - **XERCNT** - User-defined error overrides
   - **XGETUA/XSETUA** - Get/set output units
   - **NUMXER** - Returns error count
   - **FDUMP** - Prints error summary

### Error Levels

```
Level -1: Warning, print once only
Level  0: Warning, print every time
Level  1: Recoverable error (may abort based on control flag)
Level  2: Fatal error (always aborts)
```

## Modern Fortran Alternatives

### 1. For Machine Constants (No Error Handling Needed)

```fortran
module machine_constants
  use, intrinsic :: iso_fortran_env
  use, intrinsic :: ieee_arithmetic
  implicit none
  
  ! Direct intrinsic replacements
  real(real32), parameter :: r1_tiny = tiny(1.0_real32)
  real(real32), parameter :: r1_huge = huge(1.0_real32)
  real(real64), parameter :: d1_tiny = tiny(1.0_real64)
  real(real64), parameter :: d1_huge = huge(1.0_real64)
  
contains
  ! For backward compatibility only
  pure function r1mach(i) result(val)
    integer, intent(in) :: i
    real(real32) :: val
    
    select case(i)
      case(1); val = r1_tiny
      case(2); val = r1_huge
      case(3); val = epsilon(1.0_real32)
      case(4); val = epsilon(1.0_real32)
      case(5); val = log10(real(radix(1.0_real32)))
      case default; val = 0.0_real32  ! No error needed
    end select
  end function
end module
```

### 2. For Fatal Errors (Level 2)

**Old pattern**:
```fortran
CALL XERMSG('LIBRARY', 'ROUTINE', 'FATAL ERROR MESSAGE', 1, 2)
```

**Modern pattern**:
```fortran
error stop 'ROUTINE: Fatal error message'
```

### 3. For Recoverable Errors (Level 1)

**Old pattern**:
```fortran
CALL XERMSG('LIBRARY', 'ROUTINE', 'INVALID INPUT', 1, 1)
```

**Modern pattern using optional status**:
```fortran
pure subroutine compute(x, result, stat, errmsg)
  real, intent(in) :: x
  real, intent(out) :: result
  integer, intent(out), optional :: stat
  character(len=*), intent(out), optional :: errmsg
  
  if (present(stat)) stat = 0
  
  if (x < 0) then
    result = 0.0
    if (present(stat)) stat = 1
    if (present(errmsg)) errmsg = 'compute: invalid input - x must be non-negative'
    return
  end if
  
  result = sqrt(x)
end subroutine
```

### 4. For Warnings (Level 0, -1)

**Modern pattern using module state**:
```fortran
module warnings
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  
  integer, parameter :: max_warnings = 100
  character(len=256) :: warned_messages(max_warnings) = ''
  integer :: n_warnings = 0
  
  public :: warn, warn_once
  
contains
  
  subroutine warn(message)
    character(len=*), intent(in) :: message
    write(error_unit, '(A)') 'WARNING: ' // message
  end subroutine
  
  subroutine warn_once(message)
    character(len=*), intent(in) :: message
    integer :: i
    
    ! Check if already warned
    do i = 1, n_warnings
      if (warned_messages(i) == message) return
    end do
    
    ! New warning
    call warn(message)
    if (n_warnings < max_warnings) then
      n_warnings = n_warnings + 1
      warned_messages(n_warnings) = message
    end if
  end subroutine
  
end module
```

## Migration Strategy by Function Type

### Category 1: Mathematical Functions

For functions that perform mathematical operations:

1. **Remove all XERMSG calls**
2. **Add optional status/error message parameters**
3. **Return sensible defaults for invalid input**
4. **Use PURE/ELEMENTAL where possible**

**Example transformation**:
```fortran
! Old F77
SUBROUTINE SOLVE(A, B, X, N)
  IF (N .LE. 0) THEN
    CALL XERMSG('SLATEC', 'SOLVE', 'N MUST BE POSITIVE', 1, 1)
    RETURN
  END IF
  ! ... computation
END

! Modern F90
pure subroutine solve(a, b, x, stat, errmsg)
  real, intent(in) :: a(:,:), b(:)
  real, intent(out) :: x(:)
  integer, intent(out), optional :: stat
  character(len=*), intent(out), optional :: errmsg
  
  if (present(stat)) stat = 0
  
  if (size(a,1) <= 0) then
    x = 0.0
    if (present(stat)) stat = 1
    if (present(errmsg)) errmsg = 'solve: matrix dimension must be positive'
    return
  end if
  ! ... computation
end subroutine
```

### Category 2: Utility Functions

For non-mathematical utilities:

1. **Machine constants**: Return compile-time constants
2. **String operations**: Use pure functions
3. **I/O operations**: Use modern I/O with iostat

### Category 3: Error System Functions

These functions should be **completely eliminated**:

- **J4SAVE**: No modern equivalent needed
- **NUMXER**: No modern equivalent needed
- **XGETUA**: Use `error_unit` from iso_fortran_env
- **XSETUA**: Not needed with modern I/O
- **XERHLT**: Use `error stop`
- **XERPRN**: Use standard write statements
- **XERSVE**: Use modern logging if needed
- **FDUMP**: Not needed

## Implementation Phases

### Phase 0: Foundation (7 functions)
Remove error handling from basic functions:
- PIMACH, AAAAAA, LSAME, FDUMP
- I1MACH, R1MACH, D1MACH (remove XERMSG calls)

### Phase 1: Simple Functions (50-100 functions)
Functions with 1-2 XERMSG calls:
- Add optional status parameters
- Return defaults for invalid input

### Phase 2: Complex Functions (200+ functions)
Functions with multiple error conditions:
- Careful analysis of error paths
- Preserve mathematical behavior
- Add comprehensive status reporting

### Phase 3: Stateful Functions (50+ functions)
Functions that use J4SAVE for configuration:
- Replace with module variables
- Add initialization routines

## Benefits of Migration

1. **Thread Safety**: No global state via SAVE
2. **Simplicity**: No complex error routing
3. **Performance**: No runtime error checking overhead
4. **Modern Integration**: Works with exception handling
5. **Clarity**: Error handling visible in interfaces

## Code Patterns

### Pattern 1: Simple Validation
```fortran
! Don't stop execution for bad input
if (n <= 0) then
  result = 0.0
  if (present(stat)) stat = INVALID_SIZE
  return
end if
```

### Pattern 2: Critical Errors
```fortran
! Use error stop for unrecoverable states
if (.not. allocated(work)) then
  error stop 'Internal error: work array not allocated'
end if
```

### Pattern 3: Optional Diagnostics
```fortran
! Provide detailed info when requested
if (present(info)) then
  info%iterations = iter
  info%residual = res
  info%converged = (res < tol)
end if
```

## Testing Strategy

1. **Behavioral Testing**: Ensure same mathematical results
2. **Error Path Testing**: Verify error conditions handled
3. **Performance Testing**: No regression in speed
4. **Thread Safety Testing**: Parallel execution works

## Conclusion

The SLATEC error handling system was sophisticated for its era but is incompatible with modern Fortran practices. By migrating to optional status parameters, error stop statements, and module-based configuration, we achieve a cleaner, safer, and more maintainable codebase while preserving all mathematical functionality.
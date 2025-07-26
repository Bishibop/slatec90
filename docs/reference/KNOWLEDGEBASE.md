# SLATEC Migration Knowledge Base

## Overview

This document contains general SLATEC knowledge and insights. For migration instructions, see MIGRATION_GUIDE.md.

## Test Suite Analysis

### Test Structure Overview
- **54 test drivers** (test01.f through test54.f) with systematic organization
- **Quick check philosophy**: Tests designed to catch gross errors, not exhaustive validation
- **Consistent pattern**: Each driver tests specific GAMS categories with pass/fail reporting
- **KPRINT parameter**: Controls verbosity (0=minimal, 3=complete output)

### Key Insights from SLATEC Resources

**From the guide:**
- SLATEC follows strict coding guidelines for portability across supercomputers
- Quick checks protect against "gross errors" and arithmetic difficulties
- Tests are minimal but consistent, with detailed output available on request

**Test driver patterns:**
- TEST05 tests Bessel functions (`BESI, BESK, BESJ, BESY`) and integration (`EXINT, GAUS8`)
- TEST01 tests a placeholder function (`AAAAAA`) - likely informational
- Standard error handling setup with `XERMAX`, `XSETUN`, `XSETF`

### TEST05 Structure (Bessel Functions & Integration)
```fortran
PROGRAM TEST05
! Tests: EXINT, GAUS8, BESI, BESK, BESJ, BESY
! Calls: EG8CK, BIKCK, BJYCK (quick check subroutines)
! Pattern: Standard SLATEC driver with KPRINT parameter
```

### Standard Driver Pattern
1. Read KPRINT parameter for verbosity control
2. Set up error handling (`XERMAX`, `XSETUN`, `XSETF`)
3. Call category-specific quick check routines
4. Track failures and report PASS/FAIL status

## Overall Statistics
- **Total functions**: ~1,150 functions analyzed
- **Zero dependencies**: 338 functions (29%) - immediate modernization candidates
- **Foundation dependencies only**: 171 functions depend only on machine constants and error handling
- **Complex functions**: 641 functions have 6+ dependencies

## Migration Approach Evolution

### Failed Approaches
1. **LLM-based code generation** - o3-mini kept adding extraneous markdown
2. **f2py wrapper approach** - Unnecessary complexity for our needs
3. **Function-specific exceptions** - User rejected this approach

### Successful Approach
- Direct F77 compilation with gfortran
- Batch processing to handle F77 program size limits
- JSON-based test data storage
- 100% test pass rate requirement (no exceptions)

## Resources Available

### Downloaded Files
- `slatec_chk.tgz` - 54 test drivers extracted to root directory
- `guide` - Official SLATEC documentation (2,768 lines, comprehensive guide)
- `tree` - Function dependency relationships (206KB)
- `slatec_src.tgz` - Original source (736 files, 290,907 lines in v4.1)

### Current Project State
- Successfully migrated: PYTHAG (194 tests), CDIV (335 tests)
- 167 zero-dependency functions remaining for migration
- Using direct F77 compilation for test generation
- 100% test pass rate required for all migrations

## Key Technical Insights

### SLATEC Design Philosophy
1. **Well-Structured Design**: No circular dependencies found - clean hierarchical structure
2. **Error Handling Ubiquity**: 70% of functions use XERMSG system
3. **Machine Constants Central**: R1MACH/I1MACH used by 60% of functions
4. **Mathematical Function Families**: Functions like BESI/DBESI/CBESI/ZBESI share patterns
5. **BLAS-style Operations**: Many Level 0 functions are basic linear algebra operations

### Validation Framework
Our approach aligns with SLATEC's philosophy:
- Use existing test drivers as baseline validation when available
- Focus on numerical correctness over exhaustive testing
- Maintain SLATEC's pass/fail reporting style
- Leverage the dependency tree for systematic migration ordering

## Deep Insights from Official SLATEC Guide

### Historical Context
- **Formation**: 1974 by Sandia, Los Alamos, and Air Force Weapons Laboratory
- **CML Started**: 1977 to provide portable software for supercomputers
- **Version History**:
  - v1.0 (1982): 491 user-callable routines
  - v2.0 (1984): 646 routines
  - v3.0 (1986): 704 routines
  - v4.0 (1992): 901 routines
  - v4.1 (1993): 902 routines (current)

### Machine Constants Philosophy
The I1MACH/R1MACH/D1MACH functions (from Bell Labs' PORT Library) don't just return hardware specs. They define a "safe subset" of floating point arithmetic where operations behave as expected. Key insight: *"Machine constants normally cannot be determined by reading a computer's hardware reference manual"* because manuals don't describe arithmetic unit errors.

### Error Handling Architecture
SLATEC's XERMSG system has sophisticated features:
- **Three severity levels**: Warning (returns), Recoverable (may return), Fatal (terminates)
- **Error memory**: System remembers last error number for user retrieval
- **Subsidiary rule**: Lower-level routines should return error flags, not call XERMSG
- **Message formatting**: Supports multi-line messages with '$$' as line separator
- **Character concatenation**: Can build specific error messages with numeric values

### Quick Check Testing Philosophy
From Section 10: *"The quick checks are not meant to provide an exhaustive test of the Library. Instead they are designed to protect against gross errors, such as an unsatisfied external."*

Key requirements:
1. Test a few successfully solved problems
2. Provide consistent "PASS"/"FAIL" output
3. Test some error conditions purposefully
4. Must execute correctly on any ANSI F77 system
5. No test should require skipping to avoid aborts

### Prologue Format Requirements
Every SLATEC routine must have a rigidly formatted prologue:
- Starts with `C***BEGIN PROLOGUE name`
- Contains sections like PURPOSE, CATEGORY, TYPE, KEYWORDS
- Self-contained documentation (no external docs)
- Machine-processable for automatic extraction

### Multiprocessing Considerations
Section 6.9 explains why COMMON blocks and SAVE are forbidden:
- Static allocation prevents simultaneous use by multiple processors
- Only acceptable for read-only DATA loaded constants
- Users must provide workspace via arguments for thread safety

### Code Submission Process
New routines underwent rigorous review:
- Author works with committee member champion
- Multiple meetings for discussion and revision
- Testing at all member sites
- Focus on usefulness, robustness, maintainability
- Must be freely distributable (public domain)

### GAMS Classification System
SLATEC adopted the NIST GAMS (Guide to Available Mathematical Software) classification:
- Hierarchical system (A through S, with sub-categories)
- Enables systematic organization of mathematical software
- Same system used by NIST for broader software collections
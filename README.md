# SLATEC F77 to Modern Fortran Migration

Systematic migration of the SLATEC mathematical library from FORTRAN 77 to modern Fortran using LLM-assisted generation and comprehensive validation.

## Overview

SLATEC (Sandia, Los Alamos, Air Force Weapons Laboratory Technical Exchange Committee) is a comprehensive FORTRAN 77 library containing mathematical and statistical routines. This project uses an automated pipeline to migrate functions to modern Fortran (F90+) while preserving numerical accuracy through exhaustive testing.

## Current Status

✅ **Completed**: 9 functions  
📊 **Generic Validator**: Operational  
🎯 **Available**: 729 functions ready for migration

## Prerequisites

- gfortran 8.0+ (or compatible Fortran compiler)
- Python 3.6+
- OpenAI API key (for LLM-based generation)
- Basic command line tools (make, git)

## Quick Start

1. **Set up environment**:
   ```bash
   cp .env.example .env
   # Add your OpenAI API key to .env
   ```

2. **Migrate a function**:
   ```bash
   python slatec_orchestrator.py --function FUNCNAME
   ```

3. **Check results** in:
   - `modern/funcname_module.f90` - Modernized code
   - `test_cases/funcname_tests.txt` - Generated test cases
   - `logs/` - Detailed analysis and results

All migrations require 100% validation pass rate.

## Project Structure

```
slatec_test/
├── src/                    # Original SLATEC F77 source files
├── modern/                 # Modern Fortran implementations
├── test_cases/             # Generated test cases
├── fortran_validator/      # Generic validation system
├── docs/                   # Documentation
│   ├── guides/            # Technical guides
│   ├── reference/         # Reference materials
│   └── archive/           # Historical documents
├── data/                   # Analysis data
├── logs/                   # Execution logs
└── journal/               # Development journal
```

## Key Components

- **`slatec_orchestrator.py`** - Main automation script
- **`modernizer.py`** - LLM-based F77→F90 converter
- **`test_generator.py`** - Comprehensive test generation
- **`fortran_validator/`** - Generic validation system
- **`MIGRATION_GUIDE.md`** - Complete migration guide

## Documentation

- **[Migration Guide](MIGRATION_GUIDE.md)** - Complete function list and status
- **[Modernization Guide](docs/guides/SLATEC_MODERNIZATION_GUIDE.md)** - F77→F90 patterns and examples
- **[Test Generation Guide](docs/guides/SLATEC_TEST_GENERATION_GUIDE.md)** - Test creation strategies
- **[Validation Guide](docs/guides/SLATEC_VALIDATION_GUIDE.md)** - Using the generic validator

## Completed Functions

| Function | Type | Description |
|----------|------|-------------|
| AAAAAA | Version | Returns SLATEC version string |
| CDIV | Complex | Complex division with overflow protection |
| D1MACH | Machine | Double precision machine constants |
| FDUMP | Debug | Error message dump |
| I1MACH | Machine | Integer machine constants |
| LSAME | Character | Case-insensitive character comparison |
| PIMACH | Constant | Returns value of π |
| PYTHAG | Math | Pythagorean sum sqrt(a²+b²) |
| R1MACH | Machine | Single precision machine constants |

## Original SLATEC Info

- **Version**: 4.1 (July 1993)
- **Source**: Public domain
- **Size**: 738 functions in this repository
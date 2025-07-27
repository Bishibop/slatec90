# SLATEC F77 to Modern Fortran Migration

Systematic migration of the SLATEC mathematical library from FORTRAN 77 to modern Fortran using LLM-assisted generation and comprehensive validation.

## Overview

SLATEC (Sandia, Los Alamos, Air Force Weapons Laboratory Technical Exchange Committee) is a comprehensive FORTRAN 77 library containing mathematical and statistical routines. This project uses an automated pipeline to migrate functions to modern Fortran (F90+) while preserving numerical accuracy through exhaustive testing.

## Current Status

✅ **Completed**: 13 functions  
📊 **Generic Validator**: Operational with array support  
🎯 **Available**: 725+ functions ready for migration  
🚀 **New**: Gemini 2.5 Flash integration for improved code generation  
🛡️ **New**: Automatic parameter validation catches test data issues

## Prerequisites

- gfortran 8.0+ (or compatible Fortran compiler)
- Python 3.6+
- LLM API key: OpenAI or Google Gemini (for code generation)
- Basic command line tools (make, git)

## Quick Start

1. **Set up environment**:
   ```bash
   cp .env.example .env
   # Add your API key to .env:
   # For OpenAI: OPENAI_API_KEY=sk-...
   # For Gemini: GEMINI_API_KEY=...
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

## Configuration

The project supports multiple LLM providers. Create `config.json`:

```json
{
  "llm_provider": "gemini",  // or "openai"
  "gemini_model": "gemini-2.5-flash",
  "openai_model": "o3-mini",
  "validate_parameters": true  // Enable automatic test parameter validation
}
```

Default: Uses OpenAI if no config.json exists. Parameter validation is enabled by default.

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
- **`test_generator.py`** - Comprehensive test generation with parameter validation
- **`test_parameter_validator.py`** - Automatic test parameter fixing
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
| CSROOT | Complex | Complex square root |
| D1MACH | Machine | Double precision machine constants |
| ENORM | Vector | Euclidean norm of a vector (array support) |
| FDUMP | Debug | Error message dump |
| I1MACH | Machine | Integer machine constants |
| LSAME | Character | Case-insensitive character comparison |
| PIMACH | Constant | Returns value of π |
| PYTHAG | Math | Pythagorean sum sqrt(a²+b²) |
| QWGTC | Quadrature | Cauchy principal value weight function |
| R1MACH | Machine | Single precision machine constants |
| ZABS | Complex | Complex absolute value |

## Original SLATEC Info

- **Version**: 4.1 (July 1993)
- **Source**: Public domain
- **Size**: 738 functions in this repository
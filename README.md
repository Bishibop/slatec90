# SLATEC F77 to Modern Fortran Migration

Automated migration of the SLATEC mathematical library from FORTRAN 77 to modern Fortran using LLM-powered code generation with OpenAI's o3-mini.

## Overview

SLATEC (Sandia, Los Alamos, Air Force Weapons Laboratory Technical Exchange Committee) is a comprehensive FORTRAN 77 library containing 736 mathematical and statistical routines (168,355 lines). This project automates the migration to modern Fortran (F2018) while preserving numerical accuracy.

## Current Status

✅ **Implemented**: Automated migration pipeline with o3-mini  
✅ **Working**: Test generation, error feedback, iterative refinement  
✅ **Proven**: Successfully migrated DENORM function with full validation  
🚧 **In Progress**: Scaling to handle all 736 functions  

## Features

- **Automated test generation** - LLM analyzes F77 code and generates comprehensive test cases
- **Reference validation** - Tests against original F77 implementations  
- **Iterative refinement** - Failed tests sent back to LLM with compilation/runtime errors
- **Error feedback** - Full stack traces and error messages for better fixes
- **Two-library approach** - Clean separation of F77 and modern code

## Quick Start

```bash
# Install dependencies
pip install openai python-dotenv

# Set OpenAI API key
echo "OPENAI_API_KEY=your-key-here" > .env

# Run migration on simple functions
python simple_migration_pipeline.py
```

## Project Structure

```
slatec_test/
├── src/                        # Original SLATEC F77 source (736 files)
├── modern/                     # Generated modern Fortran modules
├── logs/                       # Debug info for failed migrations
├── simple_migration_pipeline.py # Main migration script
├── llm_config.py              # OpenAI o3-mini configuration
├── CURRENT_STATE.md           # Detailed project status
└── claude_docs/               # Reference documentation
    ├── DEPENDENCY_MAP.md      # Function dependencies
    └── guides/                # Implementation guides
```

## How It Works

1. **Select function** - Start with simple functions (no dependencies)
2. **Generate tests** - o3-mini creates test cases analyzing the F77 code
3. **Get reference values** - Execute original F77 to get expected outputs
4. **Generate modern code** - o3-mini converts to modern Fortran with proper structure
5. **Test & refine** - Run tests, send failures back with error details
6. **Iterate** - Up to 5 attempts to fix all test failures

## Example Migration

Successfully migrated `DENORM` (Euclidean norm):
```fortran
! Original F77: 116 lines with GOTO statements
REAL FUNCTION DENORM(N,X)
      DIMENSION X(N)
      DATA ZERO,ONE /0.0D0,1.0D0/
      ...
      GO TO 70
      
! Modern: Module with explicit interfaces
module denorm_module
  use iso_fortran_env, only: real64
  implicit none
contains
  pure function denorm(n, x) result(norm)
    integer, intent(in) :: n
    real(real64), intent(in) :: x(:)
    real(real64) :: norm
    ! Three-sum algorithm preserved
```

- Preserved exact numerical algorithm
- Module-based organization
- Pure functions with intent declarations
- F77-compatible wrapper included

## Key Documentation

- **`CURRENT_STATE.md`** - Detailed implementation status
- **`MIGRATION_QUICKSTART.md`** - Quick start guide
- **`claude_docs/DEPENDENCY_MAP.md`** - Function dependency graph
- **`claude_docs/SLATEC_MIGRATION_PLAN.md`** - Original comprehensive plan

## Technical Details

- **LLM**: OpenAI o3-mini (reasoning model, good for code)
- **Compiler**: gfortran with -std=f2018 and -std=legacy
- **Validation**: Bit-for-bit comparison with F77 results
- **Cost**: ~$0.001-0.005 per function with o3-mini

## Next Steps

1. Fix edge cases in pipeline (markdown stripping, error handling)
2. Implement batch processing for efficiency
3. Handle precision variants (s/d/c/z) systematically
4. Add dependency resolution for complex functions
5. Create comprehensive test suite

## Original SLATEC Info

- **Version**: 4.1 (July 1993)
- **Size**: 736 files, 168k lines
- **Categories**: 10 GAMS classifications
- **License**: Public domain

For complete technical details and history, see `claude_docs/` directory.
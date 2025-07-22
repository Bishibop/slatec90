# SLATEC F77 to Modern Fortran Migration

Manual migration of the SLATEC mathematical library from FORTRAN 77 to modern Fortran using comprehensive test-driven validation.

## Overview

SLATEC (Sandia, Los Alamos, Air Force Weapons Laboratory Technical Exchange Committee) is a comprehensive FORTRAN 77 library containing 736 mathematical and statistical routines (168,355 lines). This project migrates functions to modern Fortran (F90+) while preserving numerical accuracy through exhaustive testing.

## Current Status

✅ **Completed**: 2 of 169 zero-dependency functions  
📊 **In Progress**: 0  
🎯 **Available**: 167 (see MIGRATION_GUIDE.md for full list and status)

## Prerequisites

- gfortran (or compatible Fortran compiler)
- Python 3.6+
- Basic command line tools (make, git)

## Quick Start

1. **Choose a function** from the available list in MIGRATION_GUIDE.md
2. **Generate test cases**:
   ```bash
   python slatec_test_helper.py generate FUNCNAME
   ```
3. **Implement modern version** in `modern/funcname_modern.f90`
4. **Validate implementation**:
   ```bash
   python slatec_test_helper.py validate FUNCNAME
   ```

All migrations require 100% test pass rate. See **MIGRATION_GUIDE.md** for detailed instructions.

## Project Structure

```
slatec_test/
├── src/                    # Original SLATEC F77 source (736 files)
├── modern/                 # Modern Fortran implementations
│   ├── pythag_modern.f90   # Completed migration
│   └── cdiv_modern.f90     # Completed migration
├── test_data/              # Validated test cases with reference values
│   ├── pythag_tests.json   # 194 test cases
│   └── cdiv_tests.json     # 335 test cases
├── tree                    # Function dependency tree
└── MIGRATION_GUIDE.md      # Comprehensive migration instructions and status
```


## Key Files

- **`MIGRATION_GUIDE.md`** - Complete migration instructions, strategies, and function list
- **`slatec_test_helper.py`** - Required script for test generation and validation
- **`tree`** - Function dependency relationships
- **`KNOWLEDGEBASE.md`** - General SLATEC knowledge and insights

## Contributing

1. Check MIGRATION_GUIDE.md for available functions
2. Update the "In Progress" table before starting work
3. Follow the test-driven migration process
4. Ensure 100% test pass rate before marking complete
5. Commit with descriptive message following the format in MIGRATION_GUIDE.md

## Original SLATEC Info

- **Version**: 4.1 (July 1993)
- **Size**: 736 files, 168,355 lines
- **License**: Public domain
# SLATEC F77 to Modern Fortran Migration

Manual migration of the SLATEC mathematical library from FORTRAN 77 to modern Fortran using comprehensive test-driven validation.

## Overview

SLATEC (Sandia, Los Alamos, Air Force Weapons Laboratory Technical Exchange Committee) is a comprehensive FORTRAN 77 library containing 736 mathematical and statistical routines (168,355 lines). This project migrates functions to modern Fortran (F90+) while preserving numerical accuracy through exhaustive testing.

### Important Note: SLATEC Subset

This repository contains **738 of the 1,441 functions** from the complete SLATEC 4.1 library. The missing ~700 functions are primarily from the **Fullerton Special Function Library (FNLIB)**, which includes:

- Elementary functions (ACOSH, ASINH, ATANH, etc.)
- Bessel functions (BESI0, BESJ0, BESK0, BESY0, etc.)
- Airy functions (AI, BI, AIE, BIE)
- Error and exponential integrals (ERF, ERFC, E1, EI)
- Complex and double precision variants

The `tree` file shows dependencies for the complete SLATEC catalog, while migration efforts focus on the 738 functions available in `src/`. This explains why only 209 zero-dependency functions are available for migration instead of the 338 shown in the complete dependency tree.

## Current Status

✅ **Completed**: 9 of 209 zero-dependency functions  
📊 **In Progress**: 0  
🎯 **Available**: 200 (see COMPREHENSIVE_MIGRATION_GUIDE.md for complete list and strategy)

## Prerequisites

- gfortran (or compatible Fortran compiler)
- Python 3.6+
- Basic command line tools (make, git)

## Quick Start

1. **Choose a function** from the available list in COMPREHENSIVE_MIGRATION_GUIDE.md
2. **Generate test cases**:
   ```bash
   python slatec_test_helper.py generate FUNCNAME
   ```
3. **Implement modern version** in `modern/funcname_modern.f90`
4. **Validate implementation**:
   ```bash
   python slatec_test_helper.py validate FUNCNAME
   ```

All migrations require 100% test pass rate. See **COMPREHENSIVE_MIGRATION_GUIDE.md** for complete function inventory and strategic migration plan.

## Project Structure

```
slatec_test/
├── src/                    # Original SLATEC F77 source (738 files - subset of complete library)
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

- **`COMPREHENSIVE_MIGRATION_GUIDE.md`** - Complete function inventory and strategic migration plan for all 738 available functions
- **`MIGRATION_GUIDE.md`** - Detailed migration instructions and methodologies
- **`slatec_test_helper.py`** - Required script for test generation and validation
- **`tree`** - Function dependency relationships (complete SLATEC 1,441 function catalog)
- **`KNOWLEDGEBASE.md`** - General SLATEC knowledge and insights

## Contributing

1. Check COMPREHENSIVE_MIGRATION_GUIDE.md for available functions and strategic priorities
2. Follow the test-driven migration process  
3. Ensure 100% test pass rate before marking complete
4. Update migration status in both guides when complete
5. Commit with descriptive message following conventional commit format

## Original SLATEC Info

- **Version**: 4.1 (July 1993)
- **Complete Library Size**: 1,441 functions total
- **This Repository**: 738 files, 168,355 lines (subset excluding Fullerton FNLIB)
- **License**: Public domain
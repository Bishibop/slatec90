# SLATEC Dependency Analysis Tools

This directory contains tools for parsing and analyzing the SLATEC function dependency tree to help prioritize migration efforts.

## Files Created

### Core Analysis Scripts

1. **`parse_tree.py`** - Main parser that extracts dependency information from the `tree` file
2. **`generate_dot.py`** - Generates GraphViz DOT files for visualizing dependencies
3. **`simple_insights.py`** - Generates quick insights and migration recommendations
4. **`dependency_insights.py`** - Advanced insights (may be slow for large datasets)

### Generated Data Files

1. **`dependency_analysis.json`** - Complete dependency data structure in JSON format
2. **`*.dot`** - GraphViz files for visualization

## Usage

### 1. Parse the dependency tree

```bash
python3 parse_tree.py
```

This creates `dependency_analysis.json` with the complete analysis.

### 2. Generate visualization files

```bash
# Generate migration candidates and zero-dependency graphs
python3 generate_dot.py --candidates --zero-deps

# Generate full dependency graph (warning: may be very large)
python3 generate_dot.py --full

# Generate dependency subgraph for a specific function
python3 generate_dot.py --function PYTHAG

# Generate all standard graphs
python3 generate_dot.py --all
```

### 3. Get insights and recommendations

```bash
python3 simple_insights.py
```

### 4. Visualize DOT files

```bash
# Convert to PNG
dot -Tpng migration_candidates.dot -o migration_candidates.png

# Convert to SVG (better for large graphs)
dot -Tsvg zero_dependencies.dot -o zero_dependencies.svg

# Interactive viewing with xdot (if available)
xdot migration_candidates.dot
```

## Data Structure

The main data structure in `dependency_analysis.json` contains:

```json
{
  "metadata": {
    "statistics": {
      "total_functions": 1441,
      "functions_with_zero_deps": 338,
      "available_functions": 738,
      "completed_functions": 9,
      "zero_deps_available": 168,
      "zero_deps_completed": 7
    }
  },
  "functions": {
    "all_functions": ["AAAAAA", "ACOSH", ...],
    "zero_dependency_functions": ["AAAAAA", "CDIV", ...],
    "available_functions": ["AAAAAA", "BCRH", ...]
  },
  "dependencies": {
    "FUNCTION_NAME": ["DEP1", "DEP2", ...]
  },
  "status": {
    "FUNCTION_NAME": "completed|in_progress|available|not_available"
  },
  "analysis": {
    "zero_deps_available": [...],
    "zero_deps_completed": [...],
    "functions_by_status": {...}
  }
}
```

## Key Insights from Analysis

### Current Status (as of analysis)
- **Total Functions**: 1,441 in the complete SLATEC tree
- **Available in src/**: 738 functions (51.2%)
- **Zero Dependencies**: 338 functions total, 168 available in src/
- **Completed Migrations**: 9 functions
  - CDIV, D1MACH, DENORM, ENORM, I1MACH, LSAME, PYTHAG, R1MACH, ZABS

### High Priority Targets

**Most Depended Upon Functions** (available for migration):
1. **FDUMP** - Used by 583 other functions (error/debug output)
2. **J4SAVE** - Used by 461 other functions (error system state)
3. **XERCNT** - Used by 199 other functions (error counting)
4. **XERBLA** - Used by 70 other functions (error reporting)
5. **XERHLT** - Used by 66 other functions (error halting)

**System/Infrastructure Functions Available**:
- FDUMP, J4SAVE, MPERR, MPSTR, PIMACH
- These are critical for error handling and system operations

### Migration Strategy Recommendations

1. **Phase 1 - Infrastructure**: Complete error handling system
   - FDUMP, J4SAVE, XERCNT, XERBLA, XERHLT
   - These unlock many other functions

2. **Phase 2 - Mathematical Utilities**: 
   - Focus on highly-used mathematical functions
   - Consider functions with short names (often simpler)

3. **Phase 3 - Specialized Functions**:
   - Domain-specific functions after infrastructure is complete

## Command Line Examples

```bash
# Quick analysis
python3 simple_insights.py

# Generate migration priority visualization
python3 generate_dot.py --candidates
dot -Tpng migration_candidates.dot -o migration_candidates.png

# Analyze specific function dependencies
python3 generate_dot.py --function FDUMP --max-depth 3
dot -Tsvg fdump_dependencies.dot -o fdump_dependencies.svg

# Full dependency analysis (advanced)
python3 dependency_insights.py
```

## Notes

- The analysis is based on the current state of completed migrations
- Function status is determined by:
  - `completed`: Known to be migrated (PYTHAG, CDIV, etc.)
  - `available`: Present in src/ directory but not yet migrated
  - `not_available`: Not present in src/ directory
- Dependency counts help prioritize high-impact migrations
- Zero-dependency functions are the safest starting points for migration
# SLATEC Function Complexity Categorization

## Summary

Analysis of 736 SLATEC functions (out of 738 total) categorized by complexity:

| Level | Name | Count | Percentage | Description |
|-------|------|--------|------------|-------------|
| 0 | Trivial | 33 | 4.5% | < 5 parameters, no arrays or simple arrays, no EXTERNAL/COMMON/SAVE, pure mathematical functions |
| 1 | Simple | 63 | 8.6% | < 10 parameters, simple arrays, may have SAVE but simple state, no EXTERNAL parameters |
| 2 | Moderate | 415 | 56.4% | Work arrays with formulas, may have COMMON blocks, iterative algorithms, no EXTERNAL parameters |
| 3 | Complex | 223 | 30.3% | EXTERNAL parameters, complex work array sizing, multiple algorithm branches |
| 4 | Stateful | 4 | 0.5% | INDEX pattern (INDEX=1/2), requires sequential calls |

## Key Findings

1. **Majority are Moderate Complexity (56.4%)**: Most SLATEC functions fall into the moderate category, featuring work arrays and iterative algorithms but without the complexity of EXTERNAL parameters.

2. **Significant Complex Functions (30.3%)**: Nearly a third of functions are complex, primarily due to EXTERNAL parameters (194 functions, 28.1% of total).

3. **Few Truly Trivial Functions (4.5%)**: Only 33 functions are truly trivial with minimal parameters and no state.

4. **Rare Stateful Functions (0.5%)**: Only 4 functions use the INDEX pattern requiring sequential calls.

## Feature Statistics

- **EXTERNAL parameters**: 194 functions (26.4%)
- **COMMON blocks**: 110 functions (14.9%)
- **SAVE statements**: 164 functions (22.3%)
- **Arrays**: 627 functions (85.2%)
- **Work arrays**: 98 functions (13.3%)
- **INDEX pattern**: 4 functions (0.5%)

## Examples by Level

### Level 0 - Trivial (33 functions)
Examples of pure mathematical functions with minimal complexity:
- **PIMACH**: Pi machine constant (1 param)
- **FDUMP**: Symbolic dump utility (0 params)
- **CSROOT**: Complex square root (4 params)
- **CSHCH**: Hyperbolic functions (3 params)
- **NUMXER**: Error count retrieval (1 param)

### Level 1 - Simple (63 functions)
Functions with simple arrays and moderate parameter counts:
- **CDIV**: Complex division (6 params)
- **PYTHAG**: Pythagorean distance (2 params)
- **CAIRY**: Airy function Ai(z) (6 params)
- **CBESI**: Bessel I sequence (7 params)
- **DGAUS8**: Gaussian quadrature (6 params, SAVE)

### Level 2 - Moderate (415 functions)
Functions with work arrays and iterative algorithms:
- **BSPLVD**: B-spline derivatives (6 params)
- **DGESV**: Linear system solver (8 params)
- **AVINT**: Arbitrary interval integration (7 params)
- **BSPEV**: B-spline evaluation (9 params, WORK arrays)
- **BNDSOL**: Banded matrix solver (9 params)

### Level 3 - Complex (223 functions)
Functions with EXTERNAL parameters or complex array sizing:
- **BESJ**: J Bessel functions (5 params, EXTERNAL, SAVE)
- **BFQAD**: Function product integration (12 params, EXTERNAL, WORK)
- **DDRIV3**: ODE solver (29 params, EXTERNAL, WORK)
- **BVSUP**: Boundary value solver (22 params, COMMON, WORK)
- **DNSQE**: Nonlinear system solver (EXTERNAL, WORK arrays)

### Level 4 - Stateful (4 functions)
Functions requiring sequential calls with INDEX parameter:
- **BSPLVN**: B-spline basis evaluation (6 params, SAVE, INDEX pattern)
- **BSPVN**: B-spline basis values (9 params, WORK, INDEX pattern)
- **DBSPVN**: Double precision B-spline basis (9 params, WORK, INDEX pattern)
- **DFSPVN**: Double precision subsidiary (6 params, SAVE, INDEX pattern)

## Migration Implications

1. **Level 0-1 (13.1%)**: Can be migrated with minimal changes, mostly direct translations.

2. **Level 2 (56.4%)**: Require careful handling of work arrays and may benefit from modern Fortran's automatic arrays.

3. **Level 3 (30.3%)**: Need significant redesign to replace EXTERNAL parameters with modern interfaces or abstract interfaces.

4. **Level 4 (0.5%)**: Require complete redesign to eliminate stateful INDEX pattern, possibly using derived types to maintain state.

## Recommendations

1. **Start with Level 0-1**: These 96 functions (13.1%) provide quick wins for migration.

2. **Prioritize High-Use Level 2**: Focus on frequently-used moderate complexity functions.

3. **Design Patterns for Level 3**: Develop standard patterns for replacing EXTERNAL parameters.

4. **Special Handling for Level 4**: The 4 stateful functions need complete architectural redesign.

## Note

2 functions could not be analyzed (likely BLOCK DATA or special format). The analysis covers 736 out of 738 total SLATEC functions (99.7%).
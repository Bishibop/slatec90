# Skipped Functions

This document tracks SLATEC functions that are skipped during modernization due to various reasons.

## I/O Functions (File/Console Output)

These functions are skipped because they perform I/O operations that are difficult to validate automatically:

### Vector Output Functions
- **SVOUT** - Single precision vector output to console/file
  - Reason: Writes to output unit from I1MACH(2)
  - Used by: sbolsm, splpmn (for debug output when IPRINT > 0)
  
- **DVOUT** - Double precision vector output to console/file  
  - Reason: Writes to output unit from I1MACH(2)
  - Used by: dbolsm, dplpmn (for debug output when IPRINT > 0)

### Functions Dependent on I/O
These large optimization routines are skipped because they depend on SVOUT/DVOUT:

- **sbolsm** (1185 lines) - Bounded variable least squares solver
  - Depends on: SVOUT (for optional debug output)
  
- **splpmn** (988 lines) - Simplex algorithm for linear programming
  - Depends on: SVOUT (for optional debug output)
  
- **dbolsm** (1188 lines) - Double precision bounded variable least squares
  - Depends on: DVOUT (for optional debug output)
  
- **dplpmn** (988 lines) - Double precision simplex algorithm
  - Depends on: DVOUT (for optional debug output)

## Notes

- The I/O functions could potentially be modernized with special handling:
  - Mock the WRITE statements to capture output
  - Redirect output to strings/buffers for comparison
  - Create a special validation mode for I/O functions
  
- The optimization routines (sbolsm, splpmn, etc.) only use the output functions for optional debugging, so they could potentially be modernized by:
  - Commenting out the debug output calls
  - Replacing with modern logging mechanisms
  - Making the output functions optional dependencies
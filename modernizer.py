"""
LLM-based Fortran modernizer with iterative refinement
"""
import json
import logging
import os
from typing import Dict, List
from openai import OpenAI

class LLMModernizer:
    def __init__(self, config):
        self.config = config
        self.logger = logging.getLogger('LLMModernizer')
        api_key = config.get('openai_api_key') or os.getenv('OPENAI_API_KEY')
        if not api_key:
            raise ValueError("OpenAI API key not found in config or environment")
        self.client = OpenAI(api_key=api_key)
        
    def modernize(self, func_name, f77_code, test_cases):
        """Initial modernization of F77 to F90"""
        prompt = self._create_modernization_prompt(func_name, f77_code, test_cases)
        
        try:
            response = self.client.chat.completions.create(
                model=self.config.get('llm_model', 'o3-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"}
            )
            
            result = json.loads(response.choices[0].message.content)
            self.logger.info(f"Modernization complete for {func_name}")
            return result
            
        except Exception as e:
            self.logger.error(f"Modernization failed for {func_name}: {e}")
            raise
            
    def refine(self, func_name, current_code, validation_errors):
        """Refine based on validation errors"""
        prompt = f"""You are fixing validation errors in a modernized Fortran function.

Function: {func_name}

Current modernized code that has errors:
```fortran
{current_code}
```

Validation errors to fix:
{json.dumps(validation_errors, indent=2)}

Please analyze the errors and provide a corrected version. Common issues:
- Module name mismatches (should be {func_name.lower()}_module)
- Interface differences between F77 and F90
- Precision mismatches (use ISO_FORTRAN_ENV kinds)
- Missing PURE/ELEMENTAL attributes
- Incorrect INTENT specifications
- Array parameter declarations (use assumed-size (*) to match F77)
- Missing implicit none statements
- Incorrect handling of dependencies

Respond with a JSON object containing:
{{
    "name": "{func_name}",
    "error_analysis": "Your understanding of what went wrong",
    "fixes_applied": "Specific changes made to fix the errors",
    "f90_code": "The complete corrected Fortran 90/95 code"
}}"""

        try:
            response = self.client.chat.completions.create(
                model=self.config.get('llm_model', 'o3-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"}
            )
            
            result = json.loads(response.choices[0].message.content)
            self.logger.info(f"Refinement complete for {func_name}")
            return result
            
        except Exception as e:
            self.logger.error(f"Refinement failed for {func_name}: {e}")
            raise
            
    def fix_compilation(self, func_name, current_code, compilation_errors):
        """Fix compilation errors"""
        prompt = f"""Fix compilation errors in this modernized Fortran code.

Function: {func_name}

Code with compilation errors:
```fortran
{current_code}
```

Compilation errors:
{compilation_errors}

Common issues:
- Missing USE statements for dependencies
- Incorrect module/function names (module should be {func_name.lower()}_module)
- Type mismatches between F77 and F90
- Missing IMPLICIT NONE in module or procedures
- Incorrect or missing INTENT specifications
- Array declarations: use assumed-size (*) not assumed-shape (:)
- For dependencies: use module_name, only: function_name
- Character arguments: use CHARACTER(*) for assumed length

IMPORTANT: The module must contain ONLY the {func_name} function. Do not include any other functions.

Respond with JSON containing the corrected f90_code."""

        try:
            response = self.client.chat.completions.create(
                model=self.config.get('llm_model', 'o3-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"}
            )
            
            result = json.loads(response.choices[0].message.content)
            return result
            
        except Exception as e:
            self.logger.error(f"Compilation fix failed for {func_name}: {e}")
            raise
            
    def _create_modernization_prompt(self, func_name, f77_code, test_cases):
        """Create the modernization prompt"""
        return f"""You are a Fortran expert modernizing a SLATEC function from F77 to modern F90/95.

IMPORTANT: Modernize ONLY the {func_name} function. The module must contain ONLY this single function.

Function to modernize: {func_name}

Original Fortran 77 source:
```fortran
{f77_code}
```

Test cases (for understanding usage):
{test_cases}

Modernization rules:

1. **Module Structure**:
   - Create a module named `{func_name.lower()}_module`
   - Use `implicit none` in the module
   - Make the function PUBLIC: `public :: {func_name.lower()}`
   - Everything else should be PRIVATE
   - One function per module - do NOT include any other functions

2. **General Modernization**:
   - Add INTENT(IN), INTENT(OUT), or INTENT(INOUT) for all arguments
   - Replace GOTO with DO loops or IF-THEN-ELSE constructs
   - Use PURE or ELEMENTAL where appropriate (functions with no side effects)
   - Preserve the exact mathematical algorithm and numerical behavior
   - Keep the same interface type (FUNCTION vs SUBROUTINE)

3. **Type Modernization**:
   - Use ISO_FORTRAN_ENV for precision: REAL32, REAL64, INT32, etc.
   - For REAL without explicit precision, keep as default REAL
   - For DOUBLE PRECISION, use REAL(REAL64)
   - For INTEGER, keep as default INTEGER unless precision matters

4. **Error Handling Philosophy (CRITICAL)**:
   - Remove ALL calls to XERMSG, J4SAVE, or any error handling routines
   - J4SAVE maintains global error state - completely eliminate it
   - For invalid inputs, return sensible defaults:
     * Machine constants: return IEEE standard values even for invalid indices
     * Mathematical functions: return 0.0 or mathematically sensible defaults
     * Logical functions: return .FALSE.
   - NO error stops - let the caller handle invalid results
   - NO global variables or SAVE statements - ensure thread safety
   - NO hidden state - each function call must be independent
   - Replace any XGETUA/XSETUA (error output routing) with removal

5. **Dependencies**:
   - If the function calls other SLATEC functions, assume they exist as modules
   - Use: `use other_module, only: other_function`
   - Do NOT implement stub versions of dependencies

6. **Machine Constants Special Handling**:
   - For I1MACH, R1MACH, D1MACH: use Fortran intrinsics
   - Replace platform-specific DATA statements with:
     * huge(), tiny(), epsilon(), digits(), radix()
   - Return sensible defaults for invalid indices (not error stop)
   - Example: R1MACH(2) = HUGE(1.0), R1MACH(1) = TINY(1.0)

7. **Critical Requirements**:
   - Mathematical correctness is the top priority
   - Preserve all numerical stability techniques from the original
   - Thread-safe: NO global state, NO SAVE statements, NO common blocks
   - Do NOT add functions that weren't in the original F77 code
   - Do NOT create implementations for other functions
   - Each function must be completely independent and reentrant

Respond with JSON:
{{
    "name": "{func_name}",
    "description": "Brief description of what this function computes",
    "algorithm_analysis": "Your understanding of the mathematical algorithm used",
    "modernization_notes": "Key changes made during modernization",
    "f90_code": "Complete Fortran 90/95 module code containing ONLY the {func_name} function"
}}"""
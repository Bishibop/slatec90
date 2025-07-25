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
- Module name mismatches
- Interface differences between F77 and F90
- Precision mismatches
- Missing PURE/ELEMENTAL attributes
- Dependency module names (e.g., use pythag_module not pythag)
- Array parameter declarations (assumed-size vs assumed-shape)
- Complex arithmetic interface preservation
- Format string handling in output functions

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
- Missing USE statements
- Incorrect module/function names
- Type mismatches
- Missing IMPLICIT NONE
- Incorrect INTENT specifications
- For dependencies: use pythag_module, only: pythag
- Array declarations: real, intent(in) :: array(*) not array(:)
- Complex functions: separate real/imaginary components
- Output functions: character(*) for format strings

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

Function: {func_name}

Original Fortran 77 source:
```fortran
{f77_code}
```

Test cases (for understanding usage):
{test_cases}

Modernize following these rules:
1. Create a module named {func_name.lower()}_module
2. Use 'implicit none' always
3. Add INTENT for all arguments
4. Replace GOTO with structured constructs
5. For machine constants (I1MACH, R1MACH, D1MACH):
   - Use intrinsic functions from ISO_FORTRAN_ENV
   - Return 0 for invalid indices (no error stops)
   - R1MACH: Use REAL(REAL32) for all values
   - D1MACH: Use REAL(REAL64) for all values
6. Remove ALL XERMSG calls - return defaults for errors
7. Use PURE/ELEMENTAL where appropriate
8. Preserve exact mathematical behavior
9. Keep same interface (function vs subroutine)
10. For LSAME: Handle the SAVE statement carefully
11. Module structure:
    - PUBLIC only the main function
    - PRIVATE for any module variables or helper procedures
    - Use modern parameter declarations
12. For complex arithmetic functions (CDIV, CSROOT):
    - Keep separate real/imaginary components if that's the interface
    - Don't force COMPLEX type unless beneficial
    - Preserve numerical stability techniques
13. For array output functions (SVOUT, DVOUT, IVOUT):
    - Use assumed-size arrays to match F77 interface
    - Preserve formatting behavior exactly
    - Handle variable format strings properly
14. For functions with dependencies:
    - USE the modernized module (e.g., use pythag_module)
    - Import with rename if needed: use pythag_module, only: pythag
15. Special patterns:
    - PYTHAG: Keep the iterative algorithm for overflow protection
    - CSROOT: Preserve branch cut behavior
    - Output functions: Keep I1MACH(2) for output unit

Respond with JSON:
{{
    "name": "{func_name}",
    "description": "What this function does",
    "algorithm_analysis": "Understanding of the algorithm",
    "modernization_notes": "Key changes made",
    "f90_code": "Complete module code"
}}"""
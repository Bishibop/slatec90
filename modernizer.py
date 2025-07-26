"""
Unified LLM-based Fortran modernizer supporting multiple providers
"""
import json
import logging
import os
from typing import Dict, List, Optional
from abc import ABC, abstractmethod

# Provider imports
from openai import OpenAI
from google import genai

class BaseLLMProvider(ABC):
    """Abstract base class for LLM providers"""
    
    @abstractmethod
    def generate_completion(self, prompt: str, response_format: Optional[str] = None) -> Dict:
        """Generate a completion from the LLM"""
        pass

class OpenAIProvider(BaseLLMProvider):
    """OpenAI provider implementation"""
    
    def __init__(self, config):
        api_key = config.get('openai_api_key') or os.getenv('OPENAI_API_KEY')
        if not api_key:
            raise ValueError("OpenAI API key not found in config or environment")
        self.client = OpenAI(api_key=api_key)
        self.model = config.get('openai_model', 'o3-mini')
        
    def generate_completion(self, prompt: str, response_format: Optional[str] = None) -> Dict:
        kwargs = {
            "model": self.model,
            "messages": [{"role": "user", "content": prompt}]
        }
        
        if response_format == "json":
            kwargs["response_format"] = {"type": "json_object"}
            
        response = self.client.chat.completions.create(**kwargs)
        content = response.choices[0].message.content
        
        if response_format == "json":
            return json.loads(content)
        return {"text": content}

class GeminiProvider(BaseLLMProvider):
    """Google Gemini provider implementation"""
    
    def __init__(self, config):
        # Set API key from config or environment
        api_key = config.get('gemini_api_key') or os.getenv('GEMINI_API_KEY')
        if api_key:
            os.environ['GEMINI_API_KEY'] = api_key
            
        self.client = genai.Client()
        self.model = config.get('gemini_model', 'gemini-2.5-flash')
        
    def generate_completion(self, prompt: str, response_format: Optional[str] = None) -> Dict:
        # Prepare the prompt
        if response_format == "json":
            prompt = f"{prompt}\n\nIMPORTANT: Respond with valid JSON only, no additional text."
            
        # Generate response
        response = self.client.models.generate_content(
            model=self.model,
            contents=prompt,
            # Configuration options can be passed directly
            config={
                "temperature": 0.3,  # Lower temperature for more consistent code generation
                "top_p": 0.8,
                "max_output_tokens": 8192,
            }
        )
        
        content = response.text
        
        if response_format == "json":
            # Clean up potential markdown formatting
            if content.startswith("```json"):
                content = content[7:]
            if content.endswith("```"):
                content = content[:-3]
            return json.loads(content.strip())
        
        return {"text": content}

class UnifiedModernizer:
    """Modernizer that supports multiple LLM providers"""
    
    def __init__(self, config):
        self.config = config
        self.logger = logging.getLogger('UnifiedModernizer')
        
        # Select provider based on configuration
        provider = config.get('llm_provider', 'openai').lower()
        
        if provider == 'openai':
            self.provider = OpenAIProvider(config)
            self.logger.info(f"Using OpenAI provider with model: {config.get('openai_model', 'o3-mini')}")
        elif provider == 'gemini':
            self.provider = GeminiProvider(config)
            self.logger.info(f"Using Gemini provider with model: {config.get('gemini_model', 'gemini-2.5-flash')}")
        else:
            raise ValueError(f"Unknown LLM provider: {provider}")
            
    def modernize(self, func_name, f77_code, test_cases):
        """Initial modernization of F77 to F90"""
        prompt = self._create_modernization_prompt(func_name, f77_code, test_cases)
        
        try:
            result = self.provider.generate_completion(prompt, response_format="json")
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

Common issues and fixes:
1. Module structure errors:
   - Function/subroutine definitions must be INSIDE the contains section
   - The module itself cannot have "pure" - only the function/subroutine can
   
2. Declaration issues:
   - Missing USE statements for dependencies
   - Incorrect module/function names (module should be {func_name.lower()}_module)
   - Type mismatches between F77 and F90
   - Missing IMPLICIT NONE in module or procedures
   - Incorrect or missing INTENT specifications
   
3. Array vs scalar confusion:
   - If F77 doesn't have DIMENSION, the parameter is a SCALAR
   - Only use array syntax (*) if F77 explicitly has DIMENSION
   - Array declarations: use assumed-size (*) not assumed-shape (:)
   
4. Other issues:
   - For dependencies: use module_name, only: function_name
   - Character arguments: use CHARACTER(*) for assumed length
   - ELEMENTAL functions cannot have array arguments with (*)

IMPORTANT: The module must contain ONLY the {func_name} function. Do not include any other functions.

Respond with a JSON object containing:
{{
    "name": "{func_name}",
    "error_analysis": "Your understanding of what went wrong",
    "fixes_applied": "Specific changes made to fix the errors",
    "f90_code": "The complete corrected Fortran 90/95 code"
}}"""

        try:
            result = self.provider.generate_completion(prompt, response_format="json")
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

Common issues and fixes:
1. Module structure errors:
   - Function/subroutine definitions must be INSIDE the contains section
   - The module itself cannot have "pure" - only the function/subroutine can
   
2. Declaration issues:
   - Missing USE statements for dependencies
   - Incorrect module/function names (module should be {func_name.lower()}_module)
   - Type mismatches between F77 and F90
   - Missing IMPLICIT NONE in module or procedures
   - Incorrect or missing INTENT specifications
   
3. Array vs scalar confusion:
   - If F77 doesn't have DIMENSION, the parameter is a SCALAR
   - Only use array syntax (*) if F77 explicitly has DIMENSION
   - Array declarations: use assumed-size (*) not assumed-shape (:)
   
4. Other issues:
   - For dependencies: use module_name, only: function_name
   - Character arguments: use CHARACTER(*) for assumed length
   - ELEMENTAL functions cannot have array arguments with (*)

IMPORTANT: The module must contain ONLY the {func_name} function. Do not include any other functions.

Respond with JSON containing the corrected f90_code."""

        try:
            result = self.provider.generate_completion(prompt, response_format="json")
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
   - IMPORTANT: Do NOT assume parameters are arrays unless explicitly declared as DIMENSION in F77
   - Scalar parameters should remain scalars - only use array syntax (*) if F77 has DIMENSION

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

8. **Example Module Structure**:
```fortran
module function_name_module
  use iso_fortran_env, only: real32  ! or real64 as needed
  implicit none
  private
  public :: function_name

contains

  pure function function_name(arg1, arg2) result(res)
    real(real32), intent(in) :: arg1, arg2  ! Scalars unless F77 has DIMENSION
    real(real32) :: res
    ! Implementation here
  end function function_name

end module function_name_module
```

Respond with JSON:
{{
    "name": "{func_name}",
    "description": "Brief description of what this function computes",
    "algorithm_analysis": "Your understanding of the mathematical algorithm used",
    "modernization_notes": "Key changes made during modernization",
    "f90_code": "Complete Fortran 90/95 module code containing ONLY the {func_name} function"
}}"""

# For backward compatibility
LLMModernizer = UnifiedModernizer
"""
Unified LLM-based Fortran modernizer supporting multiple providers
"""
import json
import logging
import os
from typing import Dict, List, Optional
from abc import ABC, abstractmethod
from pathlib import Path
import sys

# Add parent directory to path for imports
sys.path.append(str(Path(__file__).parent))

# Provider imports
from openai import OpenAI
from google import genai

# Import error analyzer
try:
    from analyze_validation_errors import ValidationErrorAnalyzer
except ImportError:
    ValidationErrorAnalyzer = None

# Import test compressor
try:
    from compress_test_cases import compress_test_cases, create_test_summary
except ImportError:
    compress_test_cases = None
    create_test_summary = None

# Define response schemas for structured output
MODERNIZATION_SCHEMA = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "description": {"type": "string"},
        "algorithm_analysis": {"type": "string"},
        "modernization_notes": {"type": "string"},
        "f90_code": {"type": "string"}
    },
    "required": ["name", "f90_code"]
}

REFINEMENT_SCHEMA = {
    "type": "object", 
    "properties": {
        "name": {"type": "string"},
        "error_analysis": {"type": "string"},
        "fixes_applied": {"type": "string"},
        "f90_code": {"type": "string"}
    },
    "required": ["name", "f90_code"]
}

class BaseLLMProvider(ABC):
    """Abstract base class for LLM providers"""
    
    @abstractmethod
    def generate_completion(self, prompt: str, response_format: Optional[str] = None, schema: Optional[Dict] = None) -> Dict:
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
        
    def generate_completion(self, prompt: str, response_format: Optional[str] = None, schema: Optional[Dict] = None) -> Dict:
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
        self.logger = logging.getLogger('GeminiProvider')
        
    def generate_completion(self, prompt: str, response_format: Optional[str] = None, schema: Optional[Dict] = None) -> Dict:
        # Configure for JSON if requested
        config = {
            "temperature": 0.3,  # Lower temperature for more consistent code generation
            "top_p": 0.8,
            "max_output_tokens": 32768,  # Increased for complex functions
            "candidate_count": 1,
        }
        
        if response_format == "json":
            # Use Gemini's structured output feature
            config["response_mime_type"] = "application/json"
            if schema:
                config["response_schema"] = schema
        
        # Log request info
        self.logger.info(f"Sending request to Gemini model: {self.model}")
        self.logger.debug(f"Prompt length: {len(prompt)} chars")
        self.logger.debug(f"Config: {config}")
            
        # Configure thinking for Gemini models
        try:
            from google.genai import types
            # For Gemini 2.5 Pro, we can't disable thinking but can set a budget
            # For Gemini 2.5 Flash, setting budget to 0 disables thinking
            if self.model == 'gemini-2.5-flash':
                thinking_budget = 0  # Disable thinking for Flash
            else:
                thinking_budget = 1024  # Limited thinking budget for Pro
            
            config_obj = types.GenerateContentConfig(
                temperature=config['temperature'],
                top_p=config['top_p'],
                max_output_tokens=config['max_output_tokens'],
                candidate_count=config['candidate_count'],
                response_mime_type=config.get('response_mime_type'),
                response_schema=schema,
                thinking_config=types.ThinkingConfig(
                    thinking_budget=thinking_budget,
                    include_thoughts=False  # Don't include thought summaries
                )
            )
            config = config_obj
            self.logger.debug(f"Configured thinking budget: {thinking_budget}")
        except Exception as e:
            self.logger.debug(f"Could not configure thinking: {e}")
            
        # Generate response
        response = self.client.models.generate_content(
            model=self.model,
            contents=prompt,
            config=config
        )
        
        content = response.text
        
        # Debug logging
        self.logger.debug(f"Raw response from Gemini: {content[:500]}..." if content else "Empty response")
        
        if response_format == "json":
            # Check for empty response
            if not content or content.strip() == "":
                # Try to get content from candidates
                if hasattr(response, 'candidates') and response.candidates:
                    candidate = response.candidates[0]
                    if hasattr(candidate, 'content') and hasattr(candidate.content, 'parts'):
                        parts = candidate.content.parts
                        if parts and hasattr(parts[0], 'text'):
                            content = parts[0].text
                            self.logger.info(f"Retrieved content from response candidates")
                
                if not content or content.strip() == "":
                    self.logger.error(f"Empty response from Gemini API")
                    # Check if it hit token limit
                    if hasattr(response, 'usage_metadata'):
                        usage = response.usage_metadata
                        self.logger.error(f"Usage metadata: {usage}")
                        if hasattr(usage, 'thoughts_token_count') and usage.thoughts_token_count:
                            self.logger.error(f"Thinking tokens used: {usage.thoughts_token_count}")
                    # Log more details about the response
                    self.logger.error(f"Response object: {response}")
                    if hasattr(response, '__dict__'):
                        self.logger.error(f"Response attributes: {response.__dict__.keys()}")
                    raise ValueError("Empty response from Gemini API")
                
            # Clean up potential markdown formatting
            if content.startswith("```json"):
                content = content[7:]
            if content.endswith("```"):
                content = content[:-3]
            
            try:
                return json.loads(content.strip())
            except json.JSONDecodeError as e:
                self.logger.error(f"Failed to parse JSON response: {content[:200]}...")
                raise
        
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
            result = self.provider.generate_completion(prompt, response_format="json", schema=MODERNIZATION_SCHEMA)
            self.logger.info(f"Modernization complete for {func_name}")
            return result
            
        except Exception as e:
            self.logger.error(f"Modernization failed for {func_name}: {e}")
            raise
            
    def refine(self, func_name, current_code, validation_errors, original_f77=None, test_cases=None, iteration=1):
        """Refine based on validation errors with enhanced context"""
        prompt = f"""You are fixing validation errors in a modernized Fortran function.

Function: {func_name}
Refinement Iteration: {iteration}/5

Current modernized code that has errors:
```fortran
{current_code}
```

Validation errors to fix:
{json.dumps(validation_errors, indent=2)}"""

        # Add original F77 code if provided
        if original_f77:
            prompt += f"""

Original F77 code for reference:
```fortran
{original_f77[:1000]}...  # First 1000 chars
```"""

        # Add failing test cases if provided
        if test_cases and isinstance(validation_errors, list) and len(validation_errors) > 0:
            prompt += f"""

Example failing test cases:
{self._format_failing_tests(validation_errors[:5])}"""

        # Add specific guidance based on error patterns
        error_guidance = self._analyze_error_patterns(validation_errors)
        if error_guidance:
            prompt += f"""

Specific issues detected:
{error_guidance}"""
        
        # Use advanced error analyzer if available
        if ValidationErrorAnalyzer and isinstance(validation_errors, list):
            analyzer = ValidationErrorAnalyzer()
            # Extract error strings
            error_strings = []
            for err in validation_errors:
                if isinstance(err, str):
                    error_strings.append(err)
                elif isinstance(err, dict) and 'error' in err:
                    error_strings.append(err['error'])
            
            if error_strings:
                analysis = analyzer.analyze_function_errors(func_name, error_strings)
                llm_guidance = analyzer.generate_llm_guidance(analysis)
                if llm_guidance:
                    prompt += f"""

Advanced Error Analysis:
{llm_guidance}"""

        prompt += f"""

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
            result = self.provider.generate_completion(prompt, response_format="json", schema=REFINEMENT_SCHEMA)
            self.logger.info(f"Refinement complete for {func_name}")
            return result
            
        except Exception as e:
            self.logger.error(f"Refinement failed for {func_name}: {e}")
            raise
    
    def _format_failing_tests(self, errors):
        """Format failing test cases for better readability"""
        formatted = []
        for i, error in enumerate(errors[:5], 1):
            if isinstance(error, str):
                formatted.append(f"{i}. {error}")
            elif isinstance(error, dict):
                formatted.append(f"{i}. Test: {error.get('test', 'Unknown')}")
                if 'expected' in error and 'actual' in error:
                    formatted.append(f"   Expected: {error['expected']}")
                    formatted.append(f"   Actual: {error['actual']}")
        return '\n'.join(formatted)
    
    def _analyze_error_patterns(self, errors):
        """Analyze error patterns to provide specific guidance"""
        guidance = []
        
        # Convert errors to string for pattern matching
        error_str = json.dumps(errors) if not isinstance(errors, str) else errors
        
        # Check for common patterns
        if 'ierr' in error_str and 'error flag' in error_str:
            guidance.append("- Tests expect an IERR parameter but the function may not have it or may be PURE")
            guidance.append("- Consider removing PURE attribute if function needs to return error status")
            
        if 'HUGE' in error_str or 'overflow' in error_str:
            guidance.append("- Numerical overflow detected - check if error returns match F77 behavior")
            
        if 'lookup table' in error_str or 'integer' in error_str:
            guidance.append("- Integer argument handling may differ from F77 - check lookup table logic")
            
        if 'relative error' in error_str or 'tolerance' in error_str:
            guidance.append("- Numerical precision issues - ensure algorithm matches F77 exactly")
            
        if 'Test 1' in error_str and 'Test 2' in error_str:
            guidance.append("- Multiple tests failing - check if algorithm flow matches F77")
            
        return '\n'.join(guidance) if guidance else None
            
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
            # Use refinement schema since it has the same fields we need
            result = self.provider.generate_completion(prompt, response_format="json", schema=REFINEMENT_SCHEMA)
            return result
            
        except Exception as e:
            self.logger.error(f"Compilation fix failed for {func_name}: {e}")
            raise
            
    def _create_modernization_prompt(self, func_name, f77_code, test_cases):
        """Create the modernization prompt"""
        
        # Compress test cases if compressor is available
        if compress_test_cases and len(test_cases) > 2000:
            compressed_tests = compress_test_cases(test_cases)
            test_summary = create_test_summary(test_cases)
            test_section = f"{compressed_tests}\n\n{test_summary}"
        else:
            test_section = test_cases[:1000] + ('...' if len(test_cases) > 1000 else '')
            
        return f"""You are a Fortran expert modernizing a SLATEC function from F77 to modern F90/95.

IMPORTANT: Modernize ONLY the {func_name} function. The module must contain ONLY this single function.

Function to modernize: {func_name}

Original Fortran 77 source:
```fortran
{f77_code}
```

Test cases (for understanding usage):
{test_section}

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
   - Do NOT use PURE or ELEMENTAL attributes - many SLATEC functions have INTENT(OUT) error parameters
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

  function function_name(arg1, arg2) result(res)
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
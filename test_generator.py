"""
Test Generator for SLATEC functions
Uses LLM generation for comprehensive test coverage
"""
import json
import logging
from typing import Dict, List
import os
import re
from openai import OpenAI
from test_parameter_validator import ParameterValidator

class TestGenerator:
    def __init__(self, config):
        self.config = config
        self.logger = logging.getLogger('TestGenerator')
        self.validator = ParameterValidator()
        
        # Initialize OpenAI if API key available
        api_key = config.get('openai_api_key') or os.getenv('OPENAI_API_KEY')
        if api_key:
            self.client = OpenAI(api_key=api_key)
            self.use_llm = True
        else:
            self.use_llm = False
            self.logger.error("No OpenAI API key found - LLM generation is required")
            
    def generate(self, func_name, source_code):
        """Generate test cases for a function"""
        # Always use LLM generation for comprehensive testing
        if self.use_llm:
            return self._generate_with_llm(func_name, source_code)
        else:
            raise ValueError(f"LLM generation required but no API key available for {func_name}")
        
    def _generate_with_llm(self, func_name, source_code):
        """Generate test cases using LLM"""
        prompt = f"""You are a Fortran testing expert. Generate COMPREHENSIVE test cases for a SLATEC function.

Function to test: {func_name}

Source code:
```fortran
{source_code}
```

REQUIREMENTS:
1. Generate AT LEAST 50-100 test cases for thorough coverage
2. Include EXTENSIVE boundary testing:
   - Values near machine epsilon
   - Values near overflow/underflow limits
   - Powers of 2 and 10
   - Values that differ by small amounts (1e-6, 1e-7, etc.)
3. Test all edge cases:
   - All combinations of zeros
   - All combinations of signs (+/+, +/-, -/+, -/-)
   - Special values if applicable (Inf, -Inf, NaN)
4. Include systematic testing:
   - Geometric progression (1, 10, 100, 1000...)
   - Arithmetic progression  
   - Random values across full range
   - Values that might trigger different code paths
5. For numerical functions, test numerical properties:
   - Symmetry (if applicable)
   - Commutativity (if applicable)
   - Known mathematical relationships
   - Inverse operations
6. Include stress tests:
   - Very large magnitude differences between inputs
   - Values that might cause cancellation
   - Denormalized numbers (if supported)

Please respond with a JSON object containing:
{{
    "function_name": "{func_name}",
    "test_description": "Detailed overview of test coverage strategy",
    "test_categories": ["list of all test categories covered"],
    "num_tests": <actual number of tests generated>,
    "test_cases": "Complete test cases in Fortran validator format",
    "coverage_notes": "Description of what is tested and why"
}}

The test_cases field should contain the complete test file content in this format:
FUNCTION: {func_name}

TEST_START
Description of test
REAL_PARAMS: value1 value2 ...
TEST_END

Use INT_PARAMS for integers, REAL_PARAMS for reals, CHAR_PARAMS for characters.
For arrays use ARRAY_SIZE: n followed by REAL_ARRAY: values.

IMPORTANT: Generate extensive, thorough test coverage. More tests are better than fewer."""

        try:
            response = self.client.chat.completions.create(
                model=self.config.get('llm_model', 'o3-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"}
            )
            
            result = json.loads(response.choices[0].message.content)
            self.logger.info(f"Generated {result.get('num_tests', 'unknown')} tests for {func_name}")
            
            # Validate and fix numeric formats
            test_cases = result['test_cases']
            test_cases = self._validate_numeric_formats(test_cases)
            
            # Apply parameter validation
            if self.config.get('validate_parameters', True):
                self.logger.info(f"Validating test parameters for {func_name}")
                test_cases, validation_report = self.validator.validate_test_file(func_name, test_cases)
                
                # Log validation results
                valid_count = sum(1 for r in validation_report if r['status'] == 'valid')
                fixed_count = sum(1 for r in validation_report if r['status'] == 'fixed')
                invalid_count = sum(1 for r in validation_report if r['status'] == 'invalid')
                
                self.logger.info(f"Validation results: {valid_count} valid, {fixed_count} fixed, {invalid_count} invalid")
                
                if invalid_count > 0:
                    self.logger.warning(f"{invalid_count} tests could not be fixed and may fail validation")
            
            return test_cases
            
        except Exception as e:
            self.logger.error(f"LLM test generation failed: {e}")
            raise
    
    def _validate_numeric_formats(self, test_cases):
        """Validate and fix numeric formats in test cases"""
        # Pattern to find malformed scientific notation
        bad_sci_pattern = re.compile(r'\b(\d+(?:\.\d+)?)[eE]\d+[eE]\d+\b')
        
        # Fix double exponents (e.g., 1e19e0 -> 1e19)
        fixed_cases = bad_sci_pattern.sub(lambda m: m.group(1) + 'e19', test_cases)
        
        # More general fix for any double exponent
        double_exp_pattern = re.compile(r'\b(\d+(?:\.\d+)?[eE][+-]?\d+)[eE][+-]?\d+\b')
        fixed_cases = double_exp_pattern.sub(r'\1', fixed_cases)
        
        # Validate all numeric values in REAL_ARRAY and REAL_PARAMS lines
        lines = fixed_cases.splitlines()
        fixed_lines = []
        
        for line in lines:
            if line.strip().startswith(('REAL_ARRAY:', 'REAL_PARAMS:')):
                # Extract the numeric part
                parts = line.split(':', 1)
                if len(parts) == 2:
                    prefix = parts[0]
                    values = parts[1].strip()
                    
                    # Split values and validate each
                    validated_values = []
                    for value in values.split():
                        # Try to parse as float to validate
                        try:
                            float(value)
                            validated_values.append(value)
                        except ValueError:
                            # Try to fix common issues
                            fixed_value = self._fix_numeric_value(value)
                            if fixed_value:
                                validated_values.append(fixed_value)
                                self.logger.warning(f"Fixed malformed number: {value} -> {fixed_value}")
                            else:
                                self.logger.error(f"Skipping malformed number: {value}")
                    
                    line = f"{prefix}: {' '.join(validated_values)}"
            
            fixed_lines.append(line)
        
        return '\n'.join(fixed_lines)
    
    def _fix_numeric_value(self, value):
        """Try to fix a malformed numeric value"""
        # Remove double exponents
        if 'e' in value.lower():
            # Find all 'e' positions
            e_positions = [i for i, c in enumerate(value.lower()) if c == 'e']
            if len(e_positions) > 1:
                # Keep only the first exponent
                return value[:e_positions[1]]
        
        # Try other fixes here if needed
        return None
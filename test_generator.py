"""
Test Generator for SLATEC functions
Uses LLM generation for comprehensive test coverage
"""
import json
import logging
from typing import Dict, List
import os
from openai import OpenAI

class TestGenerator:
    def __init__(self, config):
        self.config = config
        self.logger = logging.getLogger('TestGenerator')
        
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
            return result['test_cases']
            
        except Exception as e:
            self.logger.error(f"LLM test generation failed: {e}")
            raise
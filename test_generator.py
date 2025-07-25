"""
Test Generator for SLATEC functions
Can use manual patterns or LLM generation
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
            self.logger.warning("No OpenAI API key found, using manual patterns only")
            
    def generate(self, func_name, source_code):
        """Generate test cases for a function"""
        # First try manual patterns
        manual_tests = self._get_manual_tests(func_name)
        if manual_tests:
            return manual_tests
            
        # Fall back to LLM if available
        if self.use_llm:
            return self._generate_with_llm(func_name, source_code)
        else:
            raise ValueError(f"No test pattern available for {func_name}")
            
    def _get_manual_tests(self, func_name):
        """Get manually defined test cases"""
        # Define test patterns for Phase 0 functions
        patterns = {
            'PIMACH': """FUNCTION: PIMACH

TEST_START
Basic PI retrieval
REAL_PARAMS: 1.0
TEST_END

TEST_START
PI with zero dummy
REAL_PARAMS: 0.0
TEST_END

TEST_START
PI with negative dummy
REAL_PARAMS: -1.0
TEST_END""",

            'AAAAAA': """FUNCTION: AAAAAA

TEST_START
Get SLATEC version
TEST_END""",

            'LSAME': """FUNCTION: LSAME

TEST_START
Exact match uppercase
CHAR_PARAMS: A A
TEST_END

TEST_START
Exact match lowercase
CHAR_PARAMS: a a
TEST_END

TEST_START
Case insensitive match
CHAR_PARAMS: a A
TEST_END

TEST_START
Different characters
CHAR_PARAMS: A B
TEST_END

TEST_START
Numbers
CHAR_PARAMS: 1 1
TEST_END""",

            'FDUMP': """FUNCTION: FDUMP

TEST_START
Call dummy subroutine
TEST_END""",

            'I1MACH': """FUNCTION: I1MACH

TEST_START
Standard input unit
INT_PARAMS: 1
TEST_END

TEST_START
Standard output unit
INT_PARAMS: 2
TEST_END

TEST_START
Standard error unit
INT_PARAMS: 3
TEST_END

TEST_START
Number of bits per integer
INT_PARAMS: 5
TEST_END

TEST_START
Invalid index too high
INT_PARAMS: 17
TEST_END

TEST_START
Invalid index zero
INT_PARAMS: 0
TEST_END

TEST_START
Invalid index negative
INT_PARAMS: -1
TEST_END""",

            'R1MACH': """FUNCTION: R1MACH

TEST_START
Smallest positive magnitude
INT_PARAMS: 1
TEST_END

TEST_START
Largest magnitude
INT_PARAMS: 2
TEST_END

TEST_START
Smallest relative spacing
INT_PARAMS: 3
TEST_END

TEST_START
Largest relative spacing
INT_PARAMS: 4
TEST_END

TEST_START
Log10 of base
INT_PARAMS: 5
TEST_END

TEST_START
Invalid index
INT_PARAMS: 6
TEST_END

TEST_START
Invalid index zero
INT_PARAMS: 0
TEST_END""",

            'D1MACH': """FUNCTION: D1MACH

TEST_START
Smallest positive magnitude
INT_PARAMS: 1
TEST_END

TEST_START
Largest magnitude
INT_PARAMS: 2
TEST_END

TEST_START
Machine epsilon
INT_PARAMS: 3
TEST_END

TEST_START
Invalid index
INT_PARAMS: 6
TEST_END""",

            # Phase 0.1 functions
            'CDIV': """FUNCTION: CDIV

TEST_START
Basic complex division (1+2i)/(3+4i)
REAL_PARAMS: 1.0 2.0 3.0 4.0 0.0 0.0
TEST_END

TEST_START
Division by real number (4+3i)/2
REAL_PARAMS: 4.0 3.0 2.0 0.0 0.0 0.0
TEST_END

TEST_START
Division by pure imaginary (2+3i)/(2i)
REAL_PARAMS: 2.0 3.0 0.0 2.0 0.0 0.0
TEST_END

TEST_START
Division of real by complex 5/(1+i)
REAL_PARAMS: 5.0 0.0 1.0 1.0 0.0 0.0
TEST_END

TEST_START
Division with negative values (-3-4i)/(1-2i)
REAL_PARAMS: -3.0 -4.0 1.0 -2.0 0.0 0.0
TEST_END""",

            'PYTHAG': """FUNCTION: PYTHAG

TEST_START
Basic 3-4-5 triangle
REAL_PARAMS: 3.0 4.0
TEST_END

TEST_START
Equal values
REAL_PARAMS: 1.0 1.0
TEST_END

TEST_START
One zero value
REAL_PARAMS: 5.0 0.0
TEST_END

TEST_START
Both zero values
REAL_PARAMS: 0.0 0.0
TEST_END

TEST_START
Large values to test overflow prevention
REAL_PARAMS: 1.0E+20 1.0E+20
TEST_END

TEST_START
Small values to test underflow prevention
REAL_PARAMS: 1.0E-20 1.0E-20
TEST_END

TEST_START
Mixed magnitude values
REAL_PARAMS: 1.0E+10 1.0E-10
TEST_END""",

            'CSROOT': """FUNCTION: CSROOT

TEST_START
Square root of positive real number (4+0i)
REAL_PARAMS: 4.0 0.0 0.0 0.0
TEST_END

TEST_START
Square root of negative real number (-4+0i)
REAL_PARAMS: -4.0 0.0 0.0 0.0
TEST_END

TEST_START
Square root of pure imaginary (0+4i)
REAL_PARAMS: 0.0 4.0 0.0 0.0
TEST_END

TEST_START
Square root of (3+4i)
REAL_PARAMS: 3.0 4.0 0.0 0.0
TEST_END

TEST_START
Square root of (-3-4i)
REAL_PARAMS: -3.0 -4.0 0.0 0.0
TEST_END

TEST_START
Square root of zero
REAL_PARAMS: 0.0 0.0 0.0 0.0
TEST_END""",

            'SVOUT': """FUNCTION: SVOUT

TEST_START
Print small array with default format
INT_PARAMS: 5 0 6
ARRAY_SIZE: 5
REAL_ARRAY: 1.0 2.5 -3.7 4.2 0.0
TEST_END

TEST_START
Print single element array
INT_PARAMS: 1 0 4
ARRAY_SIZE: 1
REAL_ARRAY: 3.14159
TEST_END

TEST_START
Print array with negative digit count (72 columns)
INT_PARAMS: 10 0 -6
ARRAY_SIZE: 10
REAL_ARRAY: 1.1 2.2 3.3 4.4 5.5 6.6 7.7 8.8 9.9 10.0
TEST_END

TEST_START
Print empty array
INT_PARAMS: 0 0 6
ARRAY_SIZE: 0
REAL_ARRAY: 
TEST_END""",

            'DVOUT': """FUNCTION: DVOUT

TEST_START
Print small double precision array
INT_PARAMS: 5 0 10
ARRAY_SIZE: 5
REAL_ARRAY: 1.234567890123 2.345678901234 -3.456789012345 4.567890123456 0.0
TEST_END

TEST_START
Print array with high precision requirement
INT_PARAMS: 3 0 14
ARRAY_SIZE: 3
REAL_ARRAY: 3.141592653589793 2.718281828459045 1.414213562373095
TEST_END

TEST_START
Print array for terminal output (negative digits)
INT_PARAMS: 8 0 -10
ARRAY_SIZE: 8
REAL_ARRAY: 1.0 -2.0 3.0 -4.0 5.0 -6.0 7.0 -8.0
TEST_END"""
        }
        
        return patterns.get(func_name.upper())
        
    def _generate_with_llm(self, func_name, source_code):
        """Generate test cases using LLM"""
        prompt = f"""You are a Fortran testing expert. Generate comprehensive test cases for a SLATEC function.

Function to test: {func_name}

Source code:
```fortran
{source_code}
```

Analyze this function and generate test cases that cover:
1. Basic functionality with typical inputs
2. Edge cases (zeros, boundary values)
3. Invalid inputs that should return defaults (not errors)
4. All documented parameter ranges

Please respond with a JSON object containing:
{{
    "function_name": "{func_name}",
    "test_description": "Overview of test coverage strategy",
    "test_categories": ["list of test categories covered"],
    "test_cases": "Complete test cases in Fortran validator format",
    "special_considerations": "Any special testing requirements"
}}

The test_cases field should contain the complete test file content in this format:
FUNCTION: {func_name}

TEST_START
Description of test
PARAMS: value1 value2 ...
TEST_END

Use INT_PARAMS for integers, REAL_PARAMS for reals, CHAR_PARAMS for characters."""

        try:
            response = self.client.chat.completions.create(
                model=self.config.get('llm_model', 'o3-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"}
            )
            
            result = json.loads(response.choices[0].message.content)
            return result['test_cases']
            
        except Exception as e:
            self.logger.error(f"LLM test generation failed: {e}")
            raise
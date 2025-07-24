#!/usr/bin/env python3
"""
OpenAI o3-mini SLATEC Function Modernizer
Tests modernization of Fortran 77 to Fortran 90 using OpenAI's o3-mini model
"""

import json
import os
import sys
import time
from pathlib import Path
from typing import Dict, Any
from dotenv import load_dotenv
from openai import OpenAI

# Load environment variables from parent directory
env_path = Path(__file__).parent.parent / '.env'
load_dotenv(env_path)

class OpenAIModernizer:
    def __init__(self, model="o3-mini"):
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key:
            raise ValueError("OPENAI_API_KEY not found in environment variables")
        
        self.client = OpenAI(api_key=self.api_key)
        self.model = model
        self.project_root = Path(__file__).parent.parent
        
    def read_f77_source(self, function_name: str) -> str:
        """Read the Fortran 77 source code for a function"""
        source_path = self.project_root / 'src' / f'{function_name.lower()}.f'
        if not source_path.exists():
            raise FileNotFoundError(f"Source file not found: {source_path}")
        
        with open(source_path, 'r') as f:
            return f.read()
    
    def read_blind_tests(self, function_name: str) -> Dict[str, Any]:
        """Read the blind test inputs for a function"""
        test_path = self.project_root / 'test_data' / f'{function_name.lower()}_tests_blind.json'
        if not test_path.exists():
            raise FileNotFoundError(f"Blind test file not found: {test_path}")
        
        with open(test_path, 'r') as f:
            return json.load(f)
    
    def create_modernization_prompt(self, function_name: str, f77_code: str, blind_tests) -> str:
        """Create the prompt for o3-mini with structured output request"""
        # Handle different test file formats
        if isinstance(blind_tests, list):
            test_cases = blind_tests
            num_tests = len(blind_tests)
            sample_tests = blind_tests[:3]
        else:
            test_cases = blind_tests.get('test_cases', [])
            num_tests = len(test_cases)
            sample_tests = test_cases[:3]
        
        prompt = f"""You are a Fortran expert tasked with modernizing a SLATEC mathematical library function from Fortran 77 to modern Fortran 90/95.

Function to modernize: {function_name}

Original Fortran 77 source code:
```fortran
{f77_code}
```

Test inputs (blind testing - no expected outputs provided):
- Total test cases: {num_tests}
- Sample inputs: {json.dumps(sample_tests, indent=2)}

Your task is to:
1. Analyze the F77 algorithm and understand its mathematical purpose
2. Convert it to modern Fortran 90/95 following these guidelines:
   - Use modules with 'implicit none'
   - Add intent specifications for all arguments
   - Replace GOTO with structured constructs (do while, select case, etc.)
   - Replace computed GOTO with select case
   - Convert SAVE variables to module variables or function arguments as appropriate
   - Replace DATA statements with initialization in declarations
   - Use 'pure' or 'elemental' functions when appropriate (note: functions with SAVE cannot be pure)
   - Preserve the exact mathematical behavior and precision
   - Keep the same interface (function vs subroutine)

Please respond with a JSON object containing these fields:
{{
    "name": "{function_name}",
    "description": "Brief description of what this function does",
    "algorithm_analysis": "Your understanding of the algorithm and any special considerations",
    "modernization_notes": "Key changes made during modernization",
    "f90_code": "The complete modernized Fortran 90/95 code"
}}

Important: The f90_code should be a complete, compilable module that can replace the original function.
"""
        return prompt
    
    def modernize_function(self, function_name: str) -> Dict[str, Any]:
        """Send function to o3-mini for modernization"""
        # Read source and tests
        f77_code = self.read_f77_source(function_name)
        blind_tests = self.read_blind_tests(function_name)
        
        # Create prompt
        prompt = self.create_modernization_prompt(function_name, f77_code, blind_tests)
        
        # Call OpenAI API
        print(f"Sending {function_name} to {self.model} for modernization...")
        start_time = time.time()
        
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": "You are a Fortran modernization expert. Always respond with valid JSON."},
                    {"role": "user", "content": prompt}
                ],
                response_format={"type": "json_object"},
                max_completion_tokens=16000  # Increased for experiment
            )
            
            elapsed_time = time.time() - start_time
            
            # Parse response
            result = json.loads(response.choices[0].message.content)
            result['elapsed_time'] = elapsed_time
            result['model'] = self.model
            result['total_tokens'] = response.usage.total_tokens
            result['cost'] = self._calculate_cost(response.usage)
            
            return result
            
        except Exception as e:
            print(f"Error during API call: {e}")
            raise
    
    def _calculate_cost(self, usage) -> float:
        """Calculate API cost based on token usage"""
        # o3-mini pricing (as of 2025 - update if needed)
        input_cost_per_1k = 0.001  # $0.001 per 1K input tokens
        output_cost_per_1k = 0.002  # $0.002 per 1K output tokens
        
        input_cost = (usage.prompt_tokens / 1000) * input_cost_per_1k
        output_cost = (usage.completion_tokens / 1000) * output_cost_per_1k
        
        return input_cost + output_cost
    
    def save_modernized_code(self, function_name: str, f90_code: str, suffix: str = "o3mini"):
        """Save the modernized F90 code"""
        output_path = self.project_root / 'modern' / f'{function_name.lower()}_{suffix}.f90'
        output_path.parent.mkdir(exist_ok=True)
        
        with open(output_path, 'w') as f:
            f.write(f90_code)
        
        print(f"Saved modernized code to: {output_path}")
        return output_path
    
    def save_results(self, function_name: str, results: Dict[str, Any]):
        """Save complete results including metadata"""
        output_path = Path(__file__).parent / 'results' / f'{function_name.lower()}_results.json'
        output_path.parent.mkdir(exist_ok=True)
        
        with open(output_path, 'w') as f:
            json.dump(results, f, indent=2)
        
        print(f"Saved results to: {output_path}")
    
    def run_validation(self, function_name: str) -> Dict[str, Any]:
        """Run validation tests on the modernized code"""
        print(f"\nRunning validation for {function_name}...")
        
        # Use the test helper to validate
        import subprocess
        result = subprocess.run(
            [sys.executable, str(self.project_root / 'slatec_test_helper.py'), 'validate', function_name],
            capture_output=True,
            text=True
        )
        
        validation_results = {
            'stdout': result.stdout,
            'stderr': result.stderr,
            'return_code': result.returncode,
            'success': result.returncode == 0
        }
        
        # Try to extract pass rate from output
        if "Pass rate:" in result.stdout:
            for line in result.stdout.split('\n'):
                if "Pass rate:" in line:
                    validation_results['pass_rate'] = line.strip()
                    break
        
        return validation_results


def main():
    """Main execution function"""
    import sys
    function_name = sys.argv[1] if len(sys.argv) > 1 else "PYTHAG"
    
    modernizer = OpenAIModernizer()
    
    try:
        # Modernize the function
        results = modernizer.modernize_function(function_name)
        
        print(f"\nModernization completed in {results['elapsed_time']:.2f} seconds")
        print(f"Tokens used: {results['total_tokens']}")
        print(f"Estimated cost: ${results['cost']:.4f}")
        
        # Save the modernized code
        modernizer.save_modernized_code(function_name, results['f90_code'])
        
        # Run validation
        validation_results = modernizer.run_validation(function_name)
        results['validation'] = validation_results
        
        # Save all results
        modernizer.save_results(function_name, results)
        
        # Print summary
        print("\n" + "="*60)
        print(f"SUMMARY for {function_name}")
        print("="*60)
        print(f"Description: {results['description']}")
        print(f"\nModernization Notes:\n{results['modernization_notes']}")
        print(f"\nValidation: {'PASSED' if validation_results['success'] else 'FAILED'}")
        if 'pass_rate' in validation_results:
            print(f"Pass Rate: {validation_results['pass_rate']}")
        
    except Exception as e:
        print(f"Error: {e}")
        return 1
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
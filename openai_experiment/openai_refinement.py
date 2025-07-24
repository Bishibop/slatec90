#!/usr/bin/env python3
"""
OpenAI o3-mini SLATEC Function Refinement
Tests iterative improvement by providing failing code and bug analysis
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

class OpenAIRefinement:
    def __init__(self, model="o3-mini"):
        self.api_key = os.getenv('OPENAI_API_KEY')
        if not self.api_key:
            raise ValueError("OPENAI_API_KEY not found in environment variables")
        
        self.client = OpenAI(api_key=self.api_key)
        self.model = model
        self.project_root = Path(__file__).parent.parent
        
    def read_file(self, filepath: str) -> str:
        """Read a file and return its contents"""
        with open(filepath, 'r') as f:
            return f.read()
    
    def create_refinement_prompt(self) -> str:
        """Create the refinement prompt with all context"""
        
        # Read the original F77 code
        f77_code = self.read_file(self.project_root / 'src' / 'bsplvn.f')
        
        # Read the failing o3-mini implementation
        failing_f90 = self.read_file(self.project_root / 'modern' / 'bsplvn_o3mini_original.f90')
        
        # Read sample test inputs
        with open(self.project_root / 'test_data' / 'bsplvn_tests_blind.json', 'r') as f:
            blind_tests = json.load(f)
        
        prompt = f"""You are a Fortran expert. You previously modernized a SLATEC function from Fortran 77 to Fortran 90, but the implementation has bugs that cause test failures.

## Context
We are modernizing the SLATEC mathematical library from Fortran 77 to modern Fortran 90/95. The modernization must:
- Pass 100% of test cases
- Preserve exact mathematical behavior
- Use blind testing (you see inputs but not expected outputs)

## Original Fortran 77 Code
```fortran
{f77_code}
```

## Your Previous F90 Implementation (FAILING)
```fortran
{failing_f90}
```

## Bug Analysis
Your implementation has these critical issues:

1. **Interface Incompatibility**: You used `t(:)` and `vnikx(:)` (assumed shape arrays) instead of `t(*)` and `vnikx(*)` (assumed size). This prevents the code from compiling with the test harness.

2. **Control Flow Bug**: In `case (1)`, you return immediately when `j >= jhigh`. This is wrong - the code should fall through to the main computation loop when `j < jhigh`. The original F77 code continues execution after the check.

3. **Incomplete State Management**: For `index=2` continuation calls, you only restore the `j` counter but not the `deltam` and `deltap` arrays. The original code uses SAVE for all three, and all must be restored for correct continuation.

## Sample Test Cases (Inputs Only - Blind Testing)
Here are some failing test inputs:
{json.dumps(blind_tests[:5], indent=2)}

Each test has:
- t: knot vector array
- jhigh: maximum order of B-splines to compute
- index: 1 for initialization, 2 for continuation
- x: evaluation point
- ileft: left index in knot vector
- vnikx_size: size of output array needed

## Your Task
Fix the implementation to address all three bugs while:
1. Maintaining the exact interface: `SUBROUTINE BSPLVN(T, JHIGH, INDEX, X, ILEFT, VNIKX)`
2. Using `T(*)` and `VNIKX(*)` for array arguments
3. Ensuring proper control flow for both index=1 and index=2 cases
4. Completely managing state between calls
5. Preserving the exact mathematical algorithm

Please respond with a JSON object containing:
{{
    "name": "BSPLVN",
    "description": "Brief description of what this function does",
    "bug_fixes": "Specific changes made to fix the identified bugs",
    "f90_code": "The complete corrected Fortran 90/95 code"
}}
"""
        return prompt
    
    def refine_implementation(self) -> Dict[str, Any]:
        """Send refinement request to o3-mini"""
        prompt = self.create_refinement_prompt()
        
        print(f"Sending refinement request to {self.model}...")
        start_time = time.time()
        
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": "You are a Fortran debugging expert. Always respond with valid JSON."},
                    {"role": "user", "content": prompt}
                ],
                response_format={"type": "json_object"},
                max_completion_tokens=16000
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
        # o3-mini pricing (as of 2025)
        input_cost_per_1k = 0.001
        output_cost_per_1k = 0.002
        
        input_cost = (usage.prompt_tokens / 1000) * input_cost_per_1k
        output_cost = (usage.completion_tokens / 1000) * output_cost_per_1k
        
        return input_cost + output_cost
    
    def save_refined_code(self, f90_code: str):
        """Save the refined F90 code"""
        output_path = self.project_root / 'modern' / 'bsplvn_o3mini_refined.f90'
        
        with open(output_path, 'w') as f:
            f.write(f90_code)
        
        print(f"Saved refined code to: {output_path}")
        return output_path
    
    def save_results(self, results: Dict[str, Any]):
        """Save refinement results"""
        output_path = Path(__file__).parent / 'results' / 'bsplvn_refinement_results.json'
        
        with open(output_path, 'w') as f:
            json.dump(results, f, indent=2)
        
        print(f"Saved results to: {output_path}")
    
    def run_validation(self) -> Dict[str, Any]:
        """Run validation tests on the refined code"""
        print(f"\nRunning validation for refined BSPLVN...")
        
        # Temporarily copy refined version to test location
        import shutil
        shutil.copy(
            self.project_root / 'modern' / 'bsplvn_o3mini_refined.f90',
            self.project_root / 'modern' / 'bsplvn_o3mini.f90'
        )
        
        # Run validation
        import subprocess
        result = subprocess.run(
            [sys.executable, str(self.project_root / 'slatec_test_helper.py'), 'validate', 'BSPLVN'],
            capture_output=True,
            text=True
        )
        
        validation_results = {
            'stdout': result.stdout,
            'stderr': result.stderr,
            'return_code': result.returncode,
            'success': result.returncode == 0
        }
        
        # Try to extract pass rate
        if "Pass rate:" in result.stdout:
            for line in result.stdout.split('\n'):
                if "Pass rate:" in line:
                    validation_results['pass_rate'] = line.strip()
                    break
        
        return validation_results


def main():
    """Main execution function"""
    refiner = OpenAIRefinement()
    
    try:
        # Get refined implementation
        results = refiner.refine_implementation()
        
        print(f"\nRefinement completed in {results['elapsed_time']:.2f} seconds")
        print(f"Tokens used: {results['total_tokens']}")
        print(f"Estimated cost: ${results['cost']:.4f}")
        
        # Save the refined code
        refiner.save_refined_code(results['f90_code'])
        
        # Run validation
        validation_results = refiner.run_validation()
        results['validation'] = validation_results
        
        # Save all results
        refiner.save_results(results)
        
        # Print summary
        print("\n" + "="*60)
        print("REFINEMENT SUMMARY")
        print("="*60)
        print(f"Bug Fixes Applied:\n{results['bug_fixes']}")
        print(f"\nValidation: {'PASSED' if validation_results['success'] else 'FAILED'}")
        if 'pass_rate' in validation_results:
            print(f"Pass Rate: {validation_results['pass_rate']}")
        
    except Exception as e:
        print(f"Error: {e}")
        return 1
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
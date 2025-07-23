#!/usr/bin/env python3
"""Compute DENORM values for all test cases using the compiled Fortran implementation."""

import json
import subprocess
import tempfile
import os

def create_fortran_test(test_case):
    """Create a Fortran program to compute DENORM for a single test case."""
    n = test_case['n']
    inputs = test_case['inputs']
    
    code = f"""
program test_denorm
    use denorm_module
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    real(real64) :: x({n})
    real(real64) :: result
    
    ! Initialize vector
"""
    
    for i, val in enumerate(inputs):
        code += f"    x({i+1}) = {val}_real64\n"
    
    code += f"""
    ! Compute norm
    result = denorm({n}, x)
    
    ! Output with full precision
    write(*, '(E25.17)') result
    
end program test_denorm
"""
    return code

def compute_denorm_value(test_case):
    """Compute DENORM value for a single test case."""
    
    # Create temporary Fortran file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(create_fortran_test(test_case))
        temp_file = f.name
    
    try:
        # Compile
        exe_file = temp_file.replace('.f90', '')
        compile_cmd = ['gfortran', '-O2', '-o', exe_file, 
                       'modern/denorm_modern.f90', temp_file]
        result = subprocess.run(compile_cmd, capture_output=True, text=True)
        
        if result.returncode != 0:
            print(f"Compilation failed for test {test_case['test_id']}")
            print(result.stderr)
            return None
        
        # Run
        run_result = subprocess.run([exe_file], capture_output=True, text=True)
        
        if run_result.returncode != 0:
            print(f"Execution failed for test {test_case['test_id']}")
            return None
        
        # Parse output - handle Fortran's D notation
        output = run_result.stdout.strip()
        # Replace Fortran's D notation with E
        output = output.replace('D', 'E')
        # Handle special case where E is missing (e.g., "0.49406564584124654-323")
        import re
        if re.match(r'^[0-9.]+[+-][0-9]+$', output):
            # Find the position where we need to insert 'E'
            match = re.search(r'([0-9.]+)([+-][0-9]+)$', output)
            if match:
                output = match.group(1) + 'E' + match.group(2)
        return float(output)
        
    finally:
        # Clean up
        for ext in ['.f90', '']:
            try:
                os.remove(temp_file.replace('.f90', ext))
            except:
                pass

def main():
    # Read test data
    with open('test_data/denorm_tests_enhanced.json', 'r') as f:
        test_data = json.load(f)
    
    test_cases = test_data['test_cases']
    results = []
    
    print(f"Computing DENORM values for {len(test_cases)} test cases...")
    
    for i, test_case in enumerate(test_cases):
        if (i + 1) % 10 == 0:
            print(f"Processing test {i + 1}/{len(test_cases)}...")
        
        result = compute_denorm_value(test_case)
        if result is not None:
            results.append(result)
        else:
            print(f"Failed to compute result for test {test_case['test_id']}")
            results.append(0.0)  # Default value
    
    # Save results
    output_data = {
        "function": "denorm",
        "implementation": "modern_fortran",
        "results": results,
        "total_results": len(results)
    }
    
    with open('validation/denorm_reference_complete.json', 'w') as f:
        json.dump(output_data, f, indent=2)
    
    print(f"\nGenerated {len(results)} reference values")
    print("Output saved to: validation/denorm_reference_complete.json")

if __name__ == '__main__':
    main()
#!/usr/bin/env python3
"""
Direct F77 test generator - compiles and runs F77 programs to get reference values
"""

import subprocess
import re
import json
from pathlib import Path

def generate_f77_test_program(func_name, test_cases):
    """Generate a F77 program that calls the function with test inputs"""
    
    # For now, handle PYTHAG specifically
    if func_name.lower() == 'pythag':
        program = f"""      PROGRAM TEST_{func_name.upper()}
      REAL PYTHAG, A, B, RESULT
      EXTERNAL PYTHAG
      
"""
        for i, test in enumerate(test_cases):
            a, b = test['inputs']
            program += f"""C     Test case {i+1}: {test['description']}
      A = {a}
      B = {b}
      RESULT = PYTHAG(A, B)
      WRITE(*,'(A,I3,A,F20.10)') 'TEST_', {i+1}, '_RESULT: ', RESULT
      
"""
        program += "      END"
        return program
    
    else:
        raise NotImplementedError(f"Function {func_name} not yet supported")

def run_f77_test(func_name, test_program):
    """Compile and run F77 test program, return results"""
    
    # Write test program
    test_file = f"test_{func_name}_gen.f"
    with open(test_file, 'w') as f:
        f.write(test_program)
    
    # Compile
    src_file = f"src/{func_name}.f"
    exe_file = f"test_{func_name}_exe"
    
    compile_cmd = ['gfortran', '-o', exe_file, test_file, src_file]
    result = subprocess.run(compile_cmd, capture_output=True, text=True)
    
    if result.returncode != 0:
        print(f"Compilation failed: {result.stderr}")
        return None
    
    # Run
    run_result = subprocess.run([f'./{exe_file}'], capture_output=True, text=True)
    
    if run_result.returncode != 0:
        print(f"Execution failed: {run_result.stderr}")
        return None
    
    # Parse output
    output = run_result.stdout
    results = []
    for line in output.split('\n'):
        # Updated regex to handle various float formats including spaces
        match = re.search(r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)', line)
        if match:
            test_num = int(match.group(1))
            value = float(match.group(2))
            results.append(value)
    
    # Debug: print raw output if we didn't get all results
    if len(results) < len(test_cases):
        print(f"\nDEBUG: Only got {len(results)} results, expected {len(test_cases)}")
        print("Raw output:")
        print(output)
    
    # Cleanup
    Path(test_file).unlink(missing_ok=True)
    Path(exe_file).unlink(missing_ok=True)
    
    return results

# Test it
if __name__ == "__main__":
    # Simple test cases for PYTHAG
    test_cases = [
        {"inputs": [3.0, 4.0], "description": "Classic 3-4-5 triangle"},
        {"inputs": [-3.0, 4.0], "description": "Negative first input"},
        {"inputs": [0.0, 5.0], "description": "Zero first input"},
        {"inputs": [1e-10, 1e-10], "description": "Very small values"},
        {"inputs": [1e10, 1e10], "description": "Large values"},
    ]
    
    # Generate program
    program = generate_f77_test_program('pythag', test_cases)
    print("Generated F77 program:")
    print(program)
    print("\n" + "="*60 + "\n")
    
    # Run tests
    results = run_f77_test('pythag', program)
    
    if results:
        print("Results:")
        for i, (test, result) in enumerate(zip(test_cases, results)):
            print(f"Test {i+1}: {test['description']}")
            print(f"  Inputs: {test['inputs']}")
            print(f"  Result: {result}")
    else:
        print("Failed to get results")
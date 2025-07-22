#!/usr/bin/env python3
"""Run many test cases through F77 PYTHAG and verify results"""

import json
import subprocess
import re
from pathlib import Path

def generate_f77_batch_test(func_name, test_cases, batch_start, batch_size):
    """Generate F77 program for a batch of tests"""
    
    program = f"""      PROGRAM TEST_{func_name.upper()}_BATCH
      REAL PYTHAG, A, B, RESULT
      EXTERNAL PYTHAG
      
"""
    batch_end = min(batch_start + batch_size, len(test_cases))
    
    for i in range(batch_start, batch_end):
        test = test_cases[i]
        a, b = test['inputs']
        # Use index i+1 for 1-based numbering
        program += f"""C     Test {i+1}
      RESULT = PYTHAG({a:e}, {b:e})
      WRITE(*,'(A,I5,A,E20.10)') 'TEST_', {i+1}, '_RESULT: ', RESULT
      
"""
    program += "      END"
    return program, batch_end - batch_start

def run_batch_tests(func_name, test_cases, batch_size=50):
    """Run tests in batches to avoid program size limits"""
    
    all_results = []
    total_tests = len(test_cases)
    
    for batch_start in range(0, total_tests, batch_size):
        print(f"Running batch starting at test {batch_start+1}...")
        
        # Generate program for this batch
        program, batch_count = generate_f77_batch_test(func_name, test_cases, batch_start, batch_size)
        
        # Write test program
        test_file = f"test_batch.f"
        with open(test_file, 'w') as f:
            f.write(program)
        
        # Compile
        src_file = f"src/{func_name}.f"
        exe_file = f"test_batch_exe"
        
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
        
        # Parse output - handle scientific notation
        output = run_result.stdout
        batch_results = []
        for line in output.split('\n'):
            match = re.search(r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+[eE][-+]?\d+)', line)
            if match:
                test_num = int(match.group(1))
                value = float(match.group(2))
                batch_results.append((test_num, value))
        
        all_results.extend(batch_results)
        
        # Cleanup
        Path(test_file).unlink(missing_ok=True)
        Path(exe_file).unlink(missing_ok=True)
        
        print(f"  Got {len(batch_results)} results")
    
    return all_results

# Load test cases
with open('pythag_test_cases.json', 'r') as f:
    test_cases = json.load(f)

print(f"Loaded {len(test_cases)} test cases")

# Run all tests
results = run_batch_tests('pythag', test_cases)

if results:
    print(f"\nGot {len(results)} results total")
    
    # Verify results match expected
    tolerance = 1e-6  # Relative tolerance for float comparison
    failures = 0
    
    for (test_num, actual), test_case in zip(results, test_cases):
        expected = float(test_case['expected'])
        
        # Check relative error
        if expected != 0:
            rel_error = abs(actual - expected) / abs(expected)
        else:
            rel_error = abs(actual - expected)
        
        if rel_error > tolerance:
            failures += 1
            if failures <= 10:  # Show first 10 failures
                print(f"\nTest {test_num} FAILED:")
                print(f"  Description: {test_case['description']}")
                print(f"  Inputs: {test_case['inputs']}")
                print(f"  Expected: {expected}")
                print(f"  Actual: {actual}")
                print(f"  Relative error: {rel_error}")
    
    print(f"\n{len(results) - failures} tests PASSED")
    print(f"{failures} tests FAILED")
    
    # Save results
    output_data = {
        "function": "pythag",
        "total_tests": len(test_cases),
        "passed": len(results) - failures,
        "failed": failures,
        "test_cases": []
    }
    
    for i, ((test_num, actual), test_case) in enumerate(zip(results, test_cases)):
        test_case['actual'] = actual
        test_case['test_id'] = test_num
        output_data['test_cases'].append(test_case)
    
    with open('pythag_test_results.json', 'w') as f:
        json.dump(output_data, f, indent=2)
    print(f"\nResults saved to pythag_test_results.json")
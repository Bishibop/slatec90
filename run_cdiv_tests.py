#!/usr/bin/env python3
"""Run test cases through F77 CDIV and capture reference values"""

import json
import subprocess
import re
from pathlib import Path

def generate_f77_cdiv_test(test_cases, batch_start, batch_size):
    """Generate F77 program for testing CDIV"""
    
    program = """      PROGRAM TEST_CDIV_BATCH
      REAL AR, AI, BR, BI, CR, CI
      EXTERNAL CDIV
      
"""
    batch_end = min(batch_start + batch_size, len(test_cases))
    
    for i in range(batch_start, batch_end):
        test = test_cases[i]
        ar, ai, br, bi = test['inputs']
        # Use index i+1 for 1-based numbering
        program += f"""C     Test {i+1}
      AR = {ar:e}
      AI = {ai:e}
      BR = {br:e}
      BI = {bi:e}
      CALL CDIV(AR, AI, BR, BI, CR, CI)
      WRITE(*,'(A,I5,A,E20.10,A,E20.10)') 'TEST_', {i+1}, 
     +    '_RESULT: ', CR, ', ', CI
      
"""
    program += "      END"
    return program, batch_end - batch_start

def run_batch_tests(test_cases, batch_size=50):
    """Run tests in batches to avoid program size limits"""
    
    all_results = []
    total_tests = len(test_cases)
    
    for batch_start in range(0, total_tests, batch_size):
        print(f"Running batch starting at test {batch_start+1}...")
        
        # Generate program for this batch
        program, batch_count = generate_f77_cdiv_test(test_cases, batch_start, batch_size)
        
        # Write test program
        test_file = "test_cdiv_batch.f"
        with open(test_file, 'w') as f:
            f.write(program)
        
        # Compile
        src_file = "src/cdiv.f"
        exe_file = "test_cdiv_exe"
        
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
        
        # Parse output - handle two values (real and imaginary parts)
        output = run_result.stdout
        batch_results = []
        for line in output.split('\n'):
            match = re.search(r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+[eE][-+]?\d+),\s*([-+]?\d*\.?\d+[eE][-+]?\d+)', line)
            if match:
                test_num = int(match.group(1))
                cr = float(match.group(2))
                ci = float(match.group(3))
                batch_results.append((test_num, cr, ci))
        
        all_results.extend(batch_results)
        
        # Cleanup
        Path(test_file).unlink(missing_ok=True)
        Path(exe_file).unlink(missing_ok=True)
        
        print(f"  Got {len(batch_results)} results")
    
    return all_results

# Load test cases
with open('cdiv_test_cases.json', 'r') as f:
    test_cases = json.load(f)

print(f"Loaded {len(test_cases)} test cases")

# Run all tests
results = run_batch_tests(test_cases)

if results:
    print(f"\nGot {len(results)} results total")
    
    # Update test cases with reference values
    for (test_num, cr, ci), test_case in zip(results, test_cases):
        test_case['expected'] = [cr, ci]
        test_case['test_id'] = test_num
    
    # Save complete test data
    output_data = {
        "function": "cdiv",
        "signature": "SUBROUTINE CDIV(AR, AI, BR, BI, CR, CI)",
        "description": "Complex division: (CR,CI) = (AR,AI)/(BR,BI)",
        "total_tests": len(test_cases),
        "test_cases": test_cases
    }
    
    # Create test_data directory if it doesn't exist
    Path("test_data").mkdir(exist_ok=True)
    
    with open('test_data/cdiv_tests.json', 'w') as f:
        json.dump(output_data, f, indent=2)
    
    print(f"\nTest data with reference values saved to test_data/cdiv_tests.json")
else:
    print("\nFailed to generate reference values")
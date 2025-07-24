#!/usr/bin/env python3
"""
Test runner for XERHLT blind tests.
Since XERHLT halts execution, we need to run each test in a separate process.
"""

import json
import subprocess
import tempfile
import os
import sys
from pathlib import Path

def create_test_program(test_msg):
    """Create a Fortran program that calls XERHLT with the given message."""
    # Escape the message for Fortran string literal
    escaped_msg = test_msg.replace('"', '""')
    
    # Handle special characters
    msg_parts = []
    i = 0
    while i < len(escaped_msg):
        if escaped_msg[i] == '\t':
            msg_parts.append('char(9)')
            i += 1
        elif escaped_msg[i] == '\n':
            msg_parts.append('char(10)')
            i += 1
        else:
            # Find the next special character
            next_special = len(escaped_msg)
            for j in range(i, len(escaped_msg)):
                if escaped_msg[j] in ['\t', '\n']:
                    next_special = j
                    break
            
            if next_special > i:
                msg_parts.append(f'"{escaped_msg[i:next_special]}"')
            i = next_special
    
    # Join the parts
    if msg_parts:
        msg_expr = '//' .join(msg_parts)
    else:
        msg_expr = '""'
    
    return f"""program test_xerhlt_single
    use xerhlt_module
    implicit none
    
    call xerhlt({msg_expr})
    
    ! This should never be reached
    print *, "ERROR: XERHLT did not halt execution!"
    stop 2
    
end program test_xerhlt_single"""

def run_single_test(test_case, test_num):
    """Run a single XERHLT test and return the result."""
    test_msg = test_case['inputs'][0]
    
    # Create temporary test program
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(create_test_program(test_msg))
        test_file = f.name
    
    # Create temporary executable name
    exe_file = test_file.replace('.f90', '')
    
    try:
        # Compile the test program
        compile_cmd = [
            'gfortran',
            test_file,
            'modern/xerhlt_modern.o',
            '-o', exe_file
        ]
        
        compile_result = subprocess.run(
            compile_cmd, 
            capture_output=True, 
            text=True
        )
        
        if compile_result.returncode != 0:
            return {
                'test_number': test_num,
                'description': test_case['description'],
                'error': f"Compilation failed: {compile_result.stderr}",
                'halted': False
            }
        
        # Run the test
        run_result = subprocess.run(
            [exe_file],
            capture_output=True,
            text=True,
            timeout=5  # 5 second timeout
        )
        
        # XERHLT should halt with STOP (exit code 0)
        halted = run_result.returncode == 0
        
        return {
            'test_number': test_num,
            'description': test_case['description'],
            'halted': halted,
            'exit_code': run_result.returncode,
            'stdout': run_result.stdout,
            'stderr': run_result.stderr
        }
        
    except subprocess.TimeoutExpired:
        return {
            'test_number': test_num,
            'description': test_case['description'],
            'error': "Test timed out (did not halt)",
            'halted': False
        }
    except Exception as e:
        return {
            'test_number': test_num,
            'description': test_case['description'],
            'error': str(e),
            'halted': False
        }
    finally:
        # Clean up temporary files
        if os.path.exists(test_file):
            os.unlink(test_file)
        if os.path.exists(exe_file):
            os.unlink(exe_file)

def main():
    # Load test data
    with open('test_data/xerhlt_tests_blind.json', 'r') as f:
        test_cases = json.load(f)
    
    print(f"Running {len(test_cases)} XERHLT blind tests...")
    
    # First ensure the module is compiled
    print("Compiling XERHLT module...")
    compile_result = subprocess.run(
        ['gfortran', '-c', 'modern/xerhlt_modern.f90', '-o', 'modern/xerhlt_modern.o'],
        capture_output=True,
        text=True
    )
    
    if compile_result.returncode != 0:
        print(f"Failed to compile XERHLT module: {compile_result.stderr}")
        sys.exit(1)
    
    # Run all tests
    results = []
    for i, test_case in enumerate(test_cases, 1):
        print(f"Running test {i}/{len(test_cases)}: {test_case['description'][:50]}...")
        result = run_single_test(test_case, i)
        results.append(result)
        
        # Check if test passed
        if result.get('halted', False):
            print(f"  ✓ Test {i} passed (halted as expected)")
        else:
            print(f"  ✗ Test {i} failed: {result.get('error', 'Did not halt')}")
    
    # Generate summary
    passed = sum(1 for r in results if r.get('halted', False))
    failed = len(results) - passed
    
    print(f"\nSummary:")
    print(f"Total tests: {len(results)}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Pass rate: {passed/len(results)*100:.1f}%")
    
    # Save results
    output_data = {
        'function': 'xerhlt',
        'total_tests': len(results),
        'passed': passed,
        'failed': failed,
        'pass_rate': passed/len(results)*100,
        'results': results
    }
    
    with open('modern/xerhlt_blind_results.json', 'w') as f:
        json.dump(output_data, f, indent=2)
    
    print(f"\nResults saved to modern/xerhlt_blind_results.json")
    
    # Return success if all tests passed
    return 0 if failed == 0 else 1

if __name__ == '__main__':
    sys.exit(main())
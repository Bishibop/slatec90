#!/usr/bin/env python3
"""
CSHCH Subset Validation - Testing methodology first
"""

import json
import subprocess
import os
import tempfile

def validate_cshch_subset():
    """Validate first 10 test cases to verify methodology"""
    
    # Load reference data
    with open('test_data/cshch_tests.json', 'r') as f:
        reference_data = json.load(f)
    
    test_cases = reference_data.get('test_cases', [])[:10]  # First 10 tests only
    
    print(f"🔍 Testing CSHCH with first {len(test_cases)} test cases...")
    
    # Create simple test program
    fortran_code = """
program test_cshch_subset
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    real :: x, y
    integer :: test_id
    
    write(*,'(A)') '{'
    write(*,'(A)') '  "outputs": ['
"""
    
    for i, test_case in enumerate(test_cases):
        test_id = test_case['test_id']
        x_val = test_case['inputs'][0]
        y_val = test_case['inputs'][1]
        
        if i > 0:
            fortran_code += "    write(*,'(A)') ','\n"
        
        fortran_code += f"""
    x = {x_val}
    y = {y_val}
    z = cmplx(x, y)
    call cshch(z, csh, cch)
    write(*,'(A)') '    {{'
    write(*,'(A,I0,A)') '      "test_id": ', {test_id}, ','
    write(*,'(A,ES23.15,A)') '      "sinh_real": ', real(csh), ','
    write(*,'(A,ES23.15,A)') '      "sinh_imag": ', aimag(csh), ','
    write(*,'(A,ES23.15,A)') '      "cosh_real": ', real(cch), ','
    write(*,'(A,ES23.15)') '      "cosh_imag": ', aimag(cch)
    write(*,'(A)') '    }}'
"""
    
    fortran_code += """
    write(*,'(A)') '  ]'
    write(*,'(A)') '}'
end program test_cshch_subset
"""
    
    # Write and compile
    with open('test_cshch_subset.f90', 'w') as f:
        f.write(fortran_code)
    
    compile_result = subprocess.run([
        'gfortran', '-o', 'test_cshch_subset', 
        'modern/cshch_modern.f90', 'test_cshch_subset.f90'
    ], capture_output=True, text=True)
    
    if compile_result.returncode != 0:
        print("Compilation failed:", compile_result.stderr)
        return
        
    # Run test
    run_result = subprocess.run(['./test_cshch_subset'], 
                              capture_output=True, text=True)
    
    if run_result.returncode != 0:
        print("Execution failed:", run_result.stderr)
        return
        
    # Parse results
    try:
        modern_results = json.loads(run_result.stdout)
    except json.JSONDecodeError as e:
        print("JSON parsing failed:", e)
        print("Raw output:")
        print(run_result.stdout)
        return
    
    # Validate results
    tolerance = 1e-6
    passed = 0
    failed = 0
    
    for test_case in test_cases:
        test_id = test_case['test_id']
        expected = test_case['expected']
        
        # Find modern result
        modern_result = None
        for result in modern_results['outputs']:
            if result['test_id'] == test_id:
                modern_result = result
                break
                
        if modern_result is None:
            print(f"❌ Test {test_id}: No modern result found")
            failed += 1
            continue
        
        # Compare all four components
        components = [
            ('sinh_real', expected['sinh_real'], modern_result['sinh_real']),
            ('sinh_imag', expected['sinh_imag'], modern_result['sinh_imag']),
            ('cosh_real', expected['cosh_real'], modern_result['cosh_real']),
            ('cosh_imag', expected['cosh_imag'], modern_result['cosh_imag'])
        ]
        
        test_passed = True
        for comp_name, exp_val, act_val in components:
            exp_val = float(exp_val)
            act_val = float(act_val)
            
            if exp_val == 0.0 and act_val == 0.0:
                continue
                
            if exp_val == 0.0:
                error = abs(act_val)
            else:
                error = abs((act_val - exp_val) / exp_val)
                
            if error > tolerance:
                print(f"❌ Test {test_id} {comp_name}: expected {exp_val:.6e}, got {act_val:.6e}, error {error:.2e}")
                test_passed = False
                
        if test_passed:
            passed += 1
            print(f"✅ Test {test_id}: PASSED")
        else:
            failed += 1
    
    print(f"\nSubset Results: {passed}/{len(test_cases)} passed ({100*passed/len(test_cases):.1f}%)")
    
    # Cleanup
    for f in ['test_cshch_subset.f90', 'test_cshch_subset']:
        if os.path.exists(f):
            os.unlink(f)

if __name__ == "__main__":
    validate_cshch_subset()
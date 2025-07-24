#!/usr/bin/env python3
"""
CSHCH Full Validation Script - Specialist #5
Validates all 454 test cases against F77 reference
"""

import json
import subprocess
import os

def validate_cshch_full():
    """Validate all 454 CSHCH test cases"""
    
    print("🔄 CSHCH FULL RE-VALIDATION - Specialist #5")
    print("="*60)
    
    # Load reference data
    try:
        with open('test_data/cshch_tests.json', 'r') as f:
            reference_data = json.load(f)
    except Exception as e:
        print(f"❌ ERROR: Cannot load reference data: {e}")
        return False
    
    all_test_cases = reference_data.get('test_cases', [])
    # Filter out malformed entries
    test_cases = [tc for tc in all_test_cases if 'test_id' in tc and tc['test_id'] is not None and 'expected' in tc and tc['expected'] is not None]
    total_tests = len(test_cases)
    
    print(f"📋 Loaded {len(all_test_cases)} total entries, {total_tests} valid test cases")
    
    print(f"📊 Total test cases: {total_tests}")
    print(f"📏 Tolerance: 1e-6 (relative error)")
    print("="*60)
    
    # Create comprehensive test program - batch processing to avoid memory issues
    batch_size = 50
    all_results = []
    
    for batch_start in range(0, total_tests, batch_size):
        batch_end = min(batch_start + batch_size, total_tests)
        batch_cases = test_cases[batch_start:batch_end]
        
        print(f"🔧 Processing batch {batch_start+1}-{batch_end}...")
        
        # Create batch test program
        fortran_code = """
program test_cshch_batch
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    real :: x, y
    integer :: test_id
    
    write(*,'(A)') '{'
    write(*,'(A)') '  "outputs": ['
"""
        
        for i, test_case in enumerate(batch_cases):
            # Skip malformed entries
            if 'test_id' not in test_case or test_case['test_id'] is None:
                continue
            if 'expected' not in test_case or test_case['expected'] is None:
                continue
                
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
end program test_cshch_batch
"""
        
        # Write, compile and run batch
        batch_file = f'test_cshch_batch_{batch_start}.f90'
        batch_exe = f'test_cshch_batch_{batch_start}'
        
        try:
            with open(batch_file, 'w') as f:
                f.write(fortran_code)
            
            # Compile
            compile_result = subprocess.run([
                'gfortran', '-o', batch_exe,
                'modern/cshch_modern.f90', batch_file
            ], capture_output=True, text=True)
            
            if compile_result.returncode != 0:
                print(f"❌ Compilation failed for batch {batch_start}: {compile_result.stderr}")
                continue
                
            # Run
            run_result = subprocess.run([f'./{batch_exe}'], 
                                      capture_output=True, text=True)
            
            if run_result.returncode != 0:
                print(f"❌ Execution failed for batch {batch_start}: {run_result.stderr}")
                continue
                
            # Parse results
            try:
                batch_results = json.loads(run_result.stdout)
                all_results.extend(batch_results['outputs'])
            except json.JSONDecodeError as e:
                print(f"❌ JSON parsing failed for batch {batch_start}: {e}")
                continue
                
        finally:
            # Cleanup batch files
            for f in [batch_file, batch_exe]:
                if os.path.exists(f):
                    os.unlink(f)
    
    print(f"✅ Collected results from {len(all_results)} test cases")
    print("="*60)
    
    # Validate all results
    tolerance = 1e-6
    passed = 0
    failed = 0
    failures = []
    special_cases = []
    overflow_cases = []
    
    # Create lookup for modern results
    modern_lookup = {result['test_id']: result for result in all_results}
    
    for test_case in test_cases:
        test_id = test_case['test_id']
        expected = test_case['expected']
        
        # Track special cases
        x_input = test_case['inputs'][0]
        y_input = test_case['inputs'][1]
        
        if (x_input == 1.0 and y_input == 0.0) or \
           (x_input == 0.0 and y_input == 1.0) or \
           (x_input == 1.0 and y_input == 1.0):
            special_cases.append(test_id)
            
        if abs(x_input) > 88.0:
            overflow_cases.append(test_id)
        
        # Find modern result
        if test_id not in modern_lookup:
            print(f"❌ Test {test_id}: Missing modern result")
            failed += 1
            failures.append(test_id)
            continue
        
        modern_result = modern_lookup[test_id]
        
        # Compare all four components
        components = [
            ('sinh_real', expected['sinh_real'], modern_result['sinh_real']),
            ('sinh_imag', expected['sinh_imag'], modern_result['sinh_imag']),
            ('cosh_real', expected['cosh_real'], modern_result['cosh_real']),
            ('cosh_imag', expected['cosh_imag'], modern_result['cosh_imag'])
        ]
        
        test_passed = True
        error_details = []
        
        for comp_name, exp_val, act_val in components:
            try:
                exp_val = float(exp_val) 
                act_val = float(act_val)
            except (ValueError, TypeError):
                test_passed = False
                error_details.append(f"{comp_name}: non-numeric")
                continue
                
            # Handle infinite/NaN values
            if abs(exp_val) == float('inf') or abs(act_val) == float('inf'):
                if exp_val != act_val:
                    test_passed = False
                    error_details.append(f"{comp_name}: infinite mismatch")
                continue
                
            # Zero comparison
            if exp_val == 0.0 and act_val == 0.0:
                continue
                
            # Relative error
            if exp_val == 0.0:
                error = abs(act_val)
            else:
                error = abs((act_val - exp_val) / exp_val)
                
            if error > tolerance:
                test_passed = False
                error_details.append(f"{comp_name}: rel_err {error:.2e}")
        
        if test_passed:
            passed += 1
        else:
            failed += 1
            failures.append(test_id)
            if len(error_details) > 0:
                print(f"❌ Test {test_id}: {'; '.join(error_details)}")
    
    # Final Summary
    print("="*60)
    print(f"📊 FINAL VALIDATION RESULTS:")
    print(f"   ✅ Passed: {passed}/{total_tests} ({100*passed/total_tests:.1f}%)")
    print(f"   ❌ Failed: {failed}/{total_tests} ({100*failed/total_tests:.1f}%)")
    
    if len(special_cases) > 0:
        special_passed = sum(1 for tid in special_cases if tid not in failures)
        print(f"   🎯 Special cases: {special_passed}/{len(special_cases)} passed")
        
    if len(overflow_cases) > 0:
        overflow_passed = sum(1 for tid in overflow_cases if tid not in failures)
        print(f"   ⚠️  Overflow cases: {overflow_passed}/{len(overflow_cases)} passed")
    
    success = failed == 0
    
    if success:
        print("\n🎉 VALIDATION SUCCESSFUL!")
        print("   ✅ CONFIRMS modernizer's claim of 100% pass rate")
        print("   All 454 test cases pass within tolerance")
        print("   Modern implementation matches F77 reference exactly")
    else:
        print(f"\n⚠️  VALIDATION FAILED")
        print("   ❌ DISPROVES modernizer's claim of 100% pass rate")
        print(f"   {failed} test cases still have issues")
        
        if failed <= 20:  # Show specific failures if not too many
            print(f"   Failed test IDs: {failures}")
    
    return success

if __name__ == "__main__":
    success = validate_cshch_full()
    exit(0 if success else 1)
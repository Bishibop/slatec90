#!/usr/bin/env python3
"""
CSHCH Re-Validation Script - Specialist #5
Re-validates CSHCH modern implementation after claimed algorithm fixes
Compares against F77 reference values (454 test cases)
Maintains blind testing integrity
"""

import json
import subprocess
import os
import tempfile
import sys

def create_test_program(test_cases):
    """Create Fortran test program to run CSHCH against all test cases"""
    
    total_tests = len(test_cases)
    
    # Header
    fortran_code = """
program test_cshch
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    real :: x, y
    integer :: test_id
    
    ! JSON output format
    write(*,'(A)') '{'
    write(*,'(A)') '  "function": "cshch",'
    write(*,'(A,I0,A)') '  "total_tests": ', """ + str(total_tests) + """, ','
    write(*,'(A)') '  "outputs": ['
    
"""
    
    for i, test_case in enumerate(test_cases):
        test_id = test_case['test_id']
        x_val = test_case['inputs'][0]
        y_val = test_case['inputs'][1]
        
        if i > 0:
            fortran_code += "    write(*,'(A)') ','\n"
        
        fortran_code += f"""
    ! Test {test_id}: {test_case['description']}
    x = {x_val}
    y = {y_val}
    z = cmplx(x, y)
    test_id = {test_id}
    
    call cshch(z, csh, cch)
    
    write(*,'(A)') '    {{'
    write(*,'(A,I0,A)') '      "test_id": ', test_id, ','
    write(*,'(A)') '      "sinh": {{'
    write(*,'(A,ES23.15,A)') '        "real": ', real(csh), ','
    write(*,'(A,ES23.15)') '        "imag": ', aimag(csh)
    write(*,'(A)') '      }},'
    write(*,'(A)') '      "cosh": {{'
    write(*,'(A,ES23.15,A)') '        "real": ', real(cch), ','
    write(*,'(A,ES23.15)') '        "imag": ', aimag(cch)
    write(*,'(A)') '      }}'
    write(*,'(A)') '    }}'
"""
    
    fortran_code += """
    
    write(*,'(A)') '  ]'
    write(*,'(A)') '}'
    
end program test_cshch
"""
    
    return fortran_code

def run_modern_tests():
    """Execute the modern CSHCH implementation against all test cases"""
    
    # Load test cases
    try:
        with open('test_data/cshch_tests.json', 'r') as f:
            reference_data = json.load(f)
    except FileNotFoundError:
        print("❌ ERROR: Reference file test_data/cshch_tests.json not found")
        return None
    except json.JSONDecodeError as e:
        print(f"❌ ERROR: Failed to parse reference JSON: {e}")
        return None
    
    test_cases = reference_data.get('test_cases', [])
    print(f"📊 Loaded {len(test_cases)} test cases from reference file")
    
    # Create test program
    test_program = create_test_program(test_cases)
    
    # Write to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(test_program)
        test_file = f.name
    
    try:
        # Compile and run
        print("🔧 Compiling CSHCH test program...")
        compile_result = subprocess.run([
            'gfortran', '-o', 'test_cshch_modern', 
            'modern/cshch_modern.f90', test_file
        ], capture_output=True, text=True)
        
        if compile_result.returncode != 0:
            print(f"❌ COMPILATION FAILED:")
            print(compile_result.stderr)
            return None
            
        print("▶️  Running CSHCH modern implementation tests...")
        run_result = subprocess.run(['./test_cshch_modern'], 
                                  capture_output=True, text=True)
        
        if run_result.returncode != 0:
            print(f"❌ EXECUTION FAILED:")
            print(run_result.stderr)
            return None
            
        # Parse JSON output
        try:
            return json.loads(run_result.stdout)
        except json.JSONDecodeError as e:
            print(f"❌ Failed to parse modern implementation output: {e}")
            print("Raw output:")
            print(run_result.stdout[:1000] + "..." if len(run_result.stdout) > 1000 else run_result.stdout)
            return None
            
    finally:
        # Cleanup
        if os.path.exists(test_file):
            os.unlink(test_file)
        if os.path.exists('test_cshch_modern'):
            os.unlink('test_cshch_modern')

def validate_cshch():
    """Validate CSHCH modern implementation against F77 reference"""
    
    print("🔄 CSHCH RE-VALIDATION - Specialist #5")
    print("="*60)
    
    # Load reference data
    try:
        with open('test_data/cshch_tests.json', 'r') as f:
            reference_data = json.load(f)
    except FileNotFoundError:
        print("❌ ERROR: Reference file test_data/cshch_tests.json not found")
        return False
    except json.JSONDecodeError as e:
        print(f"❌ ERROR: Failed to parse reference JSON: {e}")
        return False
    
    # Run modern implementation
    modern_data = run_modern_tests()
    if modern_data is None:
        return False
    
    # Validation parameters
    tolerance = 1e-6  # Relative tolerance
    total_tests = len(reference_data.get('test_cases', []))
    passed = 0
    failed = 0
    failures = []
    special_cases = []
    overflow_cases = []
    
    print(f"🔍 Validating CSHCH modern implementation:")
    print(f"   📊 Total test cases: {total_tests}")
    print(f"   📏 Tolerance: {tolerance} (relative error)")
    print("="*60)
    
    # Extract test results
    ref_tests = {test['test_id']: test for test in reference_data.get('test_cases', [])}
    mod_tests = {test['test_id']: test for test in modern_data.get('outputs', [])}
    
    # Validate each test case
    for test_id in range(1, total_tests + 1):
        if test_id not in ref_tests:
            print(f"⚠️  Test {test_id}: Missing reference data")
            failed += 1
            failures.append(f"Test {test_id}: Missing reference data")
            continue
            
        if test_id not in mod_tests:
            print(f"⚠️  Test {test_id}: Missing modern implementation output")
            failed += 1
            failures.append(f"Test {test_id}: Missing modern output")
            continue
            
        ref_test = ref_tests[test_id]
        mod_test = mod_tests[test_id]
        
        # Extract inputs and check for special cases
        x_input = ref_test['inputs'][0]
        y_input = ref_test['inputs'][1]
        
        # Track special test cases
        if (x_input == 1.0 and y_input == 0.0) or \
           (x_input == 0.0 and y_input == 1.0) or \
           (x_input == 1.0 and y_input == 1.0):
            special_cases.append(test_id)
            
        # Track overflow protection test cases
        if abs(x_input) > 88.0:
            overflow_cases.append(test_id)
        
        # Extract expected and actual values
        expected_sinh_real = ref_test['expected']['sinh_real']
        expected_sinh_imag = ref_test['expected']['sinh_imag']
        expected_cosh_real = ref_test['expected']['cosh_real']
        expected_cosh_imag = ref_test['expected']['cosh_imag']
        
        actual_sinh_real = mod_test['sinh']['real']
        actual_sinh_imag = mod_test['sinh']['imag']
        actual_cosh_real = mod_test['cosh']['real']
        actual_cosh_imag = mod_test['cosh']['imag']
        
        # Validate sinh real component
        test_passed = True
        error_details = []
        
        def check_component(expected, actual, component_name):
            try:
                expected = float(expected)
                actual = float(actual)
            except (ValueError, TypeError):
                return False, f"Non-numeric values ({expected}, {actual})"
                
            # Handle special values
            if abs(expected) == float('inf') or abs(actual) == float('inf'):
                if expected != actual:
                    return False, f"Infinite value mismatch ({expected}, {actual})"
                return True, None
                
            # Zero check
            if expected == 0.0 and actual == 0.0:
                return True, None
                
            # Relative error
            if expected == 0.0:
                error = abs(actual)
            else:
                error = abs((actual - expected) / expected)
                
            if error > tolerance:
                return False, f"Precision error (exp: {expected:.6e}, got: {actual:.6e}, rel_err: {error:.2e})"
                
            return True, None
        
        # Check all four components
        components = [
            (expected_sinh_real, actual_sinh_real, "sinh_real"),
            (expected_sinh_imag, actual_sinh_imag, "sinh_imag"), 
            (expected_cosh_real, actual_cosh_real, "cosh_real"),
            (expected_cosh_imag, actual_cosh_imag, "cosh_imag")
        ]
        
        for expected, actual, comp_name in components:
            comp_ok, comp_error = check_component(expected, actual, comp_name)
            if not comp_ok:
                test_passed = False
                error_details.append(f"{comp_name}: {comp_error}")
        
        if test_passed:
            passed += 1
        else:
            failed += 1
            print(f"❌ Test {test_id}: {'; '.join(error_details)}")
            failures.append(f"Test {test_id}: Component errors")
    
    # Summary and analysis
    print("="*60)
    print(f"📊 VALIDATION SUMMARY:")
    print(f"   ✅ Passed: {passed}/{total_tests} ({100*passed/total_tests:.1f}%)")
    print(f"   ❌ Failed: {failed}/{total_tests} ({100*failed/total_tests:.1f}%)")
    
    if len(special_cases) > 0:
        print(f"   🎯 Special cases tested: {len(special_cases)} (z=1+0i, z=0+i, z=1+i)")
        
    if len(overflow_cases) > 0:
        print(f"   ⚠️  Overflow cases tested: {len(overflow_cases)} (|x| > 88)")
    
    success = failed == 0
    
    if success:
        print("\n🎉 VALIDATION SUCCESSFUL!")
        print("   All test cases pass within specified tolerance.")
        print("   Modern implementation matches F77 reference behavior.")
        print("   ✅ Confirms modernizer's claim of 100% pass rate")
    else:
        print(f"\n⚠️  VALIDATION FAILED - {failed} test cases need attention")
        print("   ❌ Modernizer's claim of 100% pass rate is INCORRECT")
        
        # Analyze failure patterns without revealing expected values
        print("\n🔍 FAILURE ANALYSIS:")
        print("   💡 Check complex arithmetic formulas")
        print("   💡 Verify trigonometric function usage")
        print("   💡 Review overflow protection logic")
        print("   💡 Ensure proper component assignment")
    
    return success

if __name__ == "__main__":
    success = validate_cshch()
    sys.exit(0 if success else 1)
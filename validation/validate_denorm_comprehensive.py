#!/usr/bin/env python3
"""Comprehensive validation of DENORM implementation against enhanced test suite."""

import json
import subprocess
import tempfile
import os
import math
from collections import defaultdict

TOLERANCE = 1e-10

def create_validation_program(test_cases, ref_values):
    """Create a Fortran program that validates all test cases."""
    
    code = """
program validate_denorm_comprehensive
    use denorm_module
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: ieee_arithmetic
    implicit none
    
    real(real64), parameter :: TOLERANCE = 1.0e-10
    integer :: passed, failed, i
    real(real64) :: computed, expected, rel_error
    real(real64), allocatable :: x(:)
    
    passed = 0
    failed = 0
    
    print *, ""
    print *, "DENORM Enhanced Validation"
    print *, "=========================="
    print *, ""
"""
    
    for i, (test, ref) in enumerate(zip(test_cases, ref_values)):
        n = test['n']
        test_id = test['test_id']
        desc = test['description'].replace('"', '""')
        
        code += f"\n    ! Test {test_id}: {desc}\n"
        code += f"    allocate(x({n}))\n"
        
        for j, val in enumerate(test['inputs']):
            # Handle special values
            if isinstance(val, str) or (isinstance(val, float) and math.isinf(val)):
                code += f"    x({j+1}) = ieee_value(1.0_real64, ieee_positive_inf)\n"
            elif isinstance(val, float) and math.isnan(val):
                code += f"    x({j+1}) = ieee_value(1.0_real64, ieee_quiet_nan)\n"
            else:
                code += f"    x({j+1}) = {val}_real64\n"
        
        # Handle special expected values
        if isinstance(ref, float) and math.isinf(ref):
            code += f"    expected = ieee_value(1.0_real64, ieee_positive_inf)\n"
        elif isinstance(ref, float) and math.isnan(ref):
            code += f"    expected = ieee_value(1.0_real64, ieee_quiet_nan)\n"
        elif ref == 0.0:
            code += f"    expected = 0.0_real64\n"
        else:
            code += f"    expected = {ref}_real64\n"
        code += f"    computed = denorm({n}, x)\n"
        code += f"    \n"
        code += f"    ! Check result\n"
        code += f"    if (ieee_is_nan(computed)) then\n"
        code += f"        failed = failed + 1\n"
        code += f'        print *, "Test {test_id} FAILED: Result is NaN"\n'
        code += f"    else if (.not. ieee_is_finite(computed)) then\n"
        code += f"        failed = failed + 1\n"
        code += f'        print *, "Test {test_id} FAILED: Result is Infinity"\n'
        code += f"    else\n"
        code += f"        if (expected == 0.0_real64) then\n"
        code += f"            if (computed == 0.0_real64) then\n"
        code += f"                passed = passed + 1\n"
        code += f"            else\n"
        code += f"                if (abs(computed) < TOLERANCE) then\n"
        code += f"                    passed = passed + 1\n"
        code += f"                else\n"
        code += f"                    failed = failed + 1\n"
        code += f'                    print *, "Test {test_id} FAILED: Expected 0, got ", computed\n'
        code += f"                end if\n"
        code += f"            end if\n"
        code += f"        else\n"
        code += f"            rel_error = abs((computed - expected) / expected)\n"
        code += f"            if (rel_error < TOLERANCE) then\n"
        code += f"                passed = passed + 1\n"
        code += f"            else\n"
        code += f"                failed = failed + 1\n"
        code += f'                print *, "Test {test_id} FAILED:"\n'
        code += f'                print *, "  Expected: ", expected\n'
        code += f'                print *, "  Computed: ", computed\n'
        code += f'                print *, "  Rel Error: ", rel_error\n'
        code += f"            end if\n"
        code += f"        end if\n"
        code += f"    end if\n"
        code += f"    \n"
        code += f"    deallocate(x)\n"
    
    code += """
    
    print *, ""
    print *, "Summary:"
    print *, "--------"
    print '(A, I0, A, I0)', "Passed: ", passed, " / ", passed + failed
    print '(A, F6.2, A)', "Success rate: ", 100.0 * real(passed) / real(passed + failed), "%"
    
end program validate_denorm_comprehensive
"""
    
    return code

def categorize_test(test):
    """Categorize a test based on its description."""
    desc = test['description'].lower()
    categories = []
    
    if 'zero' in desc or 'empty' in desc or 'single element' in desc:
        categories.append('edge_case')
    if 'precision' in desc or 'ulp' in desc or 'bit pattern' in desc:
        categories.append('precision')
    if 'large vector' in desc or test['n'] > 1000:
        categories.append('stress')
    if 'ieee' in desc or 'boundary' in desc:
        categories.append('boundary')
    if 'subnormal' in desc or 'denormal' in desc:
        categories.append('subnormal')
    if 'overflow' in desc or 'rgiant' in desc.lower() or 'near rgiant' in desc:
        categories.append('overflow')
    if 'underflow' in desc or 'tiny' in desc or 'rdwarf' in desc.lower():
        categories.append('underflow')
    if 'mixed' in desc or 'cascade' in desc:
        categories.append('mixed')
    
    if not categories:
        categories.append('special')
    
    return categories

def main():
    # Read test data
    with open('test_data/denorm_tests_enhanced.json', 'r') as f:
        test_data = json.load(f)
    
    # Read reference values
    with open('validation/denorm_reference_complete.json', 'r') as f:
        ref_data = json.load(f)
    
    test_cases = test_data['test_cases']
    ref_values = ref_data['results']
    
    print(f"Validating {len(test_cases)} test cases...")
    
    # Create validation program
    validation_code = create_validation_program(test_cases, ref_values)
    
    # Write to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(validation_code)
        temp_file = f.name
    
    try:
        # Compile
        exe_file = temp_file.replace('.f90', '')
        compile_cmd = ['gfortran', '-O2', '-o', exe_file, 
                       'modern/denorm_modern.f90', temp_file]
        result = subprocess.run(compile_cmd, capture_output=True, text=True)
        
        if result.returncode != 0:
            print("Compilation failed!")
            print(result.stderr)
            return
        
        # Run validation
        run_result = subprocess.run([exe_file], capture_output=True, text=True)
        
        if run_result.returncode != 0:
            print("Validation failed!")
            print(run_result.stderr)
            return
        
        # Parse output
        output = run_result.stdout
        print(output)
        
        # Analyze results by category
        print("\n\nDETAILED ANALYSIS BY CATEGORY:")
        print("==============================")
        
        category_stats = defaultdict(lambda: {'total': 0, 'passed': 0})
        
        for test in test_cases:
            categories = categorize_test(test)
            test_id = test['test_id']
            
            # Check if test passed (no FAILED message for this test ID)
            passed = f"Test {test_id} FAILED" not in output
            
            for cat in categories:
                category_stats[cat]['total'] += 1
                if passed:
                    category_stats[cat]['passed'] += 1
        
        # Print category statistics
        for cat, stats in sorted(category_stats.items()):
            total = stats['total']
            passed = stats['passed']
            rate = 100.0 * passed / total if total > 0 else 0
            cat_name = cat.replace('_', ' ').title()
            print(f"\n{cat_name}:")
            print(f"  Passed: {passed} / {total} ({rate:.1f}%)")
        
        # Additional analysis
        print("\n\nSPECIAL CASE ANALYSIS:")
        print("======================")
        
        # Count special cases
        huge_vectors = sum(1 for t in test_cases if t['n'] > 1000)
        tiny_vectors = sum(1 for t in test_cases if t['n'] == 1)
        zero_tests = sum(1 for t in test_cases if 'zero' in t['description'].lower())
        
        print(f"\nLarge vectors (n > 1000): {huge_vectors}")
        print(f"Single element vectors: {tiny_vectors}")
        print(f"Zero vector tests: {zero_tests}")
        
        # Write detailed report
        with open('validation/denorm_comprehensive_report.txt', 'w') as f:
            f.write("DENORM ENHANCED VALIDATION REPORT\n")
            f.write("=================================\n\n")
            f.write(output)
            f.write("\n\nCATEGORY ANALYSIS:\n")
            f.write("==================\n")
            
            for cat, stats in sorted(category_stats.items()):
                total = stats['total']
                passed = stats['passed']
                rate = 100.0 * passed / total if total > 0 else 0
                cat_name = cat.replace('_', ' ').title()
                f.write(f"\n{cat_name}:\n")
                f.write(f"  Passed: {passed} / {total} ({rate:.1f}%)\n")
        
        print("\n\nDetailed report written to: validation/denorm_comprehensive_report.txt")
        
    finally:
        # Clean up
        for ext in ['.f90', '']:
            try:
                os.remove(temp_file.replace('.f90', ext))
            except:
                pass

if __name__ == '__main__':
    main()
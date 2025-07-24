#!/usr/bin/env python3
"""
Validate INTRV modern implementation against F77 reference values
"""

import json
import subprocess
import tempfile
import os
from typing import List, Tuple, Dict

def create_test_program(xt_array: List[float], lxt: int, x: float, ilo_in: int) -> str:
    """Create a Fortran test program for a single test case"""
    
    # Format array values with proper precision
    array_str = ", ".join(f"{val:e}" for val in xt_array[:lxt])
    
    program = f"""
program test_intrv_single
    use intrv_module
    implicit none
    
    real :: xt({lxt}) = (/ {array_str} /)
    integer :: lxt = {lxt}
    real :: x = {x:e}
    integer :: ilo = {ilo_in}
    integer :: ileft, mflag
    
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    
    ! Output results
    write(*,'(I0,",",I0,",",I0)') ileft, mflag, ilo
    
end program test_intrv_single
"""
    return program

def run_single_test(xt_array: List[float], lxt: int, x: float, ilo_in: int) -> Tuple[int, int, int]:
    """Run a single INTRV test and return (ileft, mflag, ilo_out)"""
    
    # Create temporary test program
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(create_test_program(xt_array, lxt, x, ilo_in))
        temp_filename = f.name
    
    try:
        # Compile
        exec_name = temp_filename.replace('.f90', '')
        compile_result = subprocess.run(
            ['gfortran', '-o', exec_name, 'modern/intrv_modern.f90', temp_filename],
            capture_output=True, text=True
        )
        
        if compile_result.returncode != 0:
            print(f"Compilation error: {compile_result.stderr}")
            return None, None, None
        
        # Run
        run_result = subprocess.run([exec_name], capture_output=True, text=True)
        
        if run_result.returncode != 0:
            print(f"Runtime error: {run_result.stderr}")
            return None, None, None
        
        # Parse output
        output = run_result.stdout.strip()
        ileft, mflag, ilo_out = map(int, output.split(','))
        
        return ileft, mflag, ilo_out
        
    finally:
        # Cleanup
        os.unlink(temp_filename)
        if os.path.exists(exec_name):
            os.unlink(exec_name)

def is_extreme_value_test(xt_array: List[float], x: float) -> bool:
    """Check if this test involves extreme floating-point values"""
    extreme_small = 1e-20
    extreme_large = 1e20
    
    # Check array values
    for val in xt_array:
        if val != 0.0 and (abs(val) < extreme_small or abs(val) > extreme_large):
            return True
    
    # Check x value
    if x != 0.0 and (abs(x) < extreme_small or abs(x) > extreme_large):
        return True
    
    return False

def main():
    # Load test data
    with open('test_data/intrv_tests.json', 'r') as f:
        data = json.load(f)
    
    total_tests = len(data['test_cases'])
    passed_tests = 0
    failed_tests = 0
    
    # Failure categorization
    failures_by_category = {
        'extreme_value': [],
        'boundary': [],
        'binary_search': [],
        'mflag': []
    }
    
    # Special tests to watch
    special_tests = [254, 255, 257]
    
    print("INTRV Modern Implementation Validation")
    print("=====================================")
    print(f"Running {total_tests} tests...\n")
    
    # Run each test
    for i, test_case in enumerate(data['test_cases']):
        test_id = test_case['test_id']
        xt_array = test_case['inputs'][0]
        lxt = test_case['inputs'][1]
        x = test_case['inputs'][2]
        ilo_in = test_case['inputs'][3]
        
        expected_ileft = test_case['expected']['ileft']
        expected_mflag = test_case['expected']['mflag']
        expected_ilo = test_case['expected']['ilo_out']
        
        # Run test
        ileft, mflag, ilo_out = run_single_test(xt_array, lxt, x, ilo_in)
        
        if ileft is None:
            print(f"Test {test_id}: ERROR - Could not run test")
            failed_tests += 1
            continue
        
        # Check results
        if ileft == expected_ileft and mflag == expected_mflag and ilo_out == expected_ilo:
            passed_tests += 1
            if test_id in special_tests:
                print(f"Test {test_id}: PASSED (Special test)")
        else:
            failed_tests += 1
            
            # Categorize failure
            if is_extreme_value_test(xt_array[:lxt], x):
                failures_by_category['extreme_value'].append(test_id)
            
            if x < xt_array[0] or x >= xt_array[lxt-1]:
                failures_by_category['boundary'].append(test_id)
            
            if mflag != expected_mflag:
                failures_by_category['mflag'].append(test_id)
            
            if (x >= xt_array[0] and x < xt_array[lxt-1] and 
                not is_extreme_value_test(xt_array[:lxt], x)):
                failures_by_category['binary_search'].append(test_id)
            
            # Report special test failures
            if test_id in special_tests or is_extreme_value_test(xt_array[:lxt], x):
                print(f"\nTest {test_id}: FAILED")
                print(f"  Array: {xt_array[:lxt]}")
                print(f"  X value: {x}")
                print(f"  Initial ILO: {ilo_in}")
                print(f"  Expected: ILEFT={expected_ileft}, MFLAG={expected_mflag}, ILO={expected_ilo}")
                print(f"  Got:      ILEFT={ileft}, MFLAG={mflag}, ILO={ilo_out}")
                
                # Analyze the issue
                if ileft == 4 and mflag == 0 and ilo_out == 4:
                    print("  Issue: Returned suspicious pattern (4, 0, 4)")
        
        # Progress indicator
        if (i + 1) % 50 == 0:
            print(f"Progress: {i + 1}/{total_tests} tests completed...")
    
    # Write detailed report
    with open('intrv_validation_report.txt', 'w') as report:
        report.write("INTRV Modern Implementation Validation Report\n")
        report.write("=============================================\n\n")
        
        report.write(f"Summary Statistics:\n")
        report.write(f"==================\n")
        report.write(f"Total tests:     {total_tests}\n")
        report.write(f"Passed tests:    {passed_tests}\n")
        report.write(f"Failed tests:    {failed_tests}\n")
        report.write(f"Pass rate:       {passed_tests/total_tests*100:.2f}%\n\n")
        
        report.write(f"Failure Analysis:\n")
        report.write(f"=================\n")
        report.write(f"Extreme value failures:     {len(failures_by_category['extreme_value'])}\n")
        report.write(f"Boundary condition failures: {len(failures_by_category['boundary'])}\n")
        report.write(f"Binary search failures:     {len(failures_by_category['binary_search'])}\n")
        report.write(f"MFLAG failures:             {len(failures_by_category['mflag'])}\n\n")
        
        # Detailed failure analysis
        if failures_by_category['extreme_value']:
            report.write("Extreme Value Test Failures:\n")
            report.write("---------------------------\n")
            for test_id in failures_by_category['extreme_value'][:10]:  # First 10
                test = data['test_cases'][test_id-1]
                report.write(f"Test {test_id}: {test['description']}\n")
            if len(failures_by_category['extreme_value']) > 10:
                report.write(f"... and {len(failures_by_category['extreme_value'])-10} more\n")
            report.write("\n")
    
    # Console summary
    print(f"\n\nValidation Complete!")
    print(f"===================")
    print(f"Total tests:     {total_tests}")
    print(f"Passed tests:    {passed_tests}")
    print(f"Failed tests:    {failed_tests}")
    print(f"Pass rate:       {passed_tests/total_tests*100:.2f}%")
    print(f"\nDetailed report written to intrv_validation_report.txt")

if __name__ == "__main__":
    main()
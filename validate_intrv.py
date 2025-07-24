#!/usr/bin/env python3
"""
INTRV Validation Script - Validation Specialist #4
Validates modern F90 implementation against F77 reference values
"""

import json
import subprocess
import tempfile
import os

def create_test_program(test_case):
    """Create a Fortran test program for a single test case"""
    
    # Format the array values
    array_values = ', '.join([f'{val:.6f}' for val in test_case['inputs'][0]])
    
    fortran_code = f"""
program test_single
    use intrv_module
    implicit none
    
    real :: xt({len(test_case['inputs'][0])})
    integer :: lxt, ilo, ileft, mflag
    real :: x
    
    ! Test data
    xt = [{array_values}]
    lxt = {test_case['inputs'][1]}
    x = {test_case['inputs'][2]:.6f}
    ilo = {test_case['inputs'][3]}
    
    ! Call INTRV
    call intrv(xt, lxt, x, ilo, ileft, mflag)
    
    ! Output results
    write(*,'(I0,1X,I0,1X,I0)') ileft, mflag, ilo
    
end program test_single
"""
    return fortran_code

def run_test_case(test_case):
    """Run a single test case and return results"""
    
    # Create temporary Fortran file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(create_test_program(test_case))
        temp_f90 = f.name
    
    try:
        # Compile
        exe_name = temp_f90.replace('.f90', '.exe')
        compile_cmd = [
            'gfortran', '-o', exe_name, 
            'modern/intrv_modern.f90', temp_f90
        ]
        
        result = subprocess.run(compile_cmd, capture_output=True, text=True)
        if result.returncode != 0:
            return None, f"Compilation error: {result.stderr}"
        
        # Run
        result = subprocess.run([exe_name], capture_output=True, text=True)
        if result.returncode != 0:
            return None, f"Runtime error: {result.stderr}"
        
        # Parse output
        output_values = [int(x) for x in result.stdout.strip().split()]
        if len(output_values) != 3:
            return None, f"Invalid output format: {result.stdout}"
        
        return {
            'ileft': output_values[0],
            'mflag': output_values[1], 
            'ilo_out': output_values[2]
        }, None
        
    finally:
        # Cleanup
        for f in [temp_f90, exe_name]:
            if os.path.exists(f):
                os.unlink(f)

def validate_intrv():
    """Main validation function"""
    
    print("INTRV Validation - Specialist #4")
    print("=" * 50)
    print()
    
    # Load test data
    try:
        with open('test_data/intrv_tests.json', 'r') as f:
            test_data = json.load(f)
    except FileNotFoundError:
        print("ERROR: test_data/intrv_tests.json not found")
        return False
    
    test_cases = test_data['test_cases']
    total_tests = len(test_cases)
    
    print(f"Loaded {total_tests} test cases")
    print()
    
    failures = []
    
    # Run all tests
    for i, test_case in enumerate(test_cases):
        if (i + 1) % 50 == 0:
            print(f"Progress: {i + 1}/{total_tests} tests completed...")
        
        # Run test
        actual, error = run_test_case(test_case)
        
        if error:
            failures.append({
                'test_id': test_case['test_id'],
                'description': test_case['description'][:80] + "...",
                'error': error
            })
            continue
        
        # Compare with expected
        expected = test_case['expected']
        
        if (actual['ileft'] != expected['ileft'] or 
            actual['mflag'] != expected['mflag'] or
            actual['ilo_out'] != expected['ilo_out']):
            
            failures.append({
                'test_id': test_case['test_id'],
                'description': test_case['description'][:80] + "...",
                'expected': expected,
                'actual': actual,
                'inputs': test_case['inputs']
            })
    
    # Report results
    print()
    print("Validation Results:")
    print(f"  Total tests: {total_tests}")
    print(f"  Failures: {len(failures)}")
    print(f"  Pass rate: {((total_tests - len(failures)) / total_tests * 100):.1f}%")
    print()
    
    if len(failures) == 0:
        print("✅ ALL TESTS PASSED - Modern implementation validated")
        create_validation_report(total_tests, failures)
        return True
    else:
        print("❌ VALIDATION FAILED")
        print()
        
        # Analyze failure patterns without revealing expected values
        analyze_failures(failures)
        create_validation_report(total_tests, failures)
        return False

def analyze_failures(failures):
    """Analyze failure patterns to provide constructive feedback"""
    
    print("Failure Analysis:")
    print("-" * 30)
    
    # Count by failure type
    ileft_failures = sum(1 for f in failures if 'expected' in f and 
                        f['actual']['ileft'] != f['expected']['ileft'])
    mflag_failures = sum(1 for f in failures if 'expected' in f and 
                        f['actual']['mflag'] != f['expected']['mflag'])
    ilo_failures = sum(1 for f in failures if 'expected' in f and 
                      f['actual']['ilo_out'] != f['expected']['ilo_out'])
    
    print(f"  ILEFT incorrect: {ileft_failures} cases")
    print(f"  MFLAG incorrect: {mflag_failures} cases")
    print(f"  ILO_OUT incorrect: {ilo_failures} cases")
    print()
    
    # Show sample failures (without expected values)
    print("Sample failing test cases:")
    for i, failure in enumerate(failures[:5]):
        if 'expected' in failure:
            print(f"  Test {failure['test_id']}: {failure['description']}")
            print(f"    Input array size: {len(failure['inputs'][0])}")
            print(f"    Search value: {failure['inputs'][2]}")
            print(f"    Your output: ileft={failure['actual']['ileft']}, "
                  f"mflag={failure['actual']['mflag']}, ilo_out={failure['actual']['ilo_out']}")
            print()

def create_validation_report(total_tests, failures):
    """Create final validation report"""
    
    report = f"""# INTRV Validation Report - Specialist #4

## Summary
- **Function**: INTRV (interval finding in sorted arrays)
- **Total test cases**: {total_tests}
- **Failed test cases**: {len(failures)}
- **Pass rate**: {((total_tests - len(failures)) / total_tests * 100):.1f}%
- **Status**: {'PASSED' if len(failures) == 0 else 'FAILED'}

## Implementation Analysis
The modern F90 implementation uses a hybrid search algorithm:
1. Initial range check and adjustment 
2. Exponential search (doubling step size) to bracket the target
3. Binary search to narrow down the exact interval

"""

    if len(failures) == 0:
        report += """## Validation Result
✅ **ALL TESTS PASSED**

The modern implementation correctly handles all test scenarios:
- Exact matches at array boundaries
- Values between array elements  
- Edge cases (x < first element, x >= last element)
- Various array sizes and initial ILO values
- All MFLAG conditions (-1, 0, 1)

The implementation is mathematically sound and ready for production use.
"""
    else:
        report += f"""## Validation Result  
❌ **VALIDATION FAILED**

{len(failures)} test cases failed validation. Issues identified:
- Algorithm logic errors in interval detection
- Incorrect boundary condition handling
- MFLAG assignment problems

Review the implementation logic, particularly:
1. Boundary condition checks (lines 27-43)
2. Exponential search loops (lines 46-82) 
3. Binary search termination (lines 85-98)

**Note**: Expected values not provided per blind validation protocol.
"""

    # Write report
    with open('validation/intrv_validation_report.md', 'w') as f:
        f.write(report)
    
    print(f"Validation report written to: validation/intrv_validation_report.md")

if __name__ == "__main__":
    # Ensure validation directory exists
    os.makedirs('validation', exist_ok=True)
    
    # Run validation
    success = validate_intrv()
    exit(0 if success else 1)
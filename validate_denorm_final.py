#!/usr/bin/env python3
"""
Final validation of DENORM modern implementation against F77 reference.
Processes all 257 enhanced test cases with proper numerical tolerance.
"""

import json
import subprocess
import tempfile
import os
import math
import sys

def load_test_data():
    """Load the enhanced test cases."""
    test_file = "test_data/denorm_tests_enhanced.json"
    with open(test_file, 'r') as f:
        return json.load(f)

def generate_f77_reference(test_data):
    """Generate F77 reference implementation and compute all test results."""
    
    # Create F77 test program
    f77_program = '''
      PROGRAM DENORM_REFERENCE
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAX_N = 1000
      DOUBLE PRECISION X(MAX_N)
      DOUBLE PRECISION DENORM, RESULT
      INTEGER N, I, TEST_ID
      CHARACTER*256 LINE
      
C     Read test cases from stdin and compute DENORM values
 10   CONTINUE
      READ(*, '(A)', END=999) LINE
      IF (LINE(1:4) .EQ. 'TEST') THEN
          READ(LINE(5:), *) TEST_ID, N
          READ(*, *) (X(I), I=1,N)
          RESULT = DENORM(N, X)
          WRITE(*, '(I0, 1X, ES25.16E3)') TEST_ID, RESULT
      END IF
      GO TO 10
 999  CONTINUE
      END

C     Original F77 DENORM implementation
      DOUBLE PRECISION FUNCTION DENORM(N,X)
      INTEGER N
      DOUBLE PRECISION X(*)
      INTEGER I
      DOUBLE PRECISION AGIANT,FLOATN,ONE,RDWARF,RGIANT,S1,S2,S3,
     *                 X1MAX,X3MAX,XABS,ZERO
      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT/FLOATN
      DO 90 I = 1, N
         XABS = DABS(X(I))
         IF (XABS .GE. AGIANT) GO TO 70
         IF (XABS .GT. RDWARF) GO TO 30
         IF (XABS .LE. X3MAX) GO TO 10
         S3 = ONE + S3*(X3MAX/XABS)**2
         X3MAX = XABS
         GO TO 90
   10    CONTINUE
         IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2
         GO TO 90
   30    CONTINUE
         S2 = S2 + XABS**2
         GO TO 90
   70    CONTINUE
         IF (XABS .LE. X1MAX) GO TO 80
         S1 = ONE + S1*(X1MAX/XABS)**2
         X1MAX = XABS
         GO TO 90
   80    CONTINUE
         S1 = S1 + (XABS/X1MAX)**2
   90    CONTINUE
      IF (S1 .EQ. ZERO) GO TO 100
      DENORM = X1MAX*DSQRT(S1+(S2/X1MAX)/X1MAX)
      RETURN
  100 CONTINUE
      IF (S2 .EQ. ZERO) GO TO 110
      IF (S2 .GE. X3MAX)
     *   DENORM = DSQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
      IF (S2 .LT. X3MAX)
     *   DENORM = DSQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
      RETURN
  110 CONTINUE
      DENORM = X3MAX*DSQRT(S3)
      RETURN
      END
'''
    
    # Write F77 program to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f', delete=False) as f:
        f.write(f77_program)
        f77_file = f.name
    
    try:
        # Compile F77 program
        exe_file = f77_file.replace('.f', '')
        compile_result = subprocess.run(
            ['gfortran', '-o', exe_file, f77_file],
            capture_output=True, text=True
        )
        
        if compile_result.returncode != 0:
            print(f"F77 compilation failed: {compile_result.stderr}")
            return None
        
        # Prepare input for F77 program
        input_lines = []
        for test_case in test_data['test_cases']:
            test_id = test_case['test_id']
            n = test_case['n']
            inputs = test_case['inputs']
            
            input_lines.append(f"TEST {test_id} {n}")
            input_lines.append(' '.join(str(x) for x in inputs))
        
        input_text = '\n'.join(input_lines)
        
        # Run F77 program
        run_result = subprocess.run(
            [exe_file],
            input=input_text,
            capture_output=True, text=True
        )
        
        if run_result.returncode != 0:
            print(f"F77 execution failed: {run_result.stderr}")
            return None
        
        # Parse results
        results = {}
        for line in run_result.stdout.strip().split('\n'):
            if line.strip():
                parts = line.strip().split()
                test_id = int(parts[0])
                value = float(parts[1])
                results[test_id] = value
        
        return results
        
    finally:
        # Clean up temporary files
        for f in [f77_file, exe_file]:
            if os.path.exists(f):
                os.unlink(f)

def test_modern_implementation(test_data):
    """Test the modern F90 implementation."""
    
    # Create F90 test program
    f90_program = '''
program test_denorm_modern
    use denorm_module
    implicit none
    
    integer, parameter :: max_n = 1000
    double precision :: x(max_n)
    double precision :: result
    integer :: n, i, test_id
    character(256) :: line
    
    ! Read test cases from stdin and compute DENORM values
    do
        read(*, '(A)', iostat=i) line
        if (i /= 0) exit
        
        if (line(1:4) == 'TEST') then
            read(line(5:), *) test_id, n
            read(*, *) (x(i), i=1,n)
            result = denorm(n, x)
            write(*, '(I0, 1X, ES25.16E3)') test_id, result
        end if
    end do
end program test_denorm_modern
'''
    
    # Write F90 program to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
        f.write(f90_program)
        f90_file = f.name
    
    try:
        # Compile F90 program
        exe_file = f90_file.replace('.f90', '')
        compile_result = subprocess.run(
            ['gfortran', '-o', exe_file, f90_file, 'modern/denorm_modern.f90'],
            capture_output=True, text=True
        )
        
        if compile_result.returncode != 0:
            print(f"F90 compilation failed: {compile_result.stderr}")
            return None
        
        # Prepare input for F90 program
        input_lines = []
        for test_case in test_data['test_cases']:
            test_id = test_case['test_id']
            n = test_case['n']
            inputs = test_case['inputs']
            
            input_lines.append(f"TEST {test_id} {n}")
            input_lines.append(' '.join(str(x) for x in inputs))
        
        input_text = '\n'.join(input_lines)
        
        # Run F90 program
        run_result = subprocess.run(
            [exe_file],
            input=input_text,
            capture_output=True, text=True
        )
        
        if run_result.returncode != 0:
            print(f"F90 execution failed: {run_result.stderr}")
            return None
        
        # Parse results
        results = {}
        for line in run_result.stdout.strip().split('\n'):
            if line.strip():
                parts = line.strip().split()
                test_id = int(parts[0])
                value = float(parts[1])
                results[test_id] = value
        
        return results
        
    finally:
        # Clean up temporary files
        for f in [f90_file, exe_file]:
            if os.path.exists(f):
                os.unlink(f)

def compare_results(test_data, f77_results, f90_results, tolerance=1e-10):
    """Compare F77 and F90 results with numerical tolerance."""
    
    total_tests = len(test_data['test_cases'])
    passed = 0
    failed = 0
    failures = []
    
    print(f"Validating {total_tests} test cases with tolerance {tolerance}")
    print("=" * 80)
    
    for test_case in test_data['test_cases']:
        test_id = test_case['test_id']
        description = test_case.get('description', f'Test {test_id}')
        
        if test_id not in f77_results or test_id not in f90_results:
            print(f"Test {test_id}: MISSING RESULTS")
            failed += 1
            failures.append({
                'test_id': test_id,
                'description': description,
                'reason': 'Missing results'
            })
            continue
        
        f77_val = f77_results[test_id]
        f90_val = f90_results[test_id]
        
        # Handle special cases
        if math.isnan(f77_val) and math.isnan(f90_val):
            passed += 1
            continue
        
        if math.isinf(f77_val) and math.isinf(f90_val):
            if (f77_val > 0) == (f90_val > 0):  # Same sign infinity
                passed += 1
                continue
        
        # Compute relative error
        if f77_val == 0.0 and f90_val == 0.0:
            rel_error = 0.0
        elif f77_val == 0.0:
            rel_error = abs(f90_val)
        else:
            rel_error = abs((f90_val - f77_val) / f77_val)
        
        if rel_error <= tolerance:
            passed += 1
            # Special attention to test 168 (infinity input)
            if test_id == 168:
                print(f"Test {test_id} (INFINITY): PASSED - F77: {f77_val}, F90: {f90_val}, rel_error: {rel_error}")
        else:
            failed += 1
            failures.append({
                'test_id': test_id,
                'description': description,
                'f77_value': f77_val,
                'f90_value': f90_val,
                'rel_error': rel_error,
                'reason': f'Relative error {rel_error} > {tolerance}'
            })
            
            # Print details for failed tests
            print(f"Test {test_id}: FAILED")
            print(f"  Description: {description}")
            print(f"  F77 result: {f77_val}")
            print(f"  F90 result: {f90_val}")
            print(f"  Relative error: {rel_error}")
            print()
    
    # Summary
    print("=" * 80)
    print(f"VALIDATION SUMMARY:")
    print(f"Total tests: {total_tests}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Pass rate: {passed/total_tests*100:.2f}%")
    
    if failed == 0:
        print("\n🎉 SUCCESS: 100% compatibility with F77 reference implementation!")
    else:
        print(f"\n❌ FAILURES: {failed} tests failed")
        print("\nFailed test summary:")
        for failure in failures[:10]:  # Show first 10 failures
            print(f"  Test {failure['test_id']}: {failure['reason']}")
        if len(failures) > 10:
            print(f"  ... and {len(failures) - 10} more failures")
    
    return passed == total_tests, failures

def main():
    """Main validation function."""
    print("DENORM Final Validation")
    print("=" * 50)
    
    # Load test data
    print("Loading enhanced test cases...")
    test_data = load_test_data()
    total_tests = test_data['total_tests']
    print(f"Loaded {total_tests} test cases")
    
    # Generate F77 reference values
    print("\nGenerating F77 reference values...")
    f77_results = generate_f77_reference(test_data)
    if f77_results is None:
        print("Failed to generate F77 reference values")
        return False
    print(f"Generated {len(f77_results)} F77 reference values")
    
    # Test modern implementation
    print("\nTesting modern F90 implementation...")
    f90_results = test_modern_implementation(test_data)
    if f90_results is None:
        print("Failed to test F90 implementation")
        return False
    print(f"Generated {len(f90_results)} F90 test values")
    
    # Compare results
    print("\nComparing results...")
    success, failures = compare_results(test_data, f77_results, f90_results)
    
    # Save detailed results
    validation_results = {
        'total_tests': total_tests,
        'passed': total_tests - len(failures),
        'failed': len(failures),
        'pass_rate': (total_tests - len(failures)) / total_tests,
        'tolerance': 1e-10,
        'failures': failures,
        'f77_results': f77_results,
        'f90_results': f90_results
    }
    
    with open('denorm_final_validation_results.json', 'w') as f:
        json.dump(validation_results, f, indent=2)
    
    print(f"\nDetailed results saved to: denorm_final_validation_results.json")
    
    return success

if __name__ == '__main__':
    success = main()
    sys.exit(0 if success else 1)
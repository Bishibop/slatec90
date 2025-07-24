#!/usr/bin/env python3
"""
Batch validation of INTRV modern implementation
"""

import json
import subprocess
import sys

def main():
    # Compile the batch tester
    print("Compiling batch tester...")
    compile_result = subprocess.run(
        ['gfortran', '-o', 'test_intrv_batch', 'modern/intrv_modern.f90', 'test_intrv_batch.f90'],
        capture_output=True, text=True
    )
    
    if compile_result.returncode != 0:
        print(f"Compilation error: {compile_result.stderr}")
        return
    
    # Load test data
    print("Loading test data...")
    with open('test_data/intrv_tests.json', 'r') as f:
        data = json.load(f)
    
    # Prepare input for batch tester
    test_input = []
    for test_case in data['test_cases']:
        test_id = test_case['test_id']
        xt_array = test_case['inputs'][0]
        lxt = test_case['inputs'][1]
        x = test_case['inputs'][2]
        ilo_in = test_case['inputs'][3]
        
        # Format test input
        test_input.append(str(test_id))
        test_input.append(str(lxt))
        for i in range(lxt):
            test_input.append(f"{xt_array[i]:e}")
        test_input.append(f"{x:e} {ilo_in}")
    
    # Run batch test
    print("Running batch tests...")
    process = subprocess.Popen(
        ['./test_intrv_batch'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    
    output, error = process.communicate(input='\n'.join(test_input))
    
    if process.returncode != 0:
        print(f"Runtime error: {error}")
        return
    
    # Parse results
    results = {}
    for line in output.strip().split('\n'):
        if line:
            parts = line.split(',')
            test_id = int(parts[0])
            ileft = int(parts[1])
            mflag = int(parts[2])
            ilo_out = int(parts[3])
            results[test_id] = (ileft, mflag, ilo_out)
    
    # Analyze results
    print("\nAnalyzing results...")
    total_tests = len(data['test_cases'])
    passed_tests = 0
    failed_tests = 0
    
    # Track failures
    extreme_value_failures = []
    boundary_failures = []
    pattern_4_0_4_failures = []
    other_failures = []
    
    # Special tests
    special_tests = [254, 255, 257]
    
    for test_case in data['test_cases']:
        test_id = test_case['test_id']
        
        if test_id not in results:
            print(f"Test {test_id}: NO RESULT")
            failed_tests += 1
            continue
        
        expected_ileft = test_case['expected']['ileft']
        expected_mflag = test_case['expected']['mflag']
        expected_ilo = test_case['expected']['ilo_out']
        
        ileft, mflag, ilo_out = results[test_id]
        
        if ileft == expected_ileft and mflag == expected_mflag and ilo_out == expected_ilo:
            passed_tests += 1
        else:
            failed_tests += 1
            
            # Check for the suspicious pattern
            if ileft == 4 and mflag == 0 and ilo_out == 4:
                pattern_4_0_4_failures.append(test_id)
            
            # Check if it's extreme value related
            xt_array = test_case['inputs'][0]
            lxt = test_case['inputs'][1]
            x = test_case['inputs'][2]
            
            is_extreme = False
            for val in xt_array[:lxt]:
                if val != 0 and (abs(val) < 1e-20 or abs(val) > 1e20):
                    is_extreme = True
                    break
            if x != 0 and (abs(x) < 1e-20 or abs(x) > 1e20):
                is_extreme = True
            
            if is_extreme:
                extreme_value_failures.append(test_id)
            elif x < xt_array[0] or x >= xt_array[lxt-1]:
                boundary_failures.append(test_id)
            else:
                other_failures.append(test_id)
            
            # Report special test failures
            if test_id in special_tests:
                print(f"\nSpecial Test {test_id}: FAILED")
                print(f"  Description: {test_case['description']}")
                print(f"  Array: {xt_array[:lxt]}")
                print(f"  X value: {x}")
                print(f"  Expected: ILEFT={expected_ileft}, MFLAG={expected_mflag}, ILO={expected_ilo}")
                print(f"  Got:      ILEFT={ileft}, MFLAG={mflag}, ILO={ilo_out}")
    
    # Write report
    with open('INTRV_VALIDATION_REPORT.md', 'w') as f:
        f.write("# INTRV Modern Implementation Validation Report\n\n")
        
        f.write("## Summary Statistics\n\n")
        f.write(f"- **Total tests**: {total_tests}\n")
        f.write(f"- **Passed tests**: {passed_tests}\n")
        f.write(f"- **Failed tests**: {failed_tests}\n")
        f.write(f"- **Pass rate**: {passed_tests/total_tests*100:.2f}%\n\n")
        
        f.write("## Failure Analysis\n\n")
        f.write(f"- **Extreme value failures**: {len(extreme_value_failures)}\n")
        f.write(f"- **Boundary condition failures**: {len(boundary_failures)}\n")
        f.write(f"- **Pattern (4,0,4) failures**: {len(pattern_4_0_4_failures)}\n")
        f.write(f"- **Other failures**: {len(other_failures)}\n\n")
        
        if pattern_4_0_4_failures:
            f.write("## Suspicious Pattern (4,0,4) Analysis\n\n")
            f.write(f"Found {len(pattern_4_0_4_failures)} tests returning ILEFT=4, MFLAG=0, ILO=4:\n\n")
            
            # Analyze first few
            for test_id in pattern_4_0_4_failures[:5]:
                test = data['test_cases'][test_id-1]
                f.write(f"### Test {test_id}\n")
                f.write(f"- Array size: {test['inputs'][1]}\n")
                f.write(f"- Initial ILO: {test['inputs'][3]}\n")
                f.write(f"- X value: {test['inputs'][2]}\n")
                f.write(f"- Expected: ILEFT={test['expected']['ileft']}, ")
                f.write(f"MFLAG={test['expected']['mflag']}, ILO={test['expected']['ilo_out']}\n\n")
        
        if extreme_value_failures:
            f.write("## Extreme Value Test Failures\n\n")
            f.write("Tests involving very small (< 1e-20) or very large (> 1e20) values:\n\n")
            
            # Sample a few
            for test_id in extreme_value_failures[:5]:
                test = data['test_cases'][test_id-1]
                f.write(f"- Test {test_id}: {test['description']}\n")
        
        f.write("\n## Algorithmic Guidance\n\n")
        
        if pattern_4_0_4_failures:
            f.write("### Pattern (4,0,4) Issue\n\n")
            f.write("The consistent return of ILEFT=4, MFLAG=0, ILO=4 suggests:\n\n")
            f.write("1. **Hardcoded return value**: Check if there's a code path that always returns 4\n")
            f.write("2. **Uninitialized variable**: The value 4 might be from uninitialized memory\n")
            f.write("3. **Array bounds issue**: Check if the algorithm incorrectly handles certain array sizes\n\n")
        
        if extreme_value_failures:
            f.write("### Extreme Value Handling\n\n")
            f.write("For extreme floating-point values:\n\n")
            f.write("1. **Use proper comparisons**: Ensure `x >= xt(i)` instead of `x .ge. xt(i)`\n")
            f.write("2. **Avoid equality tests**: Never use `==` for floating-point comparisons\n")
            f.write("3. **Consider denormalized numbers**: Values near zero need special care\n")
            f.write("4. **Check for overflow**: Ensure index calculations don't overflow\n\n")
        
        f.write("### Recommended Fixes\n\n")
        f.write("1. **Review binary search logic**: Ensure proper interval narrowing\n")
        f.write("2. **Check edge cases**: Verify behavior at array boundaries\n")
        f.write("3. **Validate index calculations**: Ensure `middle = (ilo + ihi) / 2` doesn't overflow\n")
        f.write("4. **Test return paths**: Verify all code paths set correct values\n")
    
    # Console output
    print(f"\n{'='*60}")
    print(f"INTRV Validation Complete")
    print(f"{'='*60}")
    print(f"Total tests:     {total_tests}")
    print(f"Passed tests:    {passed_tests}")
    print(f"Failed tests:    {failed_tests}")
    print(f"Pass rate:       {passed_tests/total_tests*100:.2f}%")
    print(f"\nFailure breakdown:")
    print(f"  Extreme values:  {len(extreme_value_failures)}")
    print(f"  Boundary:        {len(boundary_failures)}")
    print(f"  Pattern (4,0,4): {len(pattern_4_0_4_failures)}")
    print(f"  Other:           {len(other_failures)}")
    print(f"\nDetailed report written to INTRV_VALIDATION_REPORT.md")

if __name__ == "__main__":
    main()
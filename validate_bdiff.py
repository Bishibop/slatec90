#!/usr/bin/env python3
"""
BDIFF Validation Script - Specialist #5
Compares modern F90 implementation against F77 reference values
Maintains blind testing integrity by not revealing expected values
"""

import json
import sys
import os

def validate_bdiff():
    """Validate BDIFF modern implementation against F77 reference"""
    
    # Load reference and modern outputs
    try:
        with open('test_data/bdiff_tests.json', 'r') as f:
            reference_data = json.load(f)
    except FileNotFoundError:
        print("❌ ERROR: Reference file test_data/bdiff_tests.json not found")
        return False
    except json.JSONDecodeError as e:
        print(f"❌ ERROR: Failed to parse reference JSON: {e}")
        return False
        
    try:
        with open('bdiff_outputs.json', 'r') as f:
            modern_data = json.load(f)
    except FileNotFoundError:
        print("❌ ERROR: Modern outputs file bdiff_outputs.json not found")
        return False
    except json.JSONDecodeError as e:
        print(f"❌ ERROR: Failed to parse modern outputs JSON: {e}")
        return False
    
    # Validation parameters
    tolerance = 1e-6  # Relative tolerance for numerical comparison
    total_tests = 262
    passed = 0
    failed = 0
    failures = []
    
    print(f"🔍 Validating BDIFF modern implementation against {total_tests} F77 reference test cases...")
    print(f"📏 Using relative tolerance: {tolerance}")
    print("="*70)
    
    # Extract reference and modern test results
    ref_tests = {test['test_id']: test for test in reference_data.get('test_cases', [])}
    mod_tests = {test['test_id']: test['output'] for test in modern_data.get('outputs', [])}
    
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
        mod_output = mod_tests[test_id]
        
        # Extract expected value and L parameter
        expected_value = ref_test['expected']
        L = ref_test['L']
        
        # BDIFF puts the final result in V(L), which is the L-th element (1-indexed)
        # So we need the (L-1)th element in 0-indexed array
        if len(mod_output) < L:
            print(f"❌ Test {test_id}: Modern output too short (L={L}, got {len(mod_output)} elements)")
            failed += 1
            failures.append(f"Test {test_id}: Output array too short")
            continue
            
        # Compare the expected value with V(L) from modern output
        mod_result = mod_output[L-1]  # L-th element (1-indexed) = (L-1)th in 0-indexed
        
        # Handle different numeric representations
        try:
            expected_value = float(expected_value)
            mod_result = float(mod_result)
        except (ValueError, TypeError):
            print(f"❌ Test {test_id}: Non-numeric values (expected: {expected_value}, got: {mod_result})")
            failed += 1
            failures.append(f"Test {test_id}: Non-numeric values")
            continue
            
        # Check for exact zero matches
        if expected_value == 0.0 and mod_result == 0.0:
            test_passed = True
        elif not (abs(expected_value) < float('inf') and abs(mod_result) < float('inf')):
            # Check for infinite or NaN values
            if expected_value != mod_result:  # Both should be inf/nan in same way
                print(f"❌ Test {test_id}: Infinite/NaN mismatch (expected: {expected_value}, got: {mod_result})")
                test_passed = False
            else:
                test_passed = True
        else:
            # Relative error comparison
            if expected_value == 0.0:
                error = abs(mod_result)
            else:
                error = abs((mod_result - expected_value) / expected_value)
                
            if error > tolerance:
                print(f"❌ Test {test_id}: Precision error (expected: {expected_value}, got: {mod_result}, relative error: {error:.2e})")
                test_passed = False
            else:
                test_passed = True
        
        if test_passed:
            passed += 1
        else:
            failed += 1
            failures.append(f"Test {test_id}: Numerical accuracy issues")
    
    # Summary and analysis
    print("="*70)
    print(f"📊 VALIDATION SUMMARY:")
    print(f"   ✅ Passed: {passed}/{total_tests} ({100*passed/total_tests:.1f}%)")
    print(f"   ❌ Failed: {failed}/{total_tests} ({100*failed/total_tests:.1f}%)")
    
    if failed == 0:
        print("\n🎉 VALIDATION SUCCESSFUL!")
        print("   All test cases pass within specified tolerance.")
        print("   Modern implementation matches F77 reference behavior.")
        return True
    else:
        print(f"\n⚠️  VALIDATION FAILED - {failed} test cases need attention")
        
        # Analyze failure patterns
        print("\n🔍 FAILURE ANALYSIS:")
        
        # Pattern detection
        small_value_failures = 0
        large_value_failures = 0
        precision_failures = 0
        structural_failures = 0
        
        for failure in failures:
            if "length mismatch" in failure or "Missing" in failure:
                structural_failures += 1
            elif "Precision error" in failure or "accuracy" in failure:
                precision_failures += 1
                # Could analyze which ranges of test IDs are failing
                test_num = int(failure.split()[1].rstrip(':'))
                if test_num <= 60:  # Small value tests typically in early range
                    small_value_failures += 1
                else:
                    large_value_failures += 1
        
        if structural_failures > 0:
            print(f"   🏗️  Structural issues: {structural_failures} tests")
            print("      → Check algorithm logic and array handling")
            
        if precision_failures > 0:
            print(f"   🔢 Numerical precision issues: {precision_failures} tests")
            
            if small_value_failures > large_value_failures:
                print("      → Issues concentrated in small value range")
                print("      → Check underflow handling and zero comparisons")
            elif large_value_failures > small_value_failures:
                print("      → Issues concentrated in large value range")  
                print("      → Check overflow handling and scaling")
            else:
                print("      → Issues distributed across value ranges")
                print("      → Review core algorithm numerical stability")
        
        # Guidance without revealing expected values
        print("\n💡 RECOMMENDATIONS:")
        print("   1. Review backward difference computation algorithm")
        print("   2. Check binomial coefficient calculation accuracy")
        print("   3. Verify loop indices and array indexing")
        print("   4. Test with edge cases: L=1, zeros, alternating signs")
        print("   5. Ensure proper handling of truncated sums")
        
        return False

if __name__ == "__main__":
    success = validate_bdiff()
    sys.exit(0 if success else 1)
#!/usr/bin/env python3
"""
Validate ZABS blind implementation against reference values.
"""

import json
import math

def load_reference_tests(filename):
    """Load reference test cases."""
    with open(filename, 'r') as f:
        data = json.load(f)
    return data['test_cases']

def load_blind_results(filename):
    """Load blind implementation results."""
    with open(filename, 'r') as f:
        data = json.load(f)
    return data['results']

def relative_error(expected, actual):
    """Calculate relative error."""
    if expected == 0.0:
        return abs(actual)
    return abs((actual - expected) / expected)

def validate_results(reference_tests, blind_results, tolerance=1e-14):
    """Compare blind results against reference values."""
    passed = 0
    failed = 0
    failures = []
    
    # Create a mapping of test descriptions to results
    blind_map = {r['description']: r for r in blind_results}
    
    for ref_test in reference_tests:
        desc = ref_test['description']
        expected = ref_test['expected']
        test_id = ref_test['test_id']
        ref_inputs = ref_test['inputs']
        
        if desc not in blind_map:
            failed += 1
            failures.append({
                'test_id': test_id,
                'description': desc,
                'inputs': ref_inputs,
                'expected': expected,
                'error': 'Test case not found in blind results'
            })
            continue
            
        blind_test = blind_map[desc]
        actual = blind_test['result']
        
        # Check if inputs match (sanity check)
        blind_inputs = blind_test['inputs']
        
        if abs(ref_inputs[0] - blind_inputs[0]) > 1e-15 or abs(ref_inputs[1] - blind_inputs[1]) > 1e-15:
            failed += 1
            failures.append({
                'test_id': test_id,
                'description': desc,
                'error': f'Input mismatch: ref={ref_inputs}, blind={blind_inputs}'
            })
            continue
        
        # Compare results
        error = relative_error(expected, actual)
        
        if error > tolerance:
            failed += 1
            failures.append({
                'test_id': test_id,
                'description': desc,
                'inputs': ref_inputs,
                'expected': expected,
                'actual': actual,
                'relative_error': error,
                'error_type': analyze_failure(ref_inputs, expected, actual)
            })
        else:
            passed += 1
    
    return passed, failed, failures

def analyze_failure(inputs, expected, actual):
    """Analyze the type of failure without revealing exact values."""
    zr, zi = inputs
    
    # Check for special cases
    if zr == 0.0 and zi == 0.0:
        return "Zero handling issue"
    
    if zr == 0.0:
        return "Pure imaginary handling"
    
    if zi == 0.0:
        return "Pure real handling"
    
    # Check magnitude ranges
    max_mag = max(abs(zr), abs(zi))
    min_mag = min(abs(zr), abs(zi)) if abs(zr) > 0 and abs(zi) > 0 else 0
    
    # Check for subnormal/denormal range
    if max_mag < 2.225e-308 and max_mag > 0:
        return "Subnormal/denormal value handling"
    
    if max_mag < 1e-290:
        return "CDC underflow range (needs S*1.0D+0 scaling)"
    
    if max_mag < 1e-150:
        return "Very small value underflow"
    
    if max_mag > 1e+150:
        return "Large value overflow protection"
    
    # Check relative sizes
    if abs(zr) > 0 and abs(zi) > 0:
        ratio = max(abs(zr), abs(zi)) / min(abs(zr), abs(zi))
        if ratio > 1e+10:
            return "Large component ratio scaling"
    
    # Check if it's a scaled standard case
    if 'scaled by' in inputs:
        return "Scaled computation accuracy"
    
    return "General computation error"

def print_summary(passed, failed, failures):
    """Print validation summary."""
    total = passed + failed
    pass_rate = (passed / total * 100) if total > 0 else 0
    
    print(f"\nZABS Validation Summary")
    print(f"======================")
    print(f"Total tests: {total}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    print(f"Pass rate: {pass_rate:.2f}%")
    
    if failures:
        print(f"\nFailure Analysis")
        print(f"================")
        
        # First show simple listing
        for f in failures:
            if 'error' in f:
                print(f"Test {f['test_id']}: {f['description']} - {f['error']}")
                if 'inputs' in f:
                    print(f"  Inputs: {f['inputs']}")
            else:
                print(f"Test {f['test_id']}: {f['description']} - {f.get('error_type', 'Unknown error')}")
        
        # Group failures by error type
        error_types = {}
        for f in failures:
            if 'error_type' in f:
                error_type = f['error_type']
                if error_type not in error_types:
                    error_types[error_type] = []
                error_types[error_type].append(f)
        
        if error_types:
            print(f"\nGrouped by Error Type:")
            for error_type, cases in error_types.items():
                print(f"\n{error_type}: {len(cases)} failures")
                # Show a few examples without revealing exact values
                for i, case in enumerate(cases[:3]):
                    print(f"  - Test {case['test_id']}: {case['description']}")
                    if 'relative_error' in case:
                        print(f"    Relative error: {case['relative_error']:.2e}")
                if len(cases) > 3:
                    print(f"  ... and {len(cases) - 3} more")
    
    # Provide hints based on failure patterns
    if failures:
        print(f"\nHints for Implementation")
        print(f"=======================")
        
        # Check for systematic issues
        zero_failures = [f for f in failures if 'Zero handling' in f.get('error_type', '')]
        if zero_failures:
            print("- Check zero input handling: (0,0) should return exactly 0")
        
        underflow_failures = [f for f in failures if 'underflow' in f.get('error_type', '').lower()]
        if underflow_failures:
            print("- Review underflow protection, especially CDC underflow (scale by 1.0D+0)")
        
        overflow_failures = [f for f in failures if 'overflow' in f.get('error_type', '').lower()]
        if overflow_failures:
            print("- Check overflow protection scaling algorithm")
        
        ratio_failures = [f for f in failures if 'ratio' in f.get('error_type', '').lower()]
        if ratio_failures:
            print("- Verify handling of large component ratios (one component >> other)")
        
        # Check for patterns in magnitude ranges
        small_mag_failures = [f for f in failures if f.get('inputs') and max(abs(f['inputs'][0]), abs(f['inputs'][1])) < 1e-100]
        large_mag_failures = [f for f in failures if f.get('inputs') and max(abs(f['inputs'][0]), abs(f['inputs'][1])) > 1e+100]
        
        if small_mag_failures:
            print(f"- {len(small_mag_failures)} failures in small magnitude range (< 1e-100)")
        if large_mag_failures:
            print(f"- {len(large_mag_failures)} failures in large magnitude range (> 1e+100)")

def main():
    # Load data
    reference_tests = load_reference_tests('/Users/nicholasmullen/Code/gauntlet/slatec_test/test_data/zabs_tests.json')
    blind_results = load_blind_results('/Users/nicholasmullen/Code/gauntlet/slatec_test/modern/zabs_blind_results.json')
    
    # Validate
    passed, failed, failures = validate_results(reference_tests, blind_results)
    
    # Print summary
    print_summary(passed, failed, failures)
    
    # Return status code
    return 0 if failed == 0 else 1

if __name__ == '__main__':
    exit(main())
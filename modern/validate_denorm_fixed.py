#!/usr/bin/env python3
"""Validate DENORM implementation against F77 reference values."""

import json
import numpy as np

def load_test_data(filename):
    """Load test data from JSON file."""
    with open(filename, 'r') as f:
        return json.load(f)

def calculate_relative_error(expected, actual):
    """Calculate relative error between two values."""
    if expected == 0.0:
        return abs(actual) if actual != 0.0 else 0.0
    return abs((actual - expected) / expected)

def validate_results():
    """Compare implementation outputs with reference values."""
    # Load data
    reference_data = load_test_data('/Users/nicholasmullen/Code/gauntlet/slatec_test/test_data/denorm_tests.json')
    output_data = load_test_data('/Users/nicholasmullen/Code/gauntlet/slatec_test/modern/denorm_output_fixed_clean.json')
    
    # Extract values
    reference_results = []
    for test in reference_data['test_cases']:
        reference_results.append(test['expected'])
    
    implementation_results = output_data['results']
    
    # Validation parameters
    tolerance = 1e-10  # Relative error tolerance for double precision
    
    # Compare results
    passed = 0
    failed = 0
    max_error = 0.0
    failure_details = []
    
    print(f"Validating {len(reference_results)} DENORM test cases")
    print(f"Tolerance: {tolerance} (relative error)")
    print("=" * 70)
    
    for i, (expected, actual) in enumerate(zip(reference_results, implementation_results)):
        rel_error = calculate_relative_error(expected, actual)
        max_error = max(max_error, rel_error)
        
        if rel_error <= tolerance:
            passed += 1
        else:
            failed += 1
            # Store failure details without revealing expected values
            failure_details.append({
                'index': i,
                'relative_error': rel_error,
                'error_magnitude': f"{rel_error:.2e}"
            })
    
    # Report results
    total_tests = passed + failed
    pass_rate = (passed / total_tests) * 100 if total_tests > 0 else 0
    
    print(f"Test Results:")
    print(f"  Total tests: {total_tests}")
    print(f"  Passed: {passed}")
    print(f"  Failed: {failed}")
    print(f"  Pass rate: {pass_rate:.2f}%")
    print(f"  Maximum relative error: {max_error:.2e}")
    print()
    
    if failed > 0:
        print(f"Failed test indices and error magnitudes:")
        for failure in failure_details[:10]:  # Show first 10 failures
            print(f"  Test {failure['index']}: relative error = {failure['error_magnitude']}")
        if len(failure_details) > 10:
            print(f"  ... and {len(failure_details) - 10} more failures")
    
    # Analyze patterns if there are failures
    if failed > 0:
        print("\nFailure Analysis:")
        # Group by error magnitude
        error_ranges = {
            'small (< 1e-8)': 0,
            'medium (1e-8 to 1e-5)': 0,
            'large (> 1e-5)': 0
        }
        
        for failure in failure_details:
            error = failure['relative_error']
            if error < 1e-8:
                error_ranges['small (< 1e-8)'] += 1
            elif error < 1e-5:
                error_ranges['medium (1e-8 to 1e-5)'] += 1
            else:
                error_ranges['large (> 1e-5)'] += 1
        
        for range_name, count in error_ranges.items():
            if count > 0:
                print(f"  {range_name}: {count} failures")
    
    # Final verdict
    print("\n" + "=" * 70)
    if pass_rate == 100.0:
        print("✓ DENORM IMPLEMENTATION VALIDATED SUCCESSFULLY")
        print("  All tests passed within tolerance")
        print("  Implementation is ready for production use")
    else:
        print("✗ DENORM IMPLEMENTATION VALIDATION FAILED")
        print(f"  {failed} tests exceeded tolerance threshold")
        print("  Further investigation required")
    
    return pass_rate == 100.0

if __name__ == "__main__":
    success = validate_results()
    exit(0 if success else 1)
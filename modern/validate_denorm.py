#!/usr/bin/env python3
"""
Validate DENORM blind implementation against F77 reference values.
Uses relative error tolerance of 1e-10 for double precision.
"""

import json
import sys
from typing import List, Dict, Tuple

def load_json(filepath: str) -> dict:
    """Load JSON data from file."""
    with open(filepath, 'r') as f:
        content = f.read()
        # Fix Fortran formatting issues (trailing periods)
        content = content.replace('.,', '.0,')
        content = content.replace('.]', '.0]')
        return json.loads(content)

def calculate_relative_error(expected: float, actual: float) -> float:
    """Calculate relative error between expected and actual values."""
    if expected == 0.0:
        return abs(actual)
    return abs(actual - expected) / abs(expected)

def validate_results(reference_data: dict, blind_results: dict, tolerance: float = 1e-10) -> Tuple[int, int, List[Dict]]:
    """
    Compare blind implementation results with reference values.
    Returns: (passed_count, total_count, failures_list)
    """
    # Extract results from both datasets
    ref_tests = reference_data['test_cases']
    blind_values = blind_results['results']
    
    if len(ref_tests) != len(blind_values):
        print(f"Warning: Test count mismatch - Reference: {len(ref_tests)}, Blind: {len(blind_values)}")
    
    passed = 0
    failures = []
    
    # Compare each test
    for i, (ref_test, actual) in enumerate(zip(ref_tests, blind_values)):
        expected = ref_test['expected']
        rel_error = calculate_relative_error(expected, actual)
        
        if rel_error <= tolerance:
            passed += 1
        else:
            # Don't reveal expected values - just collect failure info
            failures.append({
                'test_id': ref_test.get('test_id', i+1),
                'description': ref_test.get('description', 'Unknown'),
                'n': ref_test.get('n', 'Unknown'),
                'relative_error': rel_error,
                'actual_value': actual
            })
    
    return passed, len(ref_tests), failures

def analyze_failure_patterns(failures: List[Dict]) -> Dict[str, List[Dict]]:
    """Analyze patterns in test failures without revealing expected values."""
    patterns = {
        'very_small_values': [],
        'very_large_values': [],
        'mixed_magnitudes': [],
        'edge_cases': [],
        'precision_loss': [],
        'other': []
    }
    
    for failure in failures:
        actual = failure['actual_value']
        n = failure['n']
        desc = failure['description'].lower()
        
        # Categorize based on actual values and descriptions
        if actual < 1e-10:
            patterns['very_small_values'].append(failure)
        elif actual > 1e+10:
            patterns['very_large_values'].append(failure)
        elif 'mixed' in desc or 'different magnitude' in desc:
            patterns['mixed_magnitudes'].append(failure)
        elif n == 0 or 'empty' in desc or 'zero' in desc:
            patterns['edge_cases'].append(failure)
        elif failure['relative_error'] < 1e-8:  # Small but above tolerance
            patterns['precision_loss'].append(failure)
        else:
            patterns['other'].append(failure)
    
    # Remove empty categories
    return {k: v for k, v in patterns.items() if v}

def print_validation_report(passed: int, total: int, failures: List[Dict], patterns: Dict[str, List[Dict]]):
    """Print validation report without revealing expected values."""
    print("DENORM Implementation Validation Report")
    print("=" * 50)
    print(f"\nOverall Results:")
    print(f"  Tests Passed: {passed}/{total} ({passed/total*100:.1f}%)")
    print(f"  Tests Failed: {len(failures)}/{total} ({len(failures)/total*100:.1f}%)")
    
    if failures:
        print(f"\nFailure Analysis:")
        print(f"  Total failures: {len(failures)}")
        print(f"  Tolerance used: 1e-10 (relative error)")
        
        print("\nFailure Patterns Detected:")
        for pattern, tests in patterns.items():
            if tests:
                print(f"\n  {pattern.replace('_', ' ').title()}: {len(tests)} failures")
                # Show some examples without expected values
                for i, test in enumerate(tests[:3]):  # Show up to 3 examples
                    print(f"    - Test {test['test_id']}: {test['description']}")
                    print(f"      N={test['n']}, Relative Error={test['relative_error']:.2e}")
                    if test['actual_value'] < 1e-15 or test['actual_value'] > 1e+15:
                        print(f"      Actual value in extreme range: {test['actual_value']:.2e}")
                if len(tests) > 3:
                    print(f"    ... and {len(tests)-3} more")
        
        # Provide hints based on patterns
        print("\nImplementation Hints:")
        if patterns.get('very_small_values'):
            print("  - Check handling of very small values (underflow protection)")
            print("  - Review RDWARF scaling and accumulation for tiny values")
        if patterns.get('very_large_values'):
            print("  - Check handling of very large values (overflow protection)")
            print("  - Review RGIANT scaling and accumulation")
        if patterns.get('mixed_magnitudes'):
            print("  - Check three-sum accumulation for mixed magnitude vectors")
            print("  - Ensure proper scaling between XBIG, X1MAX, X3MAX ranges")
        if patterns.get('edge_cases'):
            print("  - Review boundary conditions (N=0, all zeros)")
            print("  - Check initial value handling")
        if patterns.get('precision_loss'):
            print("  - Minor precision loss detected - check accumulation order")
            print("  - Review final norm calculation and scaling")
    else:
        print("\n✓ All tests passed! Implementation matches reference within tolerance.")

def main():
    # File paths
    reference_file = "/Users/nicholasmullen/Code/gauntlet/slatec_test/test_data/denorm_tests.json"
    blind_results_file = "/Users/nicholasmullen/Code/gauntlet/slatec_test/modern/denorm_output.json"
    
    # Load data
    print("Loading test data...")
    reference_data = load_json(reference_file)
    blind_results = load_json(blind_results_file)
    
    # Validate
    passed, total, failures = validate_results(reference_data, blind_results)
    
    # Analyze patterns
    patterns = analyze_failure_patterns(failures)
    
    # Report
    print_validation_report(passed, total, failures, patterns)
    
    # Exit code based on results
    sys.exit(0 if len(failures) == 0 else 1)

if __name__ == "__main__":
    main()
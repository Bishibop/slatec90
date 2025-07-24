#!/usr/bin/env python3
"""
Validate BSPLVN modern implementation against F77 reference values.
"""

import json
import re

def parse_results_file(filepath):
    """Parse the modern implementation results file."""
    results = {}
    with open(filepath, 'r') as f:
        for line in f:
            match = re.match(r'TEST_\s*(\d+)_RESULT:\s*(.*)', line.strip())
            if match:
                test_id = int(match.group(1))
                values_str = match.group(2)
                # Parse the floating point values
                # Handle cases where numbers are concatenated without spaces
                # Split on 'E+' or 'E-' patterns
                values = []
                # First split by spaces
                parts = values_str.split()
                for part in parts:
                    # Check if this part contains multiple numbers
                    if re.search(r'[+-]?\d+\.\d+E[+-]\d+[+-]\d+\.\d+E[+-]\d+', part):
                        # Split concatenated scientific notation numbers
                        # Find all scientific notation numbers in the string
                        numbers = re.findall(r'[+-]?\d+\.\d+E[+-]\d+', part)
                        for num in numbers:
                            values.append(float(num))
                    else:
                        values.append(float(part))
                results[test_id] = values
    return results

def relative_error(expected, actual):
    """Calculate relative error between expected and actual values."""
    if abs(expected) < 1e-15:
        # For very small expected values, use absolute error
        return abs(actual - expected)
    return abs((actual - expected) / expected)

def validate_results(reference_file, results_file, tolerance=1e-6):
    """Validate modern implementation against reference values."""
    # Load reference data
    with open(reference_file, 'r') as f:
        reference_data = json.load(f)
    
    # Parse modern results
    modern_results = parse_results_file(results_file)
    
    # Initialize counters
    total_tests = reference_data['total_tests']
    passed_tests = 0
    failed_tests = []
    
    # Category analysis
    failures_by_category = {
        'linear': [],
        'quadratic': [],
        'cubic': [],
        'quartic': [],
        'quintic': [],
        'non_uniform': [],
        'repeated_knots': [],
        'boundary': [],
        'other': []
    }
    
    # Validate each test case
    for i, test_case in enumerate(reference_data['test_cases']):
        try:
            test_id = test_case.get('test_id', i + 1)  # Use index+1 if test_id missing
            description = test_case['description']
            expected = test_case['expected']
        except KeyError as e:
            print(f"Error in test case {i}: missing key {e}")
            print(f"Test case keys: {list(test_case.keys())}")
            raise
        
        if test_id not in modern_results:
            failed_tests.append({
                'id': test_id,
                'description': description,
                'error': 'No results found'
            })
            continue
        
        actual = modern_results[test_id]
        
        # Skip if expected is None (incomplete test case)
        if expected is None:
            print(f"Warning: Test case {test_id} has no expected values, skipping")
            continue
        
        # Check if array sizes match
        if len(expected) != len(actual):
            failed_tests.append({
                'id': test_id,
                'description': description,
                'error': f'Array size mismatch: expected {len(expected)}, got {len(actual)}'
            })
            categorize_failure(description, test_id, failures_by_category)
            continue
        
        # Compare values
        test_passed = True
        max_error = 0.0
        error_index = -1
        
        for i, (exp, act) in enumerate(zip(expected, actual)):
            error = relative_error(exp, act)
            if error > max_error:
                max_error = error
                error_index = i
            
            if error > tolerance:
                test_passed = False
        
        if test_passed:
            passed_tests += 1
        else:
            failed_tests.append({
                'id': test_id,
                'description': description,
                'error': f'Max relative error {max_error:.2e} at index {error_index}'
            })
            categorize_failure(description, test_id, failures_by_category)
    
    return {
        'total_tests': total_tests,
        'passed': passed_tests,
        'failed': len(failed_tests),
        'pass_rate': passed_tests / total_tests * 100,
        'failed_tests': failed_tests,
        'failures_by_category': failures_by_category
    }

def categorize_failure(description, test_id, failures_by_category):
    """Categorize failed test based on description."""
    desc_lower = description.lower()
    
    if 'linear' in desc_lower:
        failures_by_category['linear'].append(test_id)
    elif 'quadratic' in desc_lower:
        failures_by_category['quadratic'].append(test_id)
    elif 'cubic' in desc_lower:
        failures_by_category['cubic'].append(test_id)
    elif 'quartic' in desc_lower:
        failures_by_category['quartic'].append(test_id)
    elif 'quintic' in desc_lower:
        failures_by_category['quintic'].append(test_id)
    elif 'non-uniform' in desc_lower or 'nonuniform' in desc_lower:
        failures_by_category['non_uniform'].append(test_id)
    elif 'repeated' in desc_lower or 'multiple' in desc_lower:
        failures_by_category['repeated_knots'].append(test_id)
    elif 'boundary' in desc_lower or 'edge' in desc_lower:
        failures_by_category['boundary'].append(test_id)
    else:
        failures_by_category['other'].append(test_id)

def print_validation_report(results):
    """Print a detailed validation report."""
    print("=" * 70)
    print("BSPLVN MODERN IMPLEMENTATION VALIDATION REPORT")
    print("=" * 70)
    print()
    
    print(f"Total Test Cases: {results['total_tests']}")
    print(f"Passed: {results['passed']}")
    print(f"Failed: {results['failed']}")
    print(f"Pass Rate: {results['pass_rate']:.2f}%")
    print()
    
    print("FAILURE ANALYSIS BY CATEGORY:")
    print("-" * 40)
    for category, test_ids in results['failures_by_category'].items():
        if test_ids:
            print(f"{category.upper()}: {len(test_ids)} failures")
            print(f"  Test IDs: {test_ids[:10]}{'...' if len(test_ids) > 10 else ''}")
    print()
    
    print("SAMPLE FAILED TEST CASES:")
    print("-" * 40)
    for i, failure in enumerate(results['failed_tests'][:10]):
        print(f"Test {failure['id']}: {failure['description']}")
        print(f"  Error: {failure['error']}")
        print()
    
    if results['failed'] > 10:
        print(f"... and {results['failed'] - 10} more failed tests")
    
    print()
    print("ASSESSMENT OF REPORTED FIXES:")
    print("-" * 40)
    print("Based on the validation results:")
    print("1. Array indexing (0-based to 1-based conversion)")
    print("2. State management for INDEX parameter")
    print("3. Cox-de Boor recursion implementation")
    print()
    
    if results['pass_rate'] >= 95:
        print("✓ The implementation shows excellent accuracy")
        print("✓ Core algorithm appears correctly implemented")
        print("✓ Minor remaining issues may be edge cases")
    elif results['pass_rate'] >= 80:
        print("⚠ The implementation shows good progress")
        print("⚠ Some systematic issues remain")
        print("⚠ Further debugging needed for edge cases")
    else:
        print("✗ Significant issues remain in the implementation")
        print("✗ Core algorithm may have fundamental problems")
        print("✗ Comprehensive review needed")
    
    print()
    print("FINAL RECOMMENDATION:")
    print("-" * 40)
    if results['pass_rate'] >= 95:
        print("PASS - Ready for production with minor caveats")
    elif results['pass_rate'] >= 80:
        print("CONDITIONAL PASS - Suitable for testing, not production")
    else:
        print("FAIL - Not ready for production use")

if __name__ == "__main__":
    reference_file = "test_data/bsplvn_tests.json"
    results_file = "bsplvn_results.txt"
    
    results = validate_results(reference_file, results_file)
    print_validation_report(results)
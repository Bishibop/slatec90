#!/usr/bin/env python3

import json
import re
import sys

def parse_results_file(filename):
    """Parse the modern implementation results file."""
    results = {}
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if line.startswith('TEST_'):
                # Parse: TEST_   42_RESULT: 6.2500000000000E-02 5.0781250000000E-01 ...
                match = re.match(r'TEST_\s*(\d+)_RESULT:\s*(.+)', line)
                if match:
                    test_id = int(match.group(1))
                    values_str = match.group(2)
                    
                    # Handle concatenated scientific notation (like 1.0E+00-2.0E-01)
                    # Split on E+ or E- but keep the E with the exponent
                    # Use regex to find all scientific notation numbers
                    sci_pattern = r'[+-]?\d*\.?\d+[eE][+-]?\d+'
                    values = []
                    for match_obj in re.finditer(sci_pattern, values_str):
                        values.append(float(match_obj.group()))
                    
                    # If no scientific notation found, try regular splitting
                    if not values:
                        values = [float(x) for x in values_str.split()]
                    
                    results[test_id] = values
    return results

def relative_error(expected, actual):
    """Calculate relative error between expected and actual values."""
    if abs(expected) < 1e-12:  # Handle near-zero values
        return abs(actual - expected)
    return abs((actual - expected) / expected)

def validate_bsplvn():
    """Validate BSPLVN modern implementation against F77 reference."""
    
    # Load reference test data
    print("Loading F77 reference test data...")
    with open('test_data/bsplvn_tests.json', 'r') as f:
        test_data = json.load(f)
    
    # Parse modern implementation results
    print("Loading modern implementation results...")
    modern_results = parse_results_file('bsplvn_results.txt')
    
    print(f"Found {len(modern_results)} test results")
    print(f"Expected {test_data['total_tests']} test cases")
    
    tolerance = 1e-6
    failures = []
    passes = 0
    
    print("\nValidating test cases...")
    
    for i, test_case in enumerate(test_data['test_cases']):
        # Skip test cases with missing or null expected values
        if 'expected' not in test_case or test_case['expected'] is None:
            continue
            
        # Skip test cases without test_id
        if 'test_id' not in test_case:
            continue
            
        if 'description' not in test_case:
            continue
            
        test_id = test_case['test_id']
        expected = test_case['expected']
        description = test_case['description']
        
        if test_id not in modern_results:
            failures.append({
                'test_id': test_id,
                'description': description,
                'error': 'Missing result'
            })
            continue
            
        actual = modern_results[test_id]
        
        # Check if array lengths match
        if len(expected) != len(actual):
            failures.append({
                'test_id': test_id,
                'description': description,
                'error': f'Length mismatch: expected {len(expected)} values, got {len(actual)}'
            })
            continue
        
        # Check each value with relative tolerance
        test_passed = True
        max_error = 0.0
        
        for i, (exp_val, act_val) in enumerate(zip(expected, actual)):
            error = relative_error(exp_val, act_val)
            max_error = max(max_error, error)
            
            if error > tolerance:
                test_passed = False
                break
        
        if test_passed:
            passes += 1
        else:
            failures.append({
                'test_id': test_id,
                'description': description,
                'error': f'Numerical accuracy failure (max error: {max_error:.2e})'
            })
    
    # Report results
    total_tests = len(test_data['test_cases'])
    print(f"\n=== BSPLVN VALIDATION RESULTS ===")
    print(f"Total tests: {total_tests}")
    print(f"Passes: {passes}")
    print(f"Failures: {len(failures)}")
    print(f"Pass rate: {passes/total_tests*100:.1f}%")
    
    if failures:
        print(f"\n=== FAILURE ANALYSIS ===")
        
        # Group failures by type
        missing_results = [f for f in failures if 'Missing result' in f['error']]
        length_mismatches = [f for f in failures if 'Length mismatch' in f['error']]
        numerical_failures = [f for f in failures if 'Numerical accuracy failure' in f['error']]
        
        if missing_results:
            print(f"\nMissing results ({len(missing_results)} tests):")
            for f in missing_results[:10]:  # Show first 10
                print(f"  Test {f['test_id']}: {f['description']}")
        
        if length_mismatches:
            print(f"\nLength mismatches ({len(length_mismatches)} tests):")
            for f in length_mismatches[:10]:
                print(f"  Test {f['test_id']}: {f['description']} - {f['error']}")
        
        if numerical_failures:
            print(f"\nNumerical accuracy failures ({len(numerical_failures)} tests):")
            for f in numerical_failures[:20]:  # Show first 20
                print(f"  Test {f['test_id']}: {f['description']} - {f['error']}")
                
        # Analyze patterns in failing test cases
        print(f"\n=== FAILURE PATTERNS ===")
        
        # Check for patterns in test descriptions
        failure_descriptions = [f['description'] for f in failures]
        
        linear_failures = [d for d in failure_descriptions if 'Linear' in d]
        quadratic_failures = [d for d in failure_descriptions if 'Quadratic' in d]
        cubic_failures = [d for d in failure_descriptions if 'Cubic' in d]
        quartic_failures = [d for d in failure_descriptions if 'Quartic' in d]
        quintic_failures = [d for d in failure_descriptions if 'Quintic' in d]
        
        if linear_failures:
            print(f"Linear B-spline failures: {len(linear_failures)}")
        if quadratic_failures:
            print(f"Quadratic B-spline failures: {len(quadratic_failures)}")
        if cubic_failures:
            print(f"Cubic B-spline failures: {len(cubic_failures)}")
        if quartic_failures:
            print(f"Quartic B-spline failures: {len(quartic_failures)}")
        if quintic_failures:
            print(f"Quintic B-spline failures: {len(quintic_failures)}")
        
        # Check for edge case patterns
        uniform_failures = [d for d in failure_descriptions if 'uniform' in d.lower()]
        nonuniform_failures = [d for d in failure_descriptions if 'non-uniform' in d.lower()]
        boundary_failures = [d for d in failure_descriptions if any(word in d.lower() for word in ['boundary', 'edge', 'endpoint'])]
        
        if uniform_failures:
            print(f"Uniform knot spacing failures: {len(uniform_failures)}")
        if nonuniform_failures:
            print(f"Non-uniform knot spacing failures: {len(nonuniform_failures)}")
        if boundary_failures:
            print(f"Boundary/edge case failures: {len(boundary_failures)}")
    
    else:
        print("\n✅ All tests passed!")
    
    return len(failures) == 0

if __name__ == "__main__":
    success = validate_bsplvn()
    sys.exit(0 if success else 1)
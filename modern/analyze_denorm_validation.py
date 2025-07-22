#!/usr/bin/env python3
"""Analyze DENORM validation failures in detail."""

import json
import numpy as np

def load_test_data(filename):
    """Load test data from JSON file."""
    with open(filename, 'r') as f:
        return json.load(f)

def analyze_failures():
    """Analyze failure patterns without revealing expected values."""
    # Load data
    reference_data = load_test_data('/Users/nicholasmullen/Code/gauntlet/slatec_test/test_data/denorm_tests.json')
    output_data = load_test_data('/Users/nicholasmullen/Code/gauntlet/slatec_test/modern/denorm_output_fixed_clean.json')
    
    # Extract test cases and results
    test_cases = reference_data['test_cases']
    implementation_results = output_data['results']
    
    # Analyze patterns
    print("DENORM Validation Failure Analysis")
    print("=" * 70)
    
    # Check specific categories
    categories = {
        'simple': [],
        'large_magnitude': [],
        'small_magnitude': [],
        'mixed_scale': [],
        'edge_cases': []
    }
    
    for i, (test, actual) in enumerate(zip(test_cases, implementation_results)):
        expected = test['expected']
        rel_error = abs((actual - expected) / expected) if expected != 0 else abs(actual)
        
        if rel_error > 1e-10:  # Failed test
            desc = test.get('description', '').lower()
            n = test['n']
            inputs = test['inputs']
            
            # Categorize based on description and inputs
            if 'simple' in desc or 'basic' in desc:
                categories['simple'].append(i)
            elif 'large' in desc or any(abs(x) > 1e10 for x in inputs):
                categories['large_magnitude'].append(i)
            elif 'small' in desc or 'underflow' in desc or any(abs(x) < 1e-10 and x != 0 for x in inputs):
                categories['small_magnitude'].append(i)
            elif 'mixed' in desc or 'scale' in desc:
                categories['mixed_scale'].append(i)
            elif 'edge' in desc or 'special' in desc:
                categories['edge_cases'].append(i)
    
    # Report findings
    print("\nFailure Categories:")
    for category, indices in categories.items():
        if indices:
            print(f"  {category}: {len(indices)} failures")
            # Show first few examples
            for idx in indices[:3]:
                test = test_cases[idx]
                actual = implementation_results[idx]
                expected = test['expected']
                rel_error = abs((actual - expected) / expected) if expected != 0 else abs(actual)
                print(f"    Test {idx}: {test.get('description', 'No description')}")
                print(f"      N={test['n']}, Relative error: {rel_error:.2e}")
    
    # Check for specific patterns
    print("\n\nSpecific Pattern Analysis:")
    
    # Check if it's related to N=1 cases
    n1_failures = 0
    n1_total = 0
    for i, test in enumerate(test_cases):
        if test['n'] == 1:
            n1_total += 1
            expected = test['expected']
            actual = implementation_results[i]
            rel_error = abs((actual - expected) / expected) if expected != 0 else abs(actual)
            if rel_error > 1e-10:
                n1_failures += 1
    
    print(f"  N=1 cases: {n1_failures}/{n1_total} failures")
    
    # Check for scaling factor issues
    print("\n  Checking for consistent error patterns:")
    error_ratios = []
    for i, test in enumerate(test_cases[:20]):  # Check first 20
        expected = test['expected']
        actual = implementation_results[i]
        if expected != 0 and abs((actual - expected) / expected) > 1e-10:
            ratio = actual / expected
            error_ratios.append(ratio)
            print(f"    Test {i}: actual/expected = {ratio:.6f}")
    
    if error_ratios:
        print(f"\n  Error ratio statistics:")
        print(f"    Mean: {np.mean(error_ratios):.6f}")
        print(f"    Std:  {np.std(error_ratios):.6f}")
        print(f"    Min:  {np.min(error_ratios):.6f}")
        print(f"    Max:  {np.max(error_ratios):.6f}")

if __name__ == "__main__":
    analyze_failures()
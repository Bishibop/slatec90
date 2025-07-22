#!/usr/bin/env python3
"""
Analyze specific failure patterns in DENORM implementation.
"""

import json

def load_json_fixed(filepath):
    """Load JSON with Fortran formatting fixes."""
    with open(filepath, 'r') as f:
        content = f.read()
        content = content.replace('.,', '.0,')
        content = content.replace('.]', '.0]')
        return json.loads(content)

def main():
    # Load data
    ref_data = load_json_fixed("/Users/nicholasmullen/Code/gauntlet/slatec_test/test_data/denorm_tests.json")
    blind_data = load_json_fixed("/Users/nicholasmullen/Code/gauntlet/slatec_test/modern/denorm_output.json")
    
    ref_tests = ref_data['test_cases']
    blind_values = blind_data['results']
    
    print("Analyzing specific test cases to identify patterns...\n")
    
    # Check edge cases
    print("=== Edge Case Analysis ===")
    for i, (test, actual) in enumerate(zip(ref_tests[:10], blind_values[:10])):
        if test['n'] == 0 or test['n'] == 1 or 'zero' in test['description'].lower():
            expected = test['expected']
            print(f"Test {i+1}: {test['description']}")
            print(f"  N={test['n']}, Expected={expected}, Actual={actual}")
            if expected != 0:
                rel_error = abs(actual - expected) / abs(expected)
                print(f"  Relative Error: {rel_error:.2e}")
            print()
    
    # Check scaling issues
    print("\n=== Scaling Analysis ===")
    # Look for tests with very large/small values
    for i, (test, actual) in enumerate(zip(ref_tests, blind_values)):
        if i >= len(blind_values):
            break
        desc = test['description'].lower()
        if 'rdwarf' in desc or 'rgiant' in desc or 'huge' in desc or 'tiny' in desc:
            expected = test['expected']
            print(f"Test {i+1}: {test['description']}")
            print(f"  Expected={expected:.6e}, Actual={actual:.6e}")
            if expected != 0:
                rel_error = abs(actual - expected) / abs(expected)
                print(f"  Relative Error: {rel_error:.2e}")
                if rel_error > 0.1:
                    print("  *** MAJOR SCALING ISSUE ***")
            print()
    
    # Check if implementation returns 0 when it shouldn't
    print("\n=== Zero Return Analysis ===")
    zero_count = 0
    wrong_zeros = []
    for i, (test, actual) in enumerate(zip(ref_tests, blind_values)):
        if i >= len(blind_values):
            break
        if actual == 0.0 and test['expected'] != 0.0:
            zero_count += 1
            wrong_zeros.append((i+1, test['description'], test['expected']))
    
    print(f"Found {zero_count} cases where implementation returns 0 but shouldn't:")
    for test_id, desc, expected in wrong_zeros[:5]:
        print(f"  Test {test_id}: {desc} (expected {expected:.2e})")
    if len(wrong_zeros) > 5:
        print(f"  ... and {len(wrong_zeros)-5} more")

if __name__ == "__main__":
    main()
#!/usr/bin/env python3
"""Check specific test values from DENORM test data."""

import json

# Load reference data
with open('/Users/nicholasmullen/Code/gauntlet/slatec_test/test_data/denorm_tests.json', 'r') as f:
    reference_data = json.load(f)

# Load implementation output
with open('/Users/nicholasmullen/Code/gauntlet/slatec_test/modern/denorm_output_fixed_clean.json', 'r') as f:
    output_data = json.load(f)

# Check first few test cases
print("Checking first 10 test cases:")
print("=" * 80)

for i in range(min(10, len(reference_data['test_cases']))):
    test = reference_data['test_cases'][i]
    expected = test['expected']
    actual = output_data['results'][i]
    
    print(f"\nTest {i}: {test.get('description', 'No description')}")
    print(f"  N = {test['n']}")
    print(f"  Inputs: {test['inputs']}")
    print(f"  Expected: {expected}")
    print(f"  Actual:   {actual}")
    print(f"  Ratio:    {actual/expected if expected != 0 else 'N/A'}")
    
    # Manual calculation
    import math
    manual_result = math.sqrt(sum(x**2 for x in test['inputs'][:test['n']]))
    print(f"  Manual:   {manual_result}")
    
    if abs(manual_result - actual) < 1e-10:
        print("  ✓ Implementation matches manual calculation")
    else:
        print("  ✗ Implementation differs from manual calculation")
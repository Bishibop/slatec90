#!/usr/bin/env python3
"""
Generate DENORM test cases and save to JSON without F77 validation
"""
import json
import sys
sys.path.insert(0, '.')
from slatec_test_helper import SlatecTestHelper

# Create helper and generate tests
helper = SlatecTestHelper('DENORM')
tests = helper._generate_denorm_tests()

# Create the test data structure
test_data = {
    "function": "denorm",
    "signature": "DOUBLE PRECISION FUNCTION DENORM(N, X)",
    "description": "Compute double precision Euclidean norm of a vector with overflow/underflow protection",
    "total_tests": len(tests),
    "test_cases": []
}

# Add test cases with IDs
for i, test in enumerate(tests):
    test_case = {
        "description": test["description"],
        "n": test["n"],
        "inputs": test["inputs"],
        "expected": None,  # Will be filled by F77 validation later
        "test_id": i + 1
    }
    test_data["test_cases"].append(test_case)

# Save to file
output_file = "test_data/denorm_tests_enhanced.json"
with open(output_file, 'w') as f:
    json.dump(test_data, f, indent=2)

print(f"Generated {len(tests)} test cases for DENORM")
print(f"Test data saved to {output_file}")
print(f"New tests added: {len(tests) - 157}")
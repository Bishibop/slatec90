#!/usr/bin/env python3
"""
Check what test cases were generated
"""

from simple_migration_pipeline import SimpleMigrationPipeline
import json

pipeline = SimpleMigrationPipeline()

# Read source
f77_source = open("src/denorm.f").read()

# Generate test cases
print("Generating test cases...")
test_cases = pipeline.generate_test_cases("denorm", f77_source)

print(f"\nGenerated {len(test_cases)} test cases:")
print(json.dumps(test_cases, indent=2))

# Try to get reference value for first test
print("\nTesting F77 reference generation for first test case...")
test_program = pipeline.generate_f77_test_program("denorm", test_cases[0])
print("\nGenerated test program:")
print(test_program[:500] + "...")
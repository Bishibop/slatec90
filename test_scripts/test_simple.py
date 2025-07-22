#!/usr/bin/env python3
"""
Very simple test to see where it's hanging
"""

print("1. Starting test...")

from simple_migration_pipeline import SimpleMigrationPipeline
print("2. Pipeline imported")

pipeline = SimpleMigrationPipeline()
print("3. Pipeline created")

# Just test the first step
f77_source = open("src/denorm.f").read()
print("4. Read F77 source")

deps = pipeline.extract_dependencies(f77_source)
print(f"5. Dependencies: {deps}")

print("6. Generating test cases...")
test_cases = pipeline.generate_test_cases("denorm", f77_source)
print(f"7. Generated {len(test_cases)} test cases")

print("\nTest complete!")
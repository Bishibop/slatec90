#!/usr/bin/env python3
"""
Test migration with debug output
"""

from simple_migration_pipeline import SimpleMigrationPipeline
import time

pipeline = SimpleMigrationPipeline()

# Override some methods to add debug output
original_run_f77 = pipeline.run_f77_test
def debug_run_f77(func_name, test_program):
    print(f"    Running F77 test for {func_name}...")
    start = time.time()
    result = original_run_f77(func_name, test_program)
    print(f"    F77 test completed in {time.time() - start:.2f}s, result: {result}")
    return result

pipeline.run_f77_test = debug_run_f77

# Test just getting reference values
print("Testing reference value generation...")
f77_source = open("src/denorm.f").read()
test_cases = pipeline.generate_test_cases("denorm", f77_source)
print(f"Generated {len(test_cases)} test cases")

print("\nGetting reference values:")
refs = pipeline.get_f77_reference_values("denorm", test_cases[:2])  # Just first 2
print(f"Reference values: {refs}")

print("\nDone!")
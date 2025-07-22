#!/usr/bin/env python3
"""
Migrate a very simple function: ppsgf (25 lines, no dependencies)
"""

from simple_migration_pipeline import SimpleMigrationPipeline

# Use ppsgf - it's only 25 lines with no dependencies
pipeline = SimpleMigrationPipeline()

print("Starting migration of ppsgf (25 lines, no deps)...")
result = pipeline.migrate_function("ppsgf", max_iterations=1)

print(f"\nResult: {result}")

if result["success"]:
    print("\n✓ Success! Check modern/ppsgf_modern.f90")
    # Show the generated code
    with open("modern/ppsgf_modern.f90", "r") as f:
        print("\nGenerated modern code:")
        print("-" * 60)
        print(f.read()[:1000] + "...")
else:
    print(f"\n✗ Failed with {result.get('remaining_failures', 0)} failures")
#!/usr/bin/env python3
"""
Test migration of a single function
"""

from simple_migration_pipeline import SimpleMigrationPipeline
import os

if not os.getenv("OPENAI_API_KEY"):
    print("Error: OPENAI_API_KEY not set")
    exit(1)

# Test just denorm
pipeline = SimpleMigrationPipeline()
result = pipeline.migrate_function("denorm", max_iterations=2)

print("\nResult:", result)

if result["success"]:
    print("\n✓ Migration successful!")
    print(f"Check: modern/denorm_modern.f90")
else:
    print("\n✗ Migration failed")
    print(f"Failures: {result.get('remaining_failures', 'Unknown')}")
#!/usr/bin/env python3
"""Parse the dependency tree to find all zero-dependency functions"""

import re
from pathlib import Path

def parse_dependency_tree(tree_file):
    """Parse the tree file and extract zero-dependency functions"""
    
    zero_dep_functions = []
    
    with open(tree_file, 'r') as f:
        for line in f:
            # Look for lines with "(NONE)" indicating no dependencies
            if "(NONE)" in line:
                # Extract function name - it's the last word before (NONE)
                match = re.search(r'(\w+)\s*\(NONE\)', line)
                if match:
                    func_name = match.group(1)
                    zero_dep_functions.append(func_name)
    
    return zero_dep_functions

def check_function_exists(func_name):
    """Check if the function source file exists"""
    src_file = Path(f"src/{func_name.lower()}.f")
    return src_file.exists()

# Parse the tree
tree_file = "tree"
print("Parsing dependency tree...")
zero_deps = parse_dependency_tree(tree_file)

print(f"\nFound {len(zero_deps)} zero-dependency functions")

# Check which ones exist in src/
existing = []
missing = []

for func in zero_deps:
    if check_function_exists(func):
        existing.append(func)
    else:
        missing.append(func)

print(f"\n{len(existing)} functions have source files in src/")
print(f"{len(missing)} functions are missing source files")

# Check if PYTHAG is in the list
if "PYTHAG" in existing:
    print("\n✓ PYTHAG is confirmed as a zero-dependency function")
    existing.remove("PYTHAG")  # Remove since we've already done it
    print(f"  Remaining: {len(existing)} functions to migrate")

# Save the list for future use
import json

output_data = {
    "total_zero_deps": len(zero_deps),
    "existing_in_src": len(existing),
    "already_migrated": ["PYTHAG"],
    "remaining_functions": sorted(existing)
}

with open("zero_dependency_functions.json", "w") as f:
    json.dump(output_data, f, indent=2)

print("\nSaved function list to zero_dependency_functions.json")

# Show first 10 functions to migrate
print("\nNext 10 functions to migrate:")
for i, func in enumerate(existing[:10], 1):
    print(f"  {i}. {func}")
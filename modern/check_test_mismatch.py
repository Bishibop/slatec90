#!/usr/bin/env python3
"""Check test description mismatches between reference and blind results."""

import json

# Load reference tests
with open('/Users/nicholasmullen/Code/gauntlet/slatec_test/test_data/zabs_tests.json', 'r') as f:
    ref_data = json.load(f)
    ref_tests = ref_data['test_cases']

# Load blind results
with open('/Users/nicholasmullen/Code/gauntlet/slatec_test/modern/zabs_blind_results.json', 'r') as f:
    blind_data = json.load(f)
    blind_tests = blind_data['results']

# Create sets of descriptions
ref_descriptions = {t['description'] for t in ref_tests}
blind_descriptions = {t['description'] for t in blind_tests}

# Find missing descriptions
missing_in_blind = ref_descriptions - blind_descriptions
extra_in_blind = blind_descriptions - ref_descriptions

print(f"Total reference tests: {len(ref_tests)}")
print(f"Total blind tests: {len(blind_tests)}")
print(f"\nMissing in blind results ({len(missing_in_blind)}):")
for desc in sorted(missing_in_blind):
    print(f"  - {desc}")

print(f"\nExtra in blind results ({len(extra_in_blind)}):")
for desc in sorted(extra_in_blind):
    print(f"  - {desc}")

# Check for duplicates
blind_desc_list = [t['description'] for t in blind_tests]
blind_duplicates = [desc for desc in blind_desc_list if blind_desc_list.count(desc) > 1]
if blind_duplicates:
    print(f"\nDuplicates in blind results:")
    for desc in set(blind_duplicates):
        count = blind_desc_list.count(desc)
        print(f"  - '{desc}' appears {count} times")
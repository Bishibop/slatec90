#!/usr/bin/env python3
"""
Debug CSHCH JSON structure
"""

import json

def debug_json():
    with open('test_data/cshch_tests.json', 'r') as f:
        data = json.load(f)
    
    test_cases = data.get('test_cases', [])
    print(f"Total test cases: {len(test_cases)}")
    
    for i, test_case in enumerate(test_cases):
        if 'test_id' not in test_case:
            print(f"Missing test_id in entry {i}: {test_case}")
            if i > 0:
                print(f"Previous entry: {test_cases[i-1]}")
            break
        
        # Check for other required fields
        required_fields = ['description', 'inputs', 'expected']
        missing_fields = [field for field in required_fields if field not in test_case]
        if missing_fields:
            print(f"Test {test_case.get('test_id', 'unknown')}: Missing fields {missing_fields}")
    else:
        print("All test cases have test_id field")
        
    # Check a few random entries
    for i in [0, 10, 100, 200, 300, 400, len(test_cases)-1]:
        if i < len(test_cases):
            tc = test_cases[i]
            print(f"Entry {i}: test_id={tc.get('test_id')}, description='{tc.get('description', 'N/A')[:50]}...'")

if __name__ == "__main__":
    debug_json()
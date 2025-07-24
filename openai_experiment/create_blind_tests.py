#!/usr/bin/env python3
"""Create blind test files from full test files"""

import json
from pathlib import Path

def create_blind_test_file(function_name: str):
    """Create a blind test file by removing expected values"""
    test_path = Path(f'../test_data/{function_name.lower()}_tests.json')
    blind_path = Path(f'../test_data/{function_name.lower()}_tests_blind.json')
    
    with open(test_path, 'r') as f:
        data = json.load(f)
    
    # Create blind version
    blind_data = {
        'function': data['function'],
        'signature': data['signature'],
        'description': data['description'],
        'total_tests': data['total_tests'],
        'test_cases': []
    }
    
    # Remove expected values from test cases
    for test in data['test_cases']:
        blind_test = {
            'description': test['description'],
            'inputs': test['inputs'],
            'test_id': test['test_id']
        }
        blind_data['test_cases'].append(blind_test)
    
    # Save blind version
    with open(blind_path, 'w') as f:
        json.dump(blind_data, f, indent=2)
    
    print(f"Created blind test file: {blind_path}")

if __name__ == "__main__":
    create_blind_test_file("PYTHAG")
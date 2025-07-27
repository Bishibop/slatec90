#!/usr/bin/env python3
"""
Compress test cases to include only the most relevant ones
"""
import re
import sys
from pathlib import Path

def compress_test_cases(test_content, max_tests=15):
    """
    Compress test cases to the most relevant ones
    """
    # Parse test cases
    tests = []
    current_test = None
    
    for line in test_content.splitlines():
        if line.strip() == 'TEST_START':
            current_test = {'lines': [line]}
        elif line.strip() == 'TEST_END':
            if current_test:
                current_test['lines'].append(line)
                tests.append(current_test)
                current_test = None
        elif current_test:
            current_test['lines'].append(line)
            # Extract description
            if line.startswith('Description:'):
                current_test['description'] = line
            # Extract parameter values
            elif 'PARAMS:' in line:
                current_test['params'] = line
    
    # Categorize tests
    error_tests = []
    boundary_tests = []
    special_tests = []
    regular_tests = []
    
    for test in tests:
        desc = test.get('description', '').lower()
        params = test.get('params', '')
        
        # Priority 1: Error cases
        if any(word in desc for word in ['error', 'negative', 'zero input', 'invalid', 'fail']):
            error_tests.append(test)
        # Priority 2: Boundary cases
        elif any(word in desc for word in ['boundary', 'edge', 'limit', 'overflow', 'underflow', 'tiny', 'huge', 'epsilon']):
            boundary_tests.append(test)
        # Priority 3: Special values
        elif any(word in desc for word in ['special', 'exact', 'integer', 'pi', 'inf', 'nan']):
            special_tests.append(test)
        else:
            regular_tests.append(test)
    
    # Build compressed test set
    compressed = []
    
    # Always include error tests (usually 2-3)
    compressed.extend(error_tests[:3])
    
    # Include key boundary tests (3-4)
    compressed.extend(boundary_tests[:4])
    
    # Include special value tests (2-3)
    compressed.extend(special_tests[:3])
    
    # Fill remaining slots with regular tests
    remaining_slots = max_tests - len(compressed)
    if remaining_slots > 0 and regular_tests:
        # Sample evenly from regular tests
        step = max(1, len(regular_tests) // remaining_slots)
        compressed.extend(regular_tests[::step][:remaining_slots])
    
    # Reconstruct test content
    result_lines = [f'FUNCTION: {test_content.splitlines()[0].split(":")[1].strip()}', '']
    
    for i, test in enumerate(compressed, 1):
        result_lines.extend(test['lines'])
        result_lines.append('')
    
    # Add summary comment
    result_lines.append(f'# Compressed from {len(tests)} to {len(compressed)} tests')
    result_lines.append(f'# Categories: {len(error_tests)} error, {len(boundary_tests)} boundary, {len(special_tests)} special')
    
    return '\n'.join(result_lines)

def create_test_summary(test_content):
    """
    Create a summary of test coverage without full details
    """
    # Count different types of tests
    test_count = test_content.count('TEST_START')
    
    # Extract all test descriptions to categorize
    descriptions = re.findall(r'Description:\s*(.+)', test_content)
    
    # Count test categories
    error_count = sum(1 for d in descriptions if any(w in d.lower() for w in ['error', 'negative', 'zero input']))
    integer_count = sum(1 for d in descriptions if 'integer' in d.lower() or re.search(r'Z = \d+\.0\)', d))
    boundary_count = sum(1 for d in descriptions if any(w in d.lower() for w in ['tiny', 'huge', 'epsilon', 'overflow', 'underflow']))
    
    # Extract parameter ranges
    real_params = re.findall(r'REAL_PARAMS:\s*([-\d.eE+]+)', test_content)
    if real_params:
        try:
            values = [float(p) for p in real_params if p and p != 'e']
            if values:
                min_val = min(values)
                max_val = max(values)
                
                # Find specific important values
                integers_tested = sorted(set(int(v) for v in values if v > 0 and v == int(v) and v <= 100))[:10]
                
                summary = f"""# TEST COVERAGE SUMMARY
# Total tests in full suite: {test_count}
# Parameter range tested: [{min_val:.2e}, {max_val:.2e}]
# Categories:
#   - Error cases (z <= 0): {error_count} tests
#   - Integer values: {integer_count} tests (includes {integers_tested})
#   - Boundary/edge cases: {boundary_count} tests
#   - Non-integer values: {test_count - error_count - integer_count} tests
# 
# The full test suite validates:
#   - Error handling for invalid inputs
#   - Lookup table for integers 1-100
#   - Small values requiring recurrence relation
#   - Large values using asymptotic expansion
#   - Transition regions between algorithms"""
                
                return summary
        except ValueError:
            pass
    
    return f"# Total tests: {test_count}"

if __name__ == '__main__':
    # Test with GAMLN test file
    import sys
    if len(sys.argv) > 1:
        with open(sys.argv[1]) as f:
            content = f.read()
        
        compressed = compress_test_cases(content)
        print(compressed)
        print("\n" + "="*60)
        print(create_test_summary(content))
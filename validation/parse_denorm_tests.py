#!/usr/bin/env python3
"""Parse DENORM test data and generate Fortran code for validation."""

import json
import sys

def main():
    # Read test data
    with open('test_data/denorm_tests_enhanced.json', 'r') as f:
        test_data = json.load(f)
    
    # Read reference values
    with open('test_data/denorm_output.json', 'r') as f:
        ref_data = json.load(f)
    
    # Extract test cases and reference values
    test_cases = test_data['test_cases']
    ref_values = ref_data['results']
    
    # Generate Fortran code
    print(f"! Total tests: {len(test_cases)}")
    print(f"integer, parameter :: TOTAL_TESTS = {len(test_cases)}")
    print()
    
    # Generate test data arrays
    max_n = max(tc['n'] for tc in test_cases)
    print(f"integer, parameter :: MAX_N = {max_n}")
    print()
    
    # Test metadata
    print("! Test IDs")
    print("integer :: test_ids(TOTAL_TESTS) = [", end="")
    for i, tc in enumerate(test_cases):
        if i > 0 and i % 10 == 0:
            print(" &")
        print(f"{tc['test_id']}", end="")
        if i < len(test_cases) - 1:
            print(", ", end="")
    print("]")
    print()
    
    # Vector sizes
    print("! Vector sizes")
    print("integer :: test_n(TOTAL_TESTS) = [", end="")
    for i, tc in enumerate(test_cases):
        if i > 0 and i % 10 == 0:
            print(" &")
        print(f"{tc['n']}", end="")
        if i < len(test_cases) - 1:
            print(", ", end="")
    print("]")
    print()
    
    # Reference values
    print("! Reference values")
    print("real(real64) :: ref_values(TOTAL_TESTS) = [", end="")
    for i, val in enumerate(ref_values):
        if i > 0 and i % 5 == 0:
            print(" &")
        print(f"{val}_real64", end="")
        if i < len(ref_values) - 1:
            print(", ", end="")
    print("]")
    print()
    
    # Test descriptions
    print("! Test descriptions")
    for i, tc in enumerate(test_cases):
        desc = tc['description'].replace('"', '""')
        print(f'test_desc({i+1}) = "{desc}"')
    
    # Print test categories
    print()
    print("! Test categories")
    categories = {
        'edge': [],
        'precision': [],
        'stress': [],
        'boundary': [],
        'subnormal': [],
        'overflow': [],
        'underflow': [],
        'mixed': [],
        'special': []
    }
    
    for i, tc in enumerate(test_cases):
        desc = tc['description'].lower()
        idx = i + 1
        
        if 'zero' in desc or 'empty' in desc or 'single' in desc:
            categories['edge'].append(idx)
        elif 'precision' in desc or 'ulp' in desc or 'bit pattern' in desc:
            categories['precision'].append(idx)
        elif 'large vector' in desc or tc['n'] > 1000:
            categories['stress'].append(idx)
        elif 'ieee' in desc or 'boundary' in desc:
            categories['boundary'].append(idx)
        elif 'subnormal' in desc or 'denormal' in desc:
            categories['subnormal'].append(idx)
        elif 'overflow' in desc or 'giant' in desc:
            categories['overflow'].append(idx)
        elif 'underflow' in desc or 'tiny' in desc or 'dwarf' in desc:
            categories['underflow'].append(idx)
        elif 'mixed' in desc or 'cascade' in desc:
            categories['mixed'].append(idx)
        else:
            categories['special'].append(idx)
    
    for cat, indices in categories.items():
        if indices:
            print(f"! {cat} tests: {len(indices)} tests")
            print(f"integer :: {cat}_tests({len(indices)}) = [", end="")
            for i, idx in enumerate(indices):
                print(idx, end="")
                if i < len(indices) - 1:
                    print(", ", end="")
            print("]")
    
    # Generate test vector initialization code
    print()
    print("! Initialize test vectors")
    print("subroutine init_test_vectors()")
    for i, tc in enumerate(test_cases):
        print(f"    ! Test {i+1}: {tc['description']}")
        for j, val in enumerate(tc['inputs']):
            print(f"    test_vectors({j+1}, {i+1}) = {val}_real64")
    print("end subroutine init_test_vectors")

if __name__ == '__main__':
    main()
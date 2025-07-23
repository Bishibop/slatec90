#!/usr/bin/env python3
"""
Parse SLATEC dependency tree file to extract function dependencies and create data structures
for DOT file generation.
"""

import os
import re
from typing import Dict, List, Set, Tuple
import json

def parse_tree_file(tree_file: str) -> Tuple[Dict[str, List[str]], Set[str], Set[str]]:
    """
    Parse the tree file to extract function dependencies.
    
    Returns:
        - function_deps: Dict mapping function name to list of dependencies
        - zero_deps: Set of functions with no dependencies (marked as "(NONE)")
        - all_functions: Set of all function names
    """
    function_deps = {}
    zero_deps = set()
    all_functions = set()
    
    current_function = None
    current_deps = []
    
    with open(tree_file, 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
                
            # Check if this is a function definition line (starts with non-whitespace)
            if not line.startswith(' '):
                # Save previous function if exists
                if current_function:
                    function_deps[current_function] = current_deps
                    all_functions.add(current_function)
                    if "(NONE)" in current_deps:
                        zero_deps.add(current_function)
                        function_deps[current_function] = []  # Remove "(NONE)" from deps
                
                # Parse new function line
                parts = line.split()
                current_function = parts[0]
                current_deps = []
                
                # Check for dependencies on the same line
                for part in parts[1:]:
                    if part != "(NONE)":
                        current_deps.append(part)
                        all_functions.add(part)
                    else:
                        zero_deps.add(current_function)
                        
            else:
                # This is a continuation line with more dependencies
                parts = line.split()
                for part in parts:
                    if part != "(NONE)":
                        current_deps.append(part)
                        all_functions.add(part)
        
        # Don't forget the last function
        if current_function:
            function_deps[current_function] = current_deps
            all_functions.add(current_function)
            if "(NONE)" in current_deps or not current_deps:
                zero_deps.add(current_function)
                function_deps[current_function] = []
    
    return function_deps, zero_deps, all_functions

def get_available_functions(src_dir: str) -> Set[str]:
    """
    Get the set of functions that are actually available in the src/ directory.
    """
    available = set()
    
    if not os.path.exists(src_dir):
        return available
        
    for filename in os.listdir(src_dir):
        if filename.endswith('.f'):
            # Extract function name from filename (remove .f extension)
            func_name = filename[:-2].upper()
            available.add(func_name)
    
    return available

def create_status_mapping(all_functions: Set[str], available_functions: Set[str]) -> Dict[str, str]:
    """
    Create a status mapping for functions based on completion status.
    """
    # Functions that have been completed according to the gitStatus
    completed = {
        'PYTHAG', 'CDIV', 'I1MACH', 'R1MACH', 'D1MACH', 'ENORM', 'LSAME',
        'ZABS', 'DENORM'  # Adding ZABS and DENORM from recent commits
    }
    
    # Functions currently in progress
    in_progress = set()  # None explicitly mentioned as in-progress currently
    
    status_map = {}
    
    for func in all_functions:
        if func in completed:
            status_map[func] = 'completed'
        elif func in in_progress:
            status_map[func] = 'in_progress'
        elif func in available_functions:
            status_map[func] = 'available'
        else:
            status_map[func] = 'not_available'
    
    return status_map

def generate_summary_statistics(function_deps: Dict[str, List[str]], 
                               zero_deps: Set[str], 
                               all_functions: Set[str],
                               available_functions: Set[str],
                               status_map: Dict[str, str]) -> Dict:
    """
    Generate summary statistics about the dependency tree.
    """
    stats = {
        'total_functions': len(all_functions),
        'functions_with_zero_deps': len(zero_deps),
        'available_functions': len(available_functions),
        'completed_functions': len([f for f in all_functions if status_map.get(f) == 'completed']),
        'in_progress_functions': len([f for f in all_functions if status_map.get(f) == 'in_progress']),
        'not_available_functions': len([f for f in all_functions if status_map.get(f) == 'not_available']),
        'zero_deps_available': len([f for f in zero_deps if f in available_functions]),
        'zero_deps_completed': len([f for f in zero_deps if status_map.get(f) == 'completed']),
    }
    
    # Calculate dependency chain lengths
    max_depth = 0
    avg_deps = sum(len(deps) for deps in function_deps.values()) / len(function_deps) if function_deps else 0
    
    stats['max_dependency_depth'] = max_depth  # Would need recursive calculation
    stats['average_dependencies_per_function'] = round(avg_deps, 2)
    
    return stats

def main():
    """Main function to parse tree and generate data structures."""
    
    # File paths
    tree_file = 'tree'
    src_dir = 'src'
    output_file = 'dependency_analysis.json'
    
    print("Parsing SLATEC dependency tree...")
    
    # Parse the tree file
    function_deps, zero_deps, all_functions = parse_tree_file(tree_file)
    
    # Get available functions
    available_functions = get_available_functions(src_dir)
    
    # Create status mapping
    status_map = create_status_mapping(all_functions, available_functions)
    
    # Generate statistics
    stats = generate_summary_statistics(function_deps, zero_deps, all_functions, 
                                      available_functions, status_map)
    
    # Create the main data structure
    dependency_data = {
        'metadata': {
            'description': 'SLATEC function dependency analysis',
            'source_file': tree_file,
            'generated_at': None,  # Could add timestamp
            'statistics': stats
        },
        'functions': {
            'all_functions': sorted(list(all_functions)),
            'zero_dependency_functions': sorted(list(zero_deps)),
            'available_functions': sorted(list(available_functions)),
        },
        'dependencies': function_deps,
        'status': status_map,
        'analysis': {
            'zero_deps_available': sorted([f for f in zero_deps if f in available_functions]),
            'zero_deps_completed': sorted([f for f in zero_deps if status_map.get(f) == 'completed']),
            'zero_deps_not_available': sorted([f for f in zero_deps if f not in available_functions]),
            'available_but_not_completed': sorted([f for f in available_functions 
                                                 if status_map.get(f) == 'available']),
            'functions_by_status': {
                'completed': sorted([f for f in all_functions if status_map.get(f) == 'completed']),
                'in_progress': sorted([f for f in all_functions if status_map.get(f) == 'in_progress']),
                'available': sorted([f for f in all_functions if status_map.get(f) == 'available']),
                'not_available': sorted([f for f in all_functions if status_map.get(f) == 'not_available'])
            }
        }
    }
    
    # Save to JSON file
    with open(output_file, 'w') as f:
        json.dump(dependency_data, f, indent=2)
    
    # Print summary
    print(f"\nDependency Analysis Summary:")
    print(f"=" * 50)
    print(f"Total functions in tree: {stats['total_functions']}")
    print(f"Functions with zero dependencies: {stats['functions_with_zero_deps']}")
    print(f"Available functions in src/: {stats['available_functions']}")
    print(f"Completed functions: {stats['completed_functions']}")
    print(f"Zero-dep functions available: {stats['zero_deps_available']}")
    print(f"Zero-dep functions completed: {stats['zero_deps_completed']}")
    print(f"Average dependencies per function: {stats['average_dependencies_per_function']}")
    
    print(f"\nCompleted zero-dependency functions:")
    for func in dependency_data['analysis']['zero_deps_completed']:
        print(f"  - {func}")
    
    print(f"\nAvailable zero-dependency functions (candidates for next migration):")
    available_zero_deps = [f for f in dependency_data['analysis']['zero_deps_available'] 
                          if status_map.get(f) == 'available']
    for func in available_zero_deps[:10]:  # Show first 10
        print(f"  - {func}")
    if len(available_zero_deps) > 10:
        print(f"  ... and {len(available_zero_deps) - 10} more")
    
    print(f"\nData saved to: {output_file}")
    
    return dependency_data

if __name__ == '__main__':
    dependency_data = main()
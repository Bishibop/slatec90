#!/usr/bin/env python3
"""
Parse SLATEC dependency tree file and organize functions by dependency levels.

The tree file format is:
- Each function starts with two spaces, followed by function name
- Dependencies are listed on the same line and continuation lines
- Functions with no dependencies show "(NONE)"
"""

import json
import re
from collections import defaultdict, deque
from typing import Dict, List, Set, Tuple


def parse_tree_file(filename: str) -> Dict[str, List[str]]:
    """Parse the tree file and extract function dependencies."""
    dependencies = {}
    current_function = None
    current_deps = []
    
    with open(filename, 'r') as f:
        for line in f:
            # Check if this is a new function line (starts with spaces and a letter)
            if line.startswith('  ') and line[2:3].isalpha():
                # Save previous function if exists
                if current_function:
                    dependencies[current_function] = current_deps
                
                # Parse new function line
                parts = line.strip().split()
                if parts:
                    current_function = parts[0]
                    # Check if it has no dependencies
                    if len(parts) > 1 and parts[1] == "(NONE)":
                        current_deps = []
                    else:
                        # Start collecting dependencies from this line
                        current_deps = parts[1:] if len(parts) > 1 else []
            elif line.startswith('              ') and current_function:
                # Continuation line with more dependencies
                deps = line.strip().split()
                current_deps.extend(deps)
    
    # Don't forget the last function
    if current_function:
        dependencies[current_function] = current_deps
    
    return dependencies


def categorize_by_math_type(func_name: str) -> str:
    """Categorize function by mathematical type based on naming patterns."""
    func_upper = func_name.upper()
    
    # Bessel functions
    if re.match(r'^[CDZS]?BES[IJKY]', func_upper) or func_upper in ['BESI', 'BESJ', 'BESK', 'BESY']:
        return 'bessel'
    
    # Airy functions
    if 'AIRY' in func_upper or re.match(r'^[CDZSB]?AI[RE]?$', func_upper):
        return 'airy'
    
    # Gamma and related functions
    if 'GAM' in func_upper or 'BETA' in func_upper or 'PSI' in func_upper:
        return 'gamma'
    
    # Linear algebra
    if any(pat in func_upper for pat in ['BLAS', 'LAPACK', 'GE', 'SY', 'HE', 'TR', 'DOT', 'AXPY', 'SCAL', 'COPY', 'SWAP']):
        return 'linear_algebra'
    
    # Quadrature/Integration
    if re.match(r'^[CDZS]?Q[ACFGKMNPRSW]', func_upper) or 'QUAD' in func_upper:
        return 'quadrature'
    
    # Differential equations
    if any(pat in func_upper for pat in ['DERKF', 'DEABM', 'DEBDF', 'DAS', 'ODE', 'BVP', 'RKF']):
        return 'differential_equations'
    
    # Splines and interpolation
    if any(pat in func_upper for pat in ['BSP', 'SPLINE', 'BVALU', 'PPVAL', 'INTERP']):
        return 'splines'
    
    # FFT
    if 'FFT' in func_upper:
        return 'fft'
    
    # Special functions
    if any(pat in func_upper for pat in ['ERF', 'ERFC', 'EXP', 'LOG', 'SIN', 'COS', 'TAN', 'SINH', 'COSH', 'TANH']):
        return 'special_functions'
    
    # Error handling and utilities
    if func_upper.startswith('XER') or func_upper in ['FDUMP', 'J4SAVE', 'I1MACH', 'R1MACH', 'D1MACH']:
        return 'utilities'
    
    # Complex arithmetic
    if func_upper.startswith('C') and any(pat in func_upper for pat in ['DIV', 'SQRT', 'EXP', 'LOG', 'ABS']):
        return 'complex_arithmetic'
    
    # Optimization
    if any(pat in func_upper for pat in ['MIN', 'MAX', 'OPT', 'NLS', 'NSQ']):
        return 'optimization'
    
    return 'other'


def calculate_dependency_levels(dependencies: Dict[str, List[str]]) -> Dict[int, List[str]]:
    """Calculate dependency levels for all functions."""
    # Create a set of all known functions
    all_functions = set(dependencies.keys())
    
    # Find all referenced functions that aren't defined
    all_deps = set()
    for deps in dependencies.values():
        all_deps.update(deps)
    external_deps = all_deps - all_functions
    
    # Calculate levels
    levels = {}
    assigned = set()
    
    # Level 0: Functions with no dependencies or only external dependencies
    level_0 = []
    for func, deps in dependencies.items():
        if not deps or all(d in external_deps for d in deps):
            level_0.append(func)
            levels[func] = 0
            assigned.add(func)
    
    # Calculate higher levels
    level_funcs = defaultdict(list)
    level_funcs[0] = level_0
    
    current_level = 0
    while len(assigned) < len(dependencies):
        current_level += 1
        new_level = []
        
        for func, deps in dependencies.items():
            if func in assigned:
                continue
                
            # Check if all dependencies are assigned and get max level
            max_dep_level = -1
            can_assign = True
            
            for dep in deps:
                if dep in all_functions and dep not in assigned:
                    can_assign = False
                    break
                if dep in levels:
                    max_dep_level = max(max_dep_level, levels[dep])
            
            if can_assign and max_dep_level >= 0:
                levels[func] = max_dep_level + 1
                new_level.append(func)
                assigned.add(func)
        
        if new_level:
            level_funcs[current_level] = new_level
        else:
            # Handle circular dependencies - assign remaining to highest level
            for func in dependencies:
                if func not in assigned:
                    levels[func] = current_level
                    level_funcs[current_level].append(func)
                    assigned.add(func)
            break
    
    return level_funcs


def create_mermaid_data(dependencies: Dict[str, List[str]], level_funcs: Dict[int, List[str]]) -> Dict:
    """Create structured data for Mermaid diagram generation."""
    # Group by category and level
    category_levels = defaultdict(lambda: defaultdict(list))
    
    for level, funcs in level_funcs.items():
        for func in funcs:
            category = categorize_by_math_type(func)
            category_levels[category][level].append(func)
    
    # Create edge list for dependencies
    edges = []
    for func, deps in dependencies.items():
        for dep in deps:
            if dep in dependencies:  # Only include internal dependencies
                edges.append({'from': dep, 'to': func})
    
    return {
        'functions_by_level': dict(level_funcs),
        'functions_by_category_and_level': dict(category_levels),
        'edges': edges,
        'total_functions': len(dependencies),
        'max_level': max(level_funcs.keys()) if level_funcs else 0
    }


def main():
    # Parse the tree file
    print("Parsing SLATEC dependency tree...")
    dependencies = parse_tree_file('tree')
    print(f"Found {len(dependencies)} functions")
    
    # Calculate dependency levels
    print("\nCalculating dependency levels...")
    level_funcs = calculate_dependency_levels(dependencies)
    
    # Print summary
    print("\nDependency Level Summary:")
    for level in sorted(level_funcs.keys()):
        print(f"  Level {level}: {len(level_funcs[level])} functions")
    
    # Print category summary
    categories = defaultdict(int)
    for func in dependencies:
        categories[categorize_by_math_type(func)] += 1
    
    print("\nFunction Categories:")
    for cat, count in sorted(categories.items(), key=lambda x: x[1], reverse=True):
        print(f"  {cat}: {count} functions")
    
    # Create structured output
    print("\nCreating structured output...")
    mermaid_data = create_mermaid_data(dependencies, level_funcs)
    
    # Save full dependency data
    output_data = {
        'dependencies': dependencies,
        'mermaid_data': mermaid_data,
        'categories': dict(categories)
    }
    
    with open('slatec_dependencies.json', 'w') as f:
        json.dump(output_data, f, indent=2)
    
    print("\nOutput saved to slatec_dependencies.json")
    
    # Also save a simplified version for easier viewing
    simplified = {
        'level_0_functions': sorted(level_funcs[0])[:30],  # First 30 level-0 functions
        'sample_dependencies': {
            func: dependencies[func] 
            for func in sorted(list(dependencies.keys()))[:20]
        },
        'summary': {
            'total_functions': len(dependencies),
            'levels': {level: len(funcs) for level, funcs in level_funcs.items()},
            'categories': dict(categories)
        }
    }
    
    with open('slatec_dependencies_summary.json', 'w') as f:
        json.dump(simplified, f, indent=2)
    
    print("Summary saved to slatec_dependencies_summary.json")
    
    # Print level 0 functions (no dependencies)
    print(f"\nLevel 0 functions (no dependencies): {len(level_funcs[0])}")
    print("First 20:")
    for func in sorted(level_funcs[0])[:20]:
        print(f"  - {func}")


if __name__ == '__main__':
    main()
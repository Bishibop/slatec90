#!/usr/bin/env python3
"""
Parse only the SLATEC functions we actually have (738) and organize them.
"""

import os
from collections import defaultdict, deque
from typing import Dict, List, Set

def get_available_functions() -> Set[str]:
    """Get list of functions available in src/ directory."""
    available = set()
    src_dir = 'src'
    
    if os.path.exists(src_dir):
        for filename in os.listdir(src_dir):
            if filename.endswith('.f'):
                func_name = filename[:-2].upper()
                available.add(func_name)
    
    return available

def parse_tree_for_available(available_funcs: Set[str]) -> Dict[str, List[str]]:
    """Parse tree file but only include functions we have."""
    dependencies = {}
    
    with open('tree', 'r') as f:
        current_func = None
        current_deps = []
        
        for line in f:
            line = line.rstrip()
            if not line:
                continue
                
            if line.startswith('  ') and len(line) > 2 and line[2].isupper():
                # Save previous function if it's available
                if current_func and current_func in available_funcs:
                    # Only keep dependencies that we also have
                    filtered_deps = [dep for dep in current_deps if dep in available_funcs]
                    dependencies[current_func] = filtered_deps
                
                # Parse new function
                parts = line.split()
                current_func = parts[0]
                
                if len(parts) > 1 and parts[1] == '(NONE)':
                    current_deps = []
                else:
                    current_deps = parts[1:] if len(parts) > 1 else []
                    
            elif line.startswith('              ') and current_func:
                current_deps.extend(line.split())
        
        # Save last function
        if current_func and current_func in available_funcs:
            filtered_deps = [dep for dep in current_deps if dep in available_funcs]
            dependencies[current_func] = filtered_deps
    
    return dependencies

def calculate_dependency_levels(dependencies: Dict[str, List[str]]) -> Dict[str, int]:
    """Calculate dependency levels for available functions only."""
    levels = {}
    in_degree = defaultdict(int)
    
    # Calculate in-degrees
    for func, deps in dependencies.items():
        for dep in deps:
            in_degree[func] += 1
    
    # Initialize level 0 functions
    queue = deque()
    for func in dependencies:
        if in_degree[func] == 0:
            levels[func] = 0
            queue.append(func)
    
    # Process levels
    while queue:
        current = queue.popleft()
        current_level = levels[current]
        
        # Find functions that depend on current
        for func, deps in dependencies.items():
            if current in deps and func not in levels:
                in_degree[func] -= 1
                if in_degree[func] == 0:
                    levels[func] = current_level + 1
                    queue.append(func)
    
    return levels

def categorize_functions(functions: Set[str]) -> Dict[str, List[str]]:
    """Categorize available functions by type."""
    categories = {
        'machine_constants': [],
        'error_handling': [],
        'complex_arithmetic': [],
        'blas_operations': [],
        'linear_algebra': [],
        'special_functions': [],
        'bessel_functions': [],
        'gamma_functions': [],
        'airy_functions': [],
        'quadrature': [],
        'ode_solvers': [],
        'interpolation': [],
        'optimization': [],
        'utilities': [],
        'documentation': [],
        'other': []
    }
    
    for func in functions:
        categorized = False
        
        # Machine constants
        if func in ['I1MACH', 'R1MACH', 'D1MACH', 'PIMACH']:
            categories['machine_constants'].append(func)
            categorized = True
        
        # Error handling
        elif func in ['FDUMP', 'J4SAVE', 'XERCNT', 'XERHLT', 'XERMSG', 'XERPRN', 
                      'XERSVE', 'XGETUA', 'XERCLR', 'XERMAX', 'XERDMP', 'XERBLA']:
            categories['error_handling'].append(func)
            categorized = True
        
        # Complex arithmetic (basic operations)
        elif func in ['CDIV', 'ZABS', 'ZEXP', 'ZMLT', 'CSHCH', 'ZSHCH', 'CUCHK', 'ZUCHK']:
            categories['complex_arithmetic'].append(func)
            categorized = True
        
        # BLAS and basic linear algebra
        elif any(func.startswith(prefix) for prefix in ['SASUM', 'DASUM', 'ISAMAX', 'IDAMAX']) or \
             func in ['LSAME', 'SCOPY', 'DCOPY', 'SSCAL', 'DSCAL', 'SAXPY', 'DAXPY', 'SDOT', 'DDOT']:
            categories['blas_operations'].append(func)
            categorized = True
        
        # Norms and basic utilities
        elif func in ['PYTHAG', 'ENORM', 'DENORM', 'HVNRM', 'DHVNRM', 'VNWRMS', 'DVNRMS']:
            categories['utilities'].append(func)
            categorized = True
        
        # Airy functions
        elif 'AIRY' in func or func in ['JAIRY', 'DJAIRY', 'YAIRY', 'DYAIRY']:
            categories['airy_functions'].append(func)
            categorized = True
        
        # Special functions
        elif any(x in func for x in ['CSEVL', 'INITS', 'GAMLIM']) or func.startswith('R9') or func.startswith('D9'):
            categories['special_functions'].append(func)
            categorized = True
        
        # Quadrature
        elif func.startswith(('Q', 'DQ')) and any(x in func for x in ['CHEB', 'FORM', 'MOMO', 'PSRT', 'RSLV', 'WGTC', 'WGTF', 'WGTS']):
            categories['quadrature'].append(func)
            categorized = True
        
        # Linear algebra
        elif any(x in func for x in ['GTSL', 'PTSL', 'MPYQ', 'UPDT', 'SOSSL']) or \
             func.startswith(('TRI', 'BAND', 'ORTHO')):
            categories['linear_algebra'].append(func)
            categorized = True
        
        # Interpolation/splines
        elif any(x in func for x in ['SPLINE', 'SPLVN', 'BVDER', 'POLCF', 'POLVL', 'INTRV', 'INTYD']):
            categories['interpolation'].append(func)
            categorized = True
        
        # Documentation
        elif func in ['AAAAAA', 'BSPDOC', 'FFTDOC', 'FUNDOC', 'QPDOC']:
            categories['documentation'].append(func)
            categorized = True
        
        if not categorized:
            categories['other'].append(func)
    
    # Remove empty categories and sort
    return {k: sorted(v) for k, v in categories.items() if v}

def identify_completed_functions() -> Set[str]:
    """Identify functions that are already migrated."""
    completed = {'PYTHAG', 'CDIV', 'I1MACH', 'R1MACH', 'D1MACH', 'ENORM', 'LSAME', 'ZABS', 'DENORM'}
    return completed

def main():
    print("Analyzing available SLATEC functions...")
    
    # Get available functions
    available = get_available_functions()
    print(f"Found {len(available)} available functions in src/")
    
    # Parse dependencies for available functions only
    dependencies = parse_tree_for_available(available)
    print(f"Parsed dependencies for {len(dependencies)} functions")
    
    # Calculate dependency levels
    levels = calculate_dependency_levels(dependencies)
    
    # Categorize functions
    categories = categorize_functions(available)
    
    # Get completed functions
    completed = identify_completed_functions()
    
    # Analyze zero-dependency functions
    zero_deps = [func for func, level in levels.items() if level == 0]
    zero_deps_available = [func for func in zero_deps if func not in completed]
    
    print(f"\nSummary:")
    print(f"Total available functions: {len(available)}")
    print(f"Zero-dependency functions: {len(zero_deps)}")
    print(f"Already completed: {len(completed)}")
    print(f"Available for migration: {len(zero_deps_available)}")
    
    # Level distribution
    level_counts = defaultdict(int)
    for level in levels.values():
        level_counts[level] += 1
    
    print(f"\nDependency Level Distribution:")
    for level in sorted(level_counts.keys()):
        print(f"  Level {level}: {level_counts[level]} functions")
    
    print(f"\nCategory Distribution:")
    for category, funcs in categories.items():
        completed_in_cat = len([f for f in funcs if f in completed])
        available_in_cat = len([f for f in funcs if f not in completed])
        print(f"  {category}: {len(funcs)} total ({completed_in_cat} completed, {available_in_cat} available)")
    
    # Save analysis
    analysis = {
        'total_available': len(available),
        'dependencies': dependencies,
        'levels': levels,
        'categories': categories,
        'completed': sorted(completed),
        'zero_dependency_available': sorted(zero_deps_available),
        'level_distribution': dict(level_counts)
    }
    
    import json
    with open('available_functions_analysis.json', 'w') as f:
        json.dump(analysis, f, indent=2)
    
    print(f"\nAnalysis saved to available_functions_analysis.json")
    
    # Show zero-dependency functions by category
    print(f"\nZero-dependency functions available for migration by category:")
    for category, funcs in categories.items():
        zero_funcs = [f for f in funcs if f in zero_deps_available]
        if zero_funcs:
            print(f"  {category}: {zero_funcs}")

if __name__ == '__main__':
    main()
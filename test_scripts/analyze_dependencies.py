#!/usr/bin/env python3
"""
Analyze SLATEC dependency tree to understand modernization order
"""

import re
from collections import defaultdict, Counter
from typing import Dict, List, Set, Tuple

def parse_dependency_tree(filename: str) -> Dict[str, List[str]]:
    """Parse the dependency tree file into a dictionary."""
    dependencies = {}
    current_function = None
    current_deps = []
    
    with open(filename, 'r') as f:
        for line in f:
            stripped_line = line.strip()
            if not stripped_line:
                continue
            
            # Check if this line starts a new function (not indented much)
            if not line.startswith('  '):  # Functions start with minimal indentation
                continue
                
            # Check if this line starts with spaces but not too many (new function line)
            if line.startswith('  ') and not line.startswith('              '):
                # Save previous function if exists
                if current_function:
                    dependencies[current_function] = current_deps
                
                # Parse new function line
                parts = stripped_line.split()
                if parts:
                    current_function = parts[0]
                    if len(parts) > 1:
                        if parts[1] == "(NONE)":
                            current_deps = []
                        else:
                            # Dependencies start from the second part
                            current_deps = [dep.strip() for dep in parts[1:] if dep.strip()]
                    else:
                        current_deps = []
            else:
                # Continuation line with more dependencies (heavily indented)
                if stripped_line and current_function is not None:
                    more_deps = [dep.strip() for dep in stripped_line.split() if dep.strip()]
                    current_deps.extend(more_deps)
        
        # Don't forget the last function
        if current_function:
            dependencies[current_function] = current_deps
    
    return dependencies

def analyze_dependencies(deps: Dict[str, List[str]]) -> None:
    """Perform comprehensive dependency analysis."""
    
    print("=" * 80)
    print("SLATEC DEPENDENCY TREE ANALYSIS")
    print("=" * 80)
    print()
    
    # 1. Most commonly used dependencies
    print("1. MOST COMMONLY USED DEPENDENCIES:")
    print("-" * 40)
    all_deps = []
    for func_deps in deps.values():
        all_deps.extend(func_deps)
    
    dep_counts = Counter(all_deps)
    for dep, count in dep_counts.most_common(20):
        print(f"   {dep:<15} : used by {count:3d} functions")
    print()
    
    # 2. Functions with only machine constants and error handling
    print("2. FUNCTIONS WITH ONLY MACHINE CONSTANTS & ERROR HANDLING:")
    print("-" * 60)
    machine_error_funcs = []
    machine_constants = {'I1MACH', 'R1MACH', 'D1MACH'}
    error_handling = {'XERMSG', 'XERCNT', 'XERHLT', 'XERPRN', 'XERSVE', 'XGETUA', 'FDUMP', 'J4SAVE'}
    allowed_deps = machine_constants | error_handling
    
    for func, func_deps in deps.items():
        if func_deps and all(dep in allowed_deps for dep in func_deps):
            machine_error_funcs.append((func, len(func_deps)))
    
    machine_error_funcs.sort(key=lambda x: x[1])
    for func, dep_count in machine_error_funcs:
        print(f"   {func:<15} : {dep_count:2d} deps - {', '.join(deps[func])}")
    print(f"\n   Total functions with only machine/error deps: {len(machine_error_funcs)}")
    print()
    
    # 3. Categorize by dependency complexity
    print("3. DEPENDENCY COMPLEXITY CATEGORIES:")
    print("-" * 40)
    complexity_categories = defaultdict(list)
    
    for func, func_deps in deps.items():
        dep_count = len(func_deps)
        if dep_count == 0:
            complexity_categories['0 deps'].append(func)
        elif 1 <= dep_count <= 5:
            complexity_categories['1-5 deps'].append(func)
        else:
            complexity_categories['6+ deps'].append(func)
    
    for category in ['0 deps', '1-5 deps', '6+ deps']:
        funcs = complexity_categories[category]
        print(f"   {category:<10} : {len(funcs):3d} functions")
        if len(funcs) <= 10:
            print(f"               {', '.join(sorted(funcs))}")
        else:
            print(f"               {', '.join(sorted(funcs)[:10])} ... ({len(funcs)-10} more)")
    print()
    
    # 4. Look for circular dependencies
    print("4. CIRCULAR DEPENDENCY CHECK:")
    print("-" * 30)
    
    def has_circular_dep(func: str, target: str, visited: Set[str], path: List[str]) -> List[str]:
        if func == target and len(path) > 1:
            return path + [func]
        if func in visited:
            return []
        
        visited.add(func)
        path.append(func)
        
        for dep in deps.get(func, []):
            if dep in deps:  # Only check dependencies that are also functions
                result = has_circular_dep(dep, target, visited.copy(), path.copy())
                if result:
                    return result
        
        return []
    
    circular_deps = []
    for func in deps.keys():
        cycle = has_circular_dep(func, func, set(), [])
        if cycle:
            cycle_str = ' -> '.join(cycle)
            if cycle_str not in [cd[1] for cd in circular_deps]:
                circular_deps.append((func, cycle_str))
    
    if circular_deps:
        for func, cycle in circular_deps:
            print(f"   CIRCULAR: {cycle}")
    else:
        print("   No circular dependencies found!")
    print()
    
    # 5. Dependency chains for key mathematical functions
    print("5. DEPENDENCY CHAINS FOR KEY MATHEMATICAL FUNCTIONS:")
    print("-" * 55)
    
    key_functions = ['BESI', 'BESJ', 'GAMLN', 'GAMMA', 'ERF', 'BESK']
    
    def get_dependency_chain(func: str, level: int = 0, visited: Set[str] = None) -> List[str]:
        if visited is None:
            visited = set()
        if func in visited or level > 3:  # Prevent infinite recursion and limit depth
            return []
        
        visited.add(func)
        chain = [f"{'  ' * level}{func}"]
        
        for dep in deps.get(func, []):
            if dep in deps:  # Only show dependencies that are also functions
                chain.extend(get_dependency_chain(dep, level + 1, visited.copy()))
        
        return chain
    
    for func in key_functions:
        if func in deps:
            print(f"   {func}:")
            chain = get_dependency_chain(func)
            for item in chain[:15]:  # Limit output length
                print(f"   {item}")
            if len(chain) > 15:
                print(f"     ... ({len(chain) - 15} more)")
        print()
    
    # 6. Most depended upon functions
    print("6. MOST DEPENDED UPON FUNCTIONS:")
    print("-" * 35)
    
    # Filter out machine constants and error handling from the count
    filtered_counts = {dep: count for dep, count in dep_counts.items() 
                      if dep in deps}  # Only count functions that are also defined in our tree
    
    print("   Functions that appear most frequently as dependencies:")
    for dep, count in Counter(filtered_counts).most_common(15):
        print(f"   {dep:<15} : depended upon by {count:3d} functions")
    print()
    
    # Summary statistics
    print("SUMMARY STATISTICS:")
    print("-" * 20)
    print(f"   Total functions analyzed: {len(deps)}")
    print(f"   Functions with no dependencies: {len(complexity_categories['0 deps'])}")
    print(f"   Functions with 1-5 dependencies: {len(complexity_categories['1-5 deps'])}")
    print(f"   Functions with 6+ dependencies: {len(complexity_categories['6+ deps'])}")
    print(f"   Total unique dependencies: {len(dep_counts)}")
    if dep_counts:
        print(f"   Most common dependency: {dep_counts.most_common(1)[0][0]} ({dep_counts.most_common(1)[0][1]} uses)")
    else:
        print("   No dependencies found")

if __name__ == "__main__":
    deps = parse_dependency_tree("/Users/nicholasmullen/Code/gauntlet/slatec_test/tree")
    print(f"DEBUG: Parsed {len(deps)} functions")
    if len(deps) < 10:
        print("DEBUG: Sample of parsed data:")
        for func, func_deps in list(deps.items())[:5]:
            print(f"  {func}: {func_deps}")
    analyze_dependencies(deps)
#!/usr/bin/env python3
"""
Generate insights and recommendations from SLATEC dependency analysis.
"""

import json
from typing import Dict, List, Set, Tuple
from collections import defaultdict, Counter

def load_dependency_data(json_file: str = 'dependency_analysis.json') -> Dict:
    """Load the dependency analysis data from JSON file."""
    with open(json_file, 'r') as f:
        return json.load(f)

def find_most_depended_upon(data: Dict) -> List[Tuple[str, int]]:
    """Find functions that are most depended upon by others."""
    dependency_count = Counter()
    
    for func, deps in data['dependencies'].items():
        for dep in deps:
            dependency_count[dep] += 1
    
    return dependency_count.most_common(20)

def find_dependency_chains(data: Dict, max_depth: int = 5) -> Dict[str, int]:
    """Find the longest dependency chains."""
    
    def get_max_depth(func: str, visited: Set[str] = None) -> int:
        if visited is None:
            visited = set()
        
        if func in visited:  # Circular dependency
            return 0
        
        if func not in data['dependencies'] or not data['dependencies'][func]:
            return 0
        
        visited.add(func)
        max_dep_depth = 0
        
        for dep in data['dependencies'][func]:
            dep_depth = get_max_depth(dep, visited.copy())
            max_dep_depth = max(max_dep_depth, dep_depth)
        
        return max_dep_depth + 1
    
    depths = {}
    for func in data['functions']['all_functions']:
        depths[func] = get_max_depth(func)
    
    return depths

def find_circular_dependencies(data: Dict) -> List[List[str]]:
    """Find circular dependencies in the graph."""
    
    def dfs(func: str, path: List[str], visited: Set[str]) -> List[List[str]]:
        if func in path:
            # Found a cycle
            cycle_start = path.index(func)
            return [path[cycle_start:] + [func]]
        
        if func in visited:
            return []
        
        visited.add(func)
        cycles = []
        
        if func in data['dependencies']:
            for dep in data['dependencies'][func]:
                cycles.extend(dfs(dep, path + [func], visited))
        
        return cycles
    
    all_cycles = []
    visited_global = set()
    
    for func in data['functions']['all_functions']:
        if func not in visited_global:
            cycles = dfs(func, [], set())
            all_cycles.extend(cycles)
            visited_global.add(func)
    
    return all_cycles

def recommend_next_migrations(data: Dict, limit: int = 10) -> List[Dict[str, any]]:
    """Recommend next functions to migrate based on various criteria."""
    
    candidates = data['analysis']['zero_deps_available']
    available_candidates = [f for f in candidates if data['status'].get(f) == 'available']
    
    # Calculate scores for each candidate
    most_depended = dict(find_most_depended_upon(data))
    
    recommendations = []
    for func in available_candidates:
        score = 0
        reasons = []
        
        # Higher score for functions that many others depend on
        dependency_count = most_depended.get(func, 0)
        score += dependency_count * 10
        if dependency_count > 0:
            reasons.append(f"Used by {dependency_count} other functions")
        
        # Prefer shorter function names (often simpler)
        if len(func) <= 6:
            score += 5
            reasons.append("Short name (likely simple)")
        
        # Prefer certain function patterns
        if func.startswith(('D1', 'R1', 'S1')):  # Machine-specific utilities
            score += 3
            reasons.append("Machine/system utility")
        elif func.endswith(('DOC', 'STR')):  # Documentation or string functions
            score += 2
            reasons.append("Documentation/utility function")
        elif any(x in func for x in ['MACH', 'SAVE', 'ERR']):  # System functions
            score += 4
            reasons.append("System/error handling function")
        
        recommendations.append({
            'function': func,
            'score': score,
            'dependency_count': dependency_count,
            'reasons': reasons
        })
    
    # Sort by score (descending)
    recommendations.sort(key=lambda x: x['score'], reverse=True)
    
    return recommendations[:limit]

def analyze_completion_impact(data: Dict) -> Dict[str, List[str]]:
    """Analyze what functions would become available if certain functions were completed."""
    
    impact = {}
    
    # For each incomplete function, see what it blocks
    for func in data['functions']['all_functions']:
        if data['status'].get(func) != 'completed':
            blocked = []
            
            # Find functions that depend on this one
            for other_func, deps in data['dependencies'].items():
                if func in deps and data['status'].get(other_func) != 'completed':
                    # Check if this is the only missing dependency
                    missing_deps = [d for d in deps if data['status'].get(d) != 'completed']
                    if len(missing_deps) == 1 and missing_deps[0] == func:
                        blocked.append(other_func)
            
            if blocked:
                impact[func] = blocked
    
    return impact

def generate_insights_report(data: Dict):
    """Generate a comprehensive insights report."""
    
    print("SLATEC Dependency Analysis - Insights Report")
    print("=" * 60)
    
    stats = data['metadata']['statistics']
    
    print(f"\n📊 Overview:")
    print(f"   Total functions: {stats['total_functions']}")
    print(f"   Available in src/: {stats['available_functions']} ({stats['available_functions']/stats['total_functions']*100:.1f}%)")
    print(f"   Zero dependencies: {stats['functions_with_zero_deps']} ({stats['functions_with_zero_deps']/stats['total_functions']*100:.1f}%)")
    print(f"   Completed: {stats['completed_functions']} ({stats['completed_functions']/stats['total_functions']*100:.1f}%)")
    
    print(f"\n🎯 Migration Progress:")
    print(f"   Zero-dep functions completed: {stats['zero_deps_completed']}/{stats['zero_deps_available']} ({stats['zero_deps_completed']/stats['zero_deps_available']*100:.1f}%)")
    print(f"   Zero-dep functions remaining: {stats['zero_deps_available'] - stats['zero_deps_completed']}")
    
    # Most depended upon functions
    print(f"\n🔗 Most Depended Upon Functions:")
    most_depended = find_most_depended_upon(data)
    for i, (func, count) in enumerate(most_depended[:10], 1):
        status = data['status'].get(func, 'unknown')
        status_indicator = {'completed': '✅', 'available': '🔄', 'not_available': '❌'}.get(status, '❓')
        print(f"   {i:2}. {func:<12} ({count:3} deps) {status_indicator}")
    
    # Dependency chains
    print(f"\n🔗 Longest Dependency Chains:")
    depths = find_dependency_chains(data)
    longest_chains = sorted([(func, depth) for func, depth in depths.items()], 
                           key=lambda x: x[1], reverse=True)[:10]
    for i, (func, depth) in enumerate(longest_chains, 1):
        status = data['status'].get(func, 'unknown')
        status_indicator = {'completed': '✅', 'available': '🔄', 'not_available': '❌'}.get(status, '❓')
        print(f"   {i:2}. {func:<12} (depth {depth}) {status_indicator}")
    
    # Migration recommendations
    print(f"\n🚀 Top Migration Recommendations:")
    recommendations = recommend_next_migrations(data)
    for i, rec in enumerate(recommendations[:10], 1):
        print(f"   {i:2}. {rec['function']:<12} (score: {rec['score']:2}, deps: {rec['dependency_count']:2})")
        for reason in rec['reasons']:
            print(f"       • {reason}")
    
    # Completion impact
    print(f"\n🎯 High-Impact Completions (functions that would unblock others):")
    impact = analyze_completion_impact(data)
    # Sort by number of functions unblocked
    high_impact = sorted([(func, blocked) for func, blocked in impact.items()], 
                        key=lambda x: len(x[1]), reverse=True)[:10]
    
    for func, blocked in high_impact:
        status = data['status'].get(func, 'unknown')
        if status == 'available':
            print(f"   • {func:<12} would unblock {len(blocked)} functions: {', '.join(blocked[:3])}")
            if len(blocked) > 3:
                print(f"     {' '*15} ...and {len(blocked)-3} more")
    
    # Error handling and system functions status
    print(f"\n🛠  System/Infrastructure Functions Status:")
    system_functions = [f for f in data['functions']['all_functions'] 
                       if any(keyword in f for keyword in ['MACH', 'ERR', 'XER', 'SAVE', 'DUMP'])]
    
    system_by_status = defaultdict(list)
    for func in system_functions:
        status = data['status'].get(func, 'unknown')
        system_by_status[status].append(func)
    
    for status in ['completed', 'available', 'not_available']:
        if system_by_status[status]:
            status_name = status.replace('_', ' ').title()
            print(f"   {status_name}: {len(system_by_status[status])} functions")
            if status == 'available':
                print(f"      Available: {', '.join(system_by_status[status][:5])}")
                if len(system_by_status[status]) > 5:
                    print(f"      ...and {len(system_by_status[status])-5} more")

def main():
    """Main function."""
    try:
        data = load_dependency_data()
        generate_insights_report(data)
    except FileNotFoundError:
        print("Error: dependency_analysis.json not found.")
        print("Please run parse_tree.py first to generate the dependency analysis.")
        return 1
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in dependency_analysis.json: {e}")
        return 1
    
    return 0

if __name__ == '__main__':
    exit(main())
#!/usr/bin/env python3
"""
Generate simple insights from SLATEC dependency analysis.
"""

import json
from collections import Counter

def load_dependency_data(json_file: str = 'dependency_analysis.json') -> dict:
    """Load the dependency analysis data from JSON file."""
    with open(json_file, 'r') as f:
        return json.load(f)

def find_most_depended_upon(data: dict) -> list:
    """Find functions that are most depended upon by others."""
    dependency_count = Counter()
    
    for func, deps in data['dependencies'].items():
        for dep in deps:
            dependency_count[dep] += 1
    
    return dependency_count.most_common(15)

def main():
    """Generate simple insights report."""
    
    try:
        data = load_dependency_data()
    except FileNotFoundError:
        print("Error: dependency_analysis.json not found.")
        return 1
    
    print("SLATEC Dependency Analysis - Quick Insights")
    print("=" * 50)
    
    stats = data['metadata']['statistics']
    
    print(f"\n📊 Overview:")
    print(f"   Total functions: {stats['total_functions']}")
    print(f"   Available in src/: {stats['available_functions']} ({stats['available_functions']/stats['total_functions']*100:.1f}%)")
    print(f"   Zero dependencies: {stats['functions_with_zero_deps']}")
    print(f"   Completed: {stats['completed_functions']}")
    
    print(f"\n🎯 Migration Status:")
    completed = data['analysis']['functions_by_status']['completed']
    available_zero_deps = [f for f in data['analysis']['zero_deps_available'] 
                          if data['status'].get(f) == 'available']
    
    print(f"   ✅ Completed zero-dep functions: {len(completed)}")
    for func in completed:
        print(f"      • {func}")
    
    print(f"\n   🔄 Available zero-dep functions (next candidates): {len(available_zero_deps)}")
    # Show some priority candidates
    priority_keywords = ['MACH', 'ERR', 'SAVE', 'DUMP', 'STR']
    priority_funcs = [f for f in available_zero_deps 
                     if any(kw in f for kw in priority_keywords)]
    simple_funcs = [f for f in available_zero_deps if len(f) <= 6]
    
    print(f"      Priority (system functions): {len(priority_funcs)} available")
    for func in priority_funcs[:5]:
        print(f"        • {func}")
    
    print(f"      Simple (short names): {len(simple_funcs)} available")
    for func in simple_funcs[:10]:
        print(f"        • {func}")
    
    print(f"\n🔗 Most Depended Upon Functions:")
    most_depended = find_most_depended_upon(data)
    for i, (func, count) in enumerate(most_depended[:10], 1):
        status = data['status'].get(func, 'unknown')
        status_indicator = {'completed': '✅', 'available': '🔄', 'not_available': '❌'}.get(status, '❓')
        print(f"   {i:2}. {func:<12} ({count:3} deps) {status_indicator}")
    
    print(f"\n📈 Next Recommended Migrations:")
    print("   Based on zero dependencies and importance:")
    
    # Simple scoring for recommendations
    recommendations = []
    for func in available_zero_deps[:20]:  # Limit to avoid long processing
        score = 0
        reasons = []
        
        # Check if it's used by others
        dep_count = dict(most_depended).get(func, 0)
        if dep_count > 0:
            score += dep_count * 2
            reasons.append(f"Used by {dep_count} functions")
        
        # System function bonus
        if any(kw in func for kw in ['MACH', 'ERR', 'SAVE', 'DUMP']):
            score += 10
            reasons.append("System function")
        
        # Simple function bonus
        if len(func) <= 6:
            score += 3
            reasons.append("Short name")
        
        if score > 0 or not reasons:
            recommendations.append((func, score, reasons))
    
    recommendations.sort(key=lambda x: x[1], reverse=True)
    
    for i, (func, score, reasons) in enumerate(recommendations[:8], 1):
        print(f"   {i}. {func:<12} (score: {score})")
        if reasons:
            print(f"      • {', '.join(reasons)}")

if __name__ == '__main__':
    main()
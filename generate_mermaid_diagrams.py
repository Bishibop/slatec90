#!/usr/bin/env python3
"""
Generate Mermaid diagrams from SLATEC dependency data.
"""

import json
from collections import defaultdict
from typing import Dict, List, Set


def load_dependencies():
    """Load the parsed dependency data."""
    with open('slatec_dependencies.json', 'r') as f:
        return json.load(f)


def generate_category_diagram(data: Dict, category: str, max_nodes: int = 30) -> str:
    """Generate a Mermaid diagram for a specific category."""
    dependencies = data['dependencies']
    mermaid_data = data['mermaid_data']
    category_levels = mermaid_data['functions_by_category_and_level']
    
    if category not in category_levels:
        return f"No functions found in category: {category}"
    
    # Get all functions in this category
    category_funcs = set()
    for level_funcs in category_levels[category].values():
        category_funcs.update(level_funcs)
    
    # Limit to max_nodes most connected functions
    func_connections = defaultdict(int)
    for func in category_funcs:
        if func in dependencies:
            for dep in dependencies[func]:
                if dep in category_funcs:
                    func_connections[func] += 1
                    func_connections[dep] += 1
    
    # Sort by number of connections
    sorted_funcs = sorted(func_connections.items(), key=lambda x: x[1], reverse=True)
    selected_funcs = set([f[0] for f in sorted_funcs[:max_nodes]])
    
    # Build diagram
    lines = ["```mermaid", "graph TD"]
    
    # Add nodes by level
    for level in sorted(category_levels[category].keys()):
        level_funcs = [f for f in category_levels[category][level] if f in selected_funcs]
        if level_funcs:
            lines.append(f"    %% Level {level}")
            for func in level_funcs:
                lines.append(f"    {func}")
    
    # Add edges
    lines.append("    %% Dependencies")
    for func in selected_funcs:
        if func in dependencies:
            for dep in dependencies[func]:
                if dep in selected_funcs:
                    lines.append(f"    {dep} --> {func}")
    
    lines.append("```")
    return "\n".join(lines)


def generate_level_0_summary(data: Dict) -> str:
    """Generate a summary of level 0 functions by category."""
    level_0_funcs = data['mermaid_data']['functions_by_level']['0']
    categories = data['categories']
    
    # Group level 0 functions by category
    level_0_by_category = defaultdict(list)
    for func in level_0_funcs:
        # Find category
        for cat_name, cat_data in data['mermaid_data']['functions_by_category_and_level'].items():
            if '0' in cat_data and func in cat_data['0']:
                level_0_by_category[cat_name].append(func)
                break
    
    lines = ["# Level 0 Functions (No Dependencies)", ""]
    lines.append(f"Total: {len(level_0_funcs)} functions")
    lines.append("")
    
    for category in sorted(level_0_by_category.keys()):
        funcs = sorted(level_0_by_category[category])
        lines.append(f"## {category.replace('_', ' ').title()} ({len(funcs)} functions)")
        lines.append("")
        
        # Show first 20
        for func in funcs[:20]:
            lines.append(f"- {func}")
        
        if len(funcs) > 20:
            lines.append(f"- ... and {len(funcs) - 20} more")
        lines.append("")
    
    return "\n".join(lines)


def generate_migration_candidates(data: Dict) -> str:
    """Generate a list of good migration candidates based on dependencies."""
    dependencies = data['dependencies']
    level_0_funcs = data['mermaid_data']['functions_by_level']['0']
    
    # Already migrated functions (from MIGRATION_GUIDE.md)
    migrated = {'PYTHAG', 'CDIV', 'I1MACH', 'R1MACH', 'D1MACH', 'ENORM', 'LSAME', 'ZABS'}
    
    # Functions that are frequently used as dependencies
    dep_count = defaultdict(int)
    for func, deps in dependencies.items():
        for dep in deps:
            dep_count[dep] += 1
    
    # Sort level 0 functions by how often they're used
    level_0_by_usage = []
    for func in level_0_funcs:
        if func not in migrated and func in dep_count:
            level_0_by_usage.append((func, dep_count[func]))
    
    level_0_by_usage.sort(key=lambda x: x[1], reverse=True)
    
    lines = ["# Migration Priority Candidates", ""]
    lines.append("## High-Priority Level 0 Functions (by usage)")
    lines.append("")
    lines.append("| Function | Used By | Category |")
    lines.append("|----------|---------|----------|")
    
    # Find category for each function
    for func, count in level_0_by_usage[:30]:
        category = "other"
        for cat_name, cat_data in data['mermaid_data']['functions_by_category_and_level'].items():
            if '0' in cat_data and func in cat_data['0']:
                category = cat_name
                break
        lines.append(f"| {func} | {count} functions | {category} |")
    
    return "\n".join(lines)


def main():
    # Load dependency data
    print("Loading dependency data...")
    data = load_dependencies()
    
    # Generate level 0 summary
    print("\nGenerating level 0 summary...")
    level_0_summary = generate_level_0_summary(data)
    with open('level_0_functions.md', 'w') as f:
        f.write(level_0_summary)
    print("Saved to level_0_functions.md")
    
    # Generate migration candidates
    print("\nGenerating migration candidates...")
    candidates = generate_migration_candidates(data)
    with open('migration_candidates.md', 'w') as f:
        f.write(candidates)
    print("Saved to migration_candidates.md")
    
    # Generate sample category diagrams
    print("\nGenerating category diagrams...")
    diagrams = []
    
    # Linear algebra diagram
    la_diagram = generate_category_diagram(data, 'linear_algebra', max_nodes=20)
    diagrams.append(("# Linear Algebra Functions", la_diagram))
    
    # Utilities diagram
    util_diagram = generate_category_diagram(data, 'utilities', max_nodes=20)
    diagrams.append(("# Utility Functions", util_diagram))
    
    # Special functions diagram
    spec_diagram = generate_category_diagram(data, 'special_functions', max_nodes=20)
    diagrams.append(("# Special Functions", spec_diagram))
    
    with open('slatec_diagrams.md', 'w') as f:
        for title, diagram in diagrams:
            f.write(title + "\n\n")
            f.write(diagram + "\n\n")
    print("Saved diagrams to slatec_diagrams.md")


if __name__ == '__main__':
    main()
#!/usr/bin/env python3
"""
Generate DOT files for visualizing SLATEC function dependencies.
"""

import json
import argparse
from typing import Dict, List, Set

def load_dependency_data(json_file: str) -> Dict:
    """Load the dependency analysis data from JSON file."""
    with open(json_file, 'r') as f:
        return json.load(f)

def generate_status_colors() -> Dict[str, str]:
    """Define colors for different function statuses."""
    return {
        'completed': '#90EE90',      # Light green
        'in_progress': '#FFD700',    # Gold
        'available': '#87CEEB',      # Sky blue
        'not_available': '#FFB6C1'   # Light pink
    }

def generate_full_dependency_dot(data: Dict, output_file: str = 'full_dependencies.dot'):
    """Generate a DOT file for the complete dependency graph."""
    
    colors = generate_status_colors()
    
    with open(output_file, 'w') as f:
        f.write('digraph SLATEC_Dependencies {\n')
        f.write('  rankdir=TB;\n')
        f.write('  node [shape=box, style=filled];\n')
        f.write('  edge [color=gray];\n\n')
        
        # Add legend
        f.write('  subgraph cluster_legend {\n')
        f.write('    label="Status Legend";\n')
        f.write('    style=filled;\n')
        f.write('    color=lightgray;\n')
        for status, color in colors.items():
            f.write(f'    legend_{status} [label="{status.replace("_", " ").title()}", fillcolor="{color}"];\n')
        f.write('  }\n\n')
        
        # Add all nodes with status-based coloring
        for func in data['functions']['all_functions']:
            status = data['status'].get(func, 'not_available')
            color = colors.get(status, '#FFFFFF')
            f.write(f'  "{func}" [fillcolor="{color}"];\n')
        
        f.write('\n')
        
        # Add dependencies (edges)
        for func, deps in data['dependencies'].items():
            for dep in deps:
                f.write(f'  "{dep}" -> "{func}";\n')
        
        f.write('}\n')
    
    print(f"Full dependency graph saved to: {output_file}")

def generate_zero_deps_dot(data: Dict, output_file: str = 'zero_dependencies.dot'):
    """Generate a DOT file showing only zero-dependency functions."""
    
    colors = generate_status_colors()
    zero_deps = data['functions']['zero_dependency_functions']
    
    with open(output_file, 'w') as f:
        f.write('digraph SLATEC_Zero_Dependencies {\n')
        f.write('  rankdir=TB;\n')
        f.write('  node [shape=box, style=filled];\n\n')
        
        # Add legend
        f.write('  subgraph cluster_legend {\n')
        f.write('    label="Status Legend";\n')
        f.write('    style=filled;\n')
        f.write('    color=lightgray;\n')
        for status, color in colors.items():
            f.write(f'    legend_{status} [label="{status.replace("_", " ").title()}", fillcolor="{color}"];\n')
        f.write('  }\n\n')
        
        # Group by status
        by_status = data['analysis']['functions_by_status']
        
        for status in ['completed', 'in_progress', 'available', 'not_available']:
            status_funcs = [f for f in by_status[status] if f in zero_deps]
            if status_funcs:
                f.write(f'  subgraph cluster_{status} {{\n')
                f.write(f'    label="{status.replace("_", " ").title()} ({len(status_funcs)})";\n')
                f.write('    style=filled;\n')
                f.write('    color=lightgray;\n')
                
                color = colors[status]
                for func in status_funcs:
                    f.write(f'    "{func}" [fillcolor="{color}"];\n')
                f.write('  }\n\n')
        
        f.write('}\n')
    
    print(f"Zero-dependency graph saved to: {output_file}")

def generate_migration_candidates_dot(data: Dict, output_file: str = 'migration_candidates.dot'):
    """Generate a DOT file showing available zero-dependency functions as migration candidates."""
    
    colors = generate_status_colors()
    candidates = data['analysis']['zero_deps_available']
    completed = data['analysis']['zero_deps_completed']
    
    with open(output_file, 'w') as f:
        f.write('digraph SLATEC_Migration_Candidates {\n')
        f.write('  rankdir=TB;\n')
        f.write('  node [shape=box, style=filled];\n\n')
        
        # Completed functions
        f.write('  subgraph cluster_completed {\n')
        f.write(f'    label="Completed ({len(completed)})";\n')
        f.write('    style=filled;\n')
        f.write('    color=lightgreen;\n')
        for func in completed:
            f.write(f'    "{func}" [fillcolor="{colors["completed"]}"];\n')
        f.write('  }\n\n')
        
        # Available candidates (not yet completed)
        available_candidates = [f for f in candidates if data['status'].get(f) == 'available']
        f.write('  subgraph cluster_candidates {\n')
        f.write(f'    label="Available for Migration ({len(available_candidates)})";\n')
        f.write('    style=filled;\n')
        f.write('    color=lightblue;\n')
        for func in available_candidates:
            f.write(f'    "{func}" [fillcolor="{colors["available"]}"];\n')
        f.write('  }\n\n')
        
        f.write('}\n')
    
    print(f"Migration candidates graph saved to: {output_file}")

def generate_dependency_subgraph(data: Dict, function_name: str, 
                                output_file: str = None, max_depth: int = 2):
    """Generate a DOT file for a specific function and its dependencies."""
    
    if output_file is None:
        output_file = f'{function_name.lower()}_dependencies.dot'
    
    colors = generate_status_colors()
    
    # Find all dependencies recursively up to max_depth
    def get_dependencies(func: str, depth: int = 0) -> Set[str]:
        if depth >= max_depth or func not in data['dependencies']:
            return set()
        
        deps = set(data['dependencies'][func])
        for dep in list(deps):
            deps.update(get_dependencies(dep, depth + 1))
        return deps
    
    all_deps = get_dependencies(function_name)
    all_funcs = {function_name} | all_deps
    
    with open(output_file, 'w') as f:
        f.write(f'digraph {function_name}_Dependencies {{\n')
        f.write('  rankdir=TB;\n')
        f.write('  node [shape=box, style=filled];\n')
        f.write('  edge [color=gray];\n\n')
        
        # Highlight the target function
        status = data['status'].get(function_name, 'not_available')
        color = colors.get(status, '#FFFFFF')
        f.write(f'  "{function_name}" [fillcolor="{color}", penwidth=3, color=red];\n')
        
        # Add dependency nodes
        for func in all_deps:
            status = data['status'].get(func, 'not_available')
            color = colors.get(status, '#FFFFFF')
            f.write(f'  "{func}" [fillcolor="{color}"];\n')
        
        f.write('\n')
        
        # Add edges for functions in our subgraph
        for func in all_funcs:
            if func in data['dependencies']:
                for dep in data['dependencies'][func]:
                    if dep in all_funcs:
                        f.write(f'  "{dep}" -> "{func}";\n')
        
        f.write('}\n')
    
    print(f"Dependency subgraph for {function_name} saved to: {output_file}")

def main():
    """Main function with command-line interface."""
    parser = argparse.ArgumentParser(description='Generate DOT files from SLATEC dependency analysis')
    parser.add_argument('--data-file', default='dependency_analysis.json',
                       help='JSON file with dependency analysis (default: dependency_analysis.json)')
    parser.add_argument('--full', action='store_true',
                       help='Generate full dependency graph')
    parser.add_argument('--zero-deps', action='store_true',
                       help='Generate zero-dependency functions graph')
    parser.add_argument('--candidates', action='store_true',
                       help='Generate migration candidates graph')
    parser.add_argument('--function', type=str,
                       help='Generate dependency subgraph for specific function')
    parser.add_argument('--max-depth', type=int, default=2,
                       help='Maximum depth for function dependency subgraph (default: 2)')
    parser.add_argument('--all', action='store_true',
                       help='Generate all standard graphs')
    
    args = parser.parse_args()
    
    # Load data
    try:
        data = load_dependency_data(args.data_file)
        print(f"Loaded dependency data from {args.data_file}")
    except FileNotFoundError:
        print(f"Error: Could not find {args.data_file}")
        print("Please run parse_tree.py first to generate the dependency analysis.")
        return 1
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in {args.data_file}: {e}")
        return 1
    
    # Generate requested graphs
    if args.all or args.zero_deps:
        generate_zero_deps_dot(data)
    
    if args.all or args.candidates:
        generate_migration_candidates_dot(data)
    
    if args.full:
        generate_full_dependency_dot(data)
    
    if args.function:
        if args.function.upper() in data['functions']['all_functions']:
            generate_dependency_subgraph(data, args.function.upper(), max_depth=args.max_depth)
        else:
            print(f"Error: Function {args.function} not found in dependency data")
            available = [f for f in data['functions']['all_functions'] 
                        if f.startswith(args.function.upper())]
            if available:
                print(f"Did you mean one of: {', '.join(available[:5])}")
            return 1
    
    # Default behavior if no specific options given
    if not (args.full or args.zero_deps or args.candidates or args.function or args.all):
        print("Generating default graphs (zero-deps and candidates)...")
        generate_zero_deps_dot(data)
        generate_migration_candidates_dot(data)
    
    print("\nTo visualize the DOT files, use:")
    print("  dot -Tpng filename.dot -o filename.png")
    print("  dot -Tsvg filename.dot -o filename.svg")
    
    return 0

if __name__ == '__main__':
    exit(main())
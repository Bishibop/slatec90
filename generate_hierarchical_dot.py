#!/usr/bin/env python3
"""
Generate hierarchical DOT files organized by dependency levels and functional groups.
"""

import json
import argparse
from typing import Dict, List, Set, Tuple
from collections import defaultdict

def load_dependency_data(json_file: str) -> Dict:
    """Load the dependency analysis data from JSON file."""
    with open(json_file, 'r') as f:
        return json.load(f)

def calculate_dependency_levels(data: Dict) -> Dict[str, int]:
    """Calculate the dependency level for each function (0 = no deps, 1 = depends on level 0, etc.)"""
    levels = {}
    dependencies = data['dependencies']
    
    # Start with zero-dependency functions
    for func in data['functions']['zero_dependency_functions']:
        levels[func] = 0
    
    # Iteratively calculate levels
    changed = True
    while changed:
        changed = False
        for func, deps in dependencies.items():
            if func in levels:
                continue
                
            # Check if all dependencies have known levels
            if all(dep in levels for dep in deps):
                max_dep_level = max(levels[dep] for dep in deps) if deps else 0
                levels[func] = max_dep_level + 1
                changed = True
    
    return levels

def categorize_functions(functions: List[str]) -> Dict[str, List[str]]:
    """Categorize functions by their purpose/domain based on naming patterns."""
    categories = {
        'Machine Constants': [],
        'Error Handling': [],
        'BLAS Level 1': [],
        'BLAS Level 2/3': [],
        'Linear Algebra': [],
        'Special Functions': [],
        'Complex Arithmetic': [],
        'Norms & Metrics': [],
        'Quadrature': [],
        'ODE Solvers': [],
        'PDE Solvers': [],
        'Optimization': [],
        'Utilities': []
    }
    
    for func in functions:
        name = func.upper()
        
        # Machine constants
        if name in ['I1MACH', 'R1MACH', 'D1MACH', 'PIMACH']:
            categories['Machine Constants'].append(func)
        
        # Error handling
        elif any(x in name for x in ['XER', 'FDUMP', 'J4SAVE']):
            categories['Error Handling'].append(func)
        
        # BLAS Level 1 (vector operations)
        elif any(name.startswith(x) for x in ['SASUM', 'DASUM', 'SCASUM', 'DZASUM',
                                              'SAXPY', 'DAXPY', 'CAXPY', 'ZAXPY',
                                              'SCOPY', 'DCOPY', 'CCOPY', 'ZCOPY',
                                              'SDOT', 'DDOT', 'CDOT', 'ZDOT',
                                              'SNRM2', 'DNRM2', 'SCNRM2', 'DZNRM2',
                                              'SROT', 'DROT', 'CSROT', 'ZDROT',
                                              'SSCAL', 'DSCAL', 'CSCAL', 'ZSCAL',
                                              'SSWAP', 'DSWAP', 'CSWAP', 'ZSWAP',
                                              'ISAMAX', 'IDAMAX', 'ICAMAX', 'IZAMAX']):
            categories['BLAS Level 1'].append(func)
        
        # BLAS Level 2/3 (matrix operations)
        elif any(x in name for x in ['GEMV', 'GEMM', 'TRMV', 'TRMM', 'SYRK', 'SYR2K', 'GER']):
            categories['BLAS Level 2/3'].append(func)
        
        # Linear algebra
        elif any(x in name for x in ['GE', 'GB', 'GT', 'PO', 'PP', 'PB', 'PT', 'SY', 'SP', 'SB', 'ST',
                                     'HE', 'HP', 'HB', 'HT', 'TR', 'TP', 'TB', 'TZ']) and \
             any(x in name for x in ['FA', 'SL', 'CO', 'DI', 'TRF', 'TRS', 'CON', 'EQU']):
            categories['Linear Algebra'].append(func)
        
        # Special functions
        elif any(x in name for x in ['GAMMA', 'BETA', 'ERF', 'BESSEL', 'AIRY', 'JAIRY', 'YAIRY',
                                     'BESI', 'BESJ', 'BESK', 'BESY', 'AI', 'BI']):
            categories['Special Functions'].append(func)
        
        # Complex arithmetic
        elif any(name.startswith(x) for x in ['C', 'Z']) and \
             any(x in name for x in ['DIV', 'ABS', 'ARG', 'EXP', 'LOG', 'SIN', 'COS', 'TAN',
                                     'SINH', 'COSH', 'TANH', 'SQRT']):
            categories['Complex Arithmetic'].append(func)
        
        # Norms and metrics
        elif any(x in name for x in ['NORM', 'NRM']):
            categories['Norms & Metrics'].append(func)
        
        # Quadrature
        elif any(name.startswith(x) for x in ['Q', 'DQ']) and \
             any(x in name for x in ['QUAD', 'INT', 'WGT', 'K15', 'K21', 'K31', 'K41', 'K51', 'K61']):
            categories['Quadrature'].append(func)
        
        # ODE solvers
        elif any(x in name for x in ['ODE', 'STEP', 'INTRP', 'RK']):
            categories['ODE Solvers'].append(func)
        
        # PDE solvers
        elif any(x in name for x in ['POISSON', 'HELMH', 'SEPX', 'BLKTRI', 'GENBUN']):
            categories['PDE Solvers'].append(func)
        
        # Optimization
        elif any(x in name for x in ['MIN', 'MAX', 'OPT', 'FMIN', 'LBFGS']):
            categories['Optimization'].append(func)
        
        else:
            categories['Utilities'].append(func)
    
    # Remove empty categories
    return {k: v for k, v in categories.items() if v}

def generate_hierarchical_dot(data: Dict, output_file: str = 'hierarchical_dependencies.dot'):
    """Generate a hierarchical DOT file organized by dependency levels and categories."""
    
    levels = calculate_dependency_levels(data)
    available_functions = data['analysis']['zero_deps_available'] + data['analysis']['zero_deps_completed']
    # Get all functions that are available in src/ (not just zero deps)
    all_available = [f for f, status in data['status'].items() if status != 'not_available']
    
    # Group functions by level
    functions_by_level = defaultdict(list)
    for func, level in levels.items():
        if func in all_available:  # Only include functions we have in src/
            functions_by_level[level].append(func)
    
    # Status colors
    colors = {
        'completed': '#90EE90',      # Light green
        'in_progress': '#FFD700',    # Gold
        'available': '#87CEEB',      # Sky blue
        'not_available': '#FFB6C1'   # Light pink
    }
    
    with open(output_file, 'w') as f:
        f.write('digraph SLATEC_Hierarchical {\n')
        f.write('  rankdir=TB;\n')
        f.write('  node [shape=box, style=filled];\n')
        f.write('  edge [color=gray];\n')
        f.write('  splines=true;\n')
        f.write('  overlap=false;\n\n')
        
        # Add legend
        f.write('  subgraph cluster_legend {\n')
        f.write('    label="Status Legend";\n')
        f.write('    style=filled;\n')
        f.write('    color=lightgray;\n')
        f.write('    rank=source;\n')
        for status, color in colors.items():
            label = status.replace('_', ' ').title()
            f.write(f'    legend_{status} [label="{label}", fillcolor="{color}"];\n')
        f.write('  }\n\n')
        
        # Process each dependency level
        max_level = max(functions_by_level.keys()) if functions_by_level else 0
        
        for level in range(min(max_level + 1, 4)):  # Limit to first 4 levels for readability
            level_functions = functions_by_level.get(level, [])
            if not level_functions:
                continue
                
            f.write(f'  subgraph cluster_level_{level} {{\n')
            f.write(f'    label="Dependency Level {level} ({len(level_functions)} functions)";\n')
            f.write('    style=filled;\n')
            f.write('    color=lightblue;\n')
            
            if level == 0:
                f.write('    rank=source;\n')
            
            # Categorize functions at this level
            categories = categorize_functions(level_functions)
            
            for category, funcs in categories.items():
                if not funcs:
                    continue
                    
                f.write(f'    subgraph cluster_level_{level}_{category.replace(" ", "_").replace("/", "_").lower()} {{\n')
                f.write(f'      label="{category} ({len(funcs)})";\n')
                f.write('      style=filled;\n')
                f.write('      color=lightyellow;\n')
                
                for func in funcs:
                    status = data['status'].get(func, 'available')
                    color = colors.get(status, '#87CEEB')
                    
                    # Add dependency count for high-impact functions
                    dependents = sum(1 for deps in data['dependencies'].values() if func in deps)
                    if dependents > 10:
                        label = f"{func}\\n({dependents} deps)"
                        penwidth = 3
                    else:
                        label = func
                        penwidth = 1
                    
                    f.write(f'      "{func}" [label="{label}", fillcolor="{color}", penwidth={penwidth}];\n')
                
                f.write('    }\n')
            
            f.write('  }\n\n')
        
        # Add dependency edges (only between levels we're showing)
        max_shown_level = min(max_level, 3)
        for func, deps in data['dependencies'].items():
            func_level = levels.get(func, 999)
            if func_level <= max_shown_level and func in all_available:
                for dep in deps:
                    dep_level = levels.get(dep, 999)
                    if dep_level <= max_shown_level and dep in all_available:
                        # Use thicker edges for critical dependencies
                        if dep in ['I1MACH', 'R1MACH', 'D1MACH', 'FDUMP', 'J4SAVE']:
                            f.write(f'  "{dep}" -> "{func}" [penwidth=2, color=red];\n')
                        else:
                            f.write(f'  "{dep}" -> "{func}";\n')
        
        f.write('}\n')
    
    print(f"Hierarchical dependency graph saved to: {output_file}")
    print(f"Showing dependency levels 0-{min(max_level, 3)} with {sum(len(functions_by_level[i]) for i in range(min(max_level + 1, 4)))} functions")

def generate_critical_path_dot(data: Dict, output_file: str = 'critical_path.dot'):
    """Generate a DOT file showing only the critical migration path."""
    
    # High-priority functions based on dependent count
    critical_functions = []
    for func, deps in data['dependencies'].items():
        dependent_count = sum(1 for f_deps in data['dependencies'].values() if func in f_deps)
        if dependent_count > 50 or func in data['analysis']['zero_deps_completed']:
            critical_functions.append(func)
    
    # Add their immediate dependencies
    expanded_critical = set(critical_functions)
    for func in critical_functions:
        if func in data['dependencies']:
            expanded_critical.update(data['dependencies'][func])
    
    # Filter to only available functions
    all_available = set(f for f, status in data['status'].items() if status != 'not_available')
    critical_available = expanded_critical & all_available
    
    colors = {
        'completed': '#90EE90',
        'in_progress': '#FFD700',
        'available': '#87CEEB',
        'high_priority': '#FF6B6B'
    }
    
    with open(output_file, 'w') as f:
        f.write('digraph SLATEC_Critical_Path {\n')
        f.write('  rankdir=TB;\n')
        f.write('  node [shape=box, style=filled];\n')
        f.write('  edge [color=gray];\n\n')
        
        # Group by status and priority
        completed = [f for f in critical_available if data['status'].get(f) == 'completed']
        in_progress = [f for f in critical_available if data['status'].get(f) == 'in_progress']
        high_priority = []
        available = []
        
        for func in critical_available:
            if data['status'].get(func) in ['completed', 'in_progress']:
                continue
            dependent_count = sum(1 for f_deps in data['dependencies'].values() if func in f_deps)
            if dependent_count > 100:
                high_priority.append(func)
            else:
                available.append(func)
        
        # Completed functions
        if completed:
            f.write('  subgraph cluster_completed {\n')
            f.write(f'    label="Completed ({len(completed)})";\n')
            f.write('    style=filled;\n')
            f.write('    color=lightgreen;\n')
            for func in completed:
                f.write(f'    "{func}" [fillcolor="{colors["completed"]}"];\n')
            f.write('  }\n\n')
        
        # In progress
        if in_progress:
            f.write('  subgraph cluster_in_progress {\n')
            f.write(f'    label="In Progress ({len(in_progress)})";\n')
            f.write('    style=filled;\n')
            f.write('    color=lightyellow;\n')
            for func in in_progress:
                f.write(f'    "{func}" [fillcolor="{colors["in_progress"]}"];\n')
            f.write('  }\n\n')
        
        # High priority (>100 dependents)
        if high_priority:
            f.write('  subgraph cluster_high_priority {\n')
            f.write(f'    label="High Priority - Critical Infrastructure ({len(high_priority)})";\n')
            f.write('    style=filled;\n')
            f.write('    color=lightcoral;\n')
            for func in high_priority:
                dependent_count = sum(1 for f_deps in data['dependencies'].values() if func in f_deps)
                label = f"{func}\\n({dependent_count} deps)"
                f.write(f'    "{func}" [label="{label}", fillcolor="{colors["high_priority"]}", penwidth=3];\n')
            f.write('  }\n\n')
        
        # Available
        if available:
            f.write('  subgraph cluster_available {\n')
            f.write(f'    label="Available ({len(available)})";\n')
            f.write('    style=filled;\n')
            f.write('    color=lightblue;\n')
            for func in available:
                dependent_count = sum(1 for f_deps in data['dependencies'].values() if func in f_deps)
                if dependent_count > 10:
                    label = f"{func}\\n({dependent_count} deps)"
                else:
                    label = func
                f.write(f'    "{func}" [label="{label}", fillcolor="{colors["available"]}"];\n')
            f.write('  }\n\n')
        
        # Add dependency edges
        for func in critical_available:
            if func in data['dependencies']:
                for dep in data['dependencies'][func]:
                    if dep in critical_available:
                        f.write(f'  "{dep}" -> "{func}";\n')
        
        f.write('}\n')
    
    print(f"Critical path graph saved to: {output_file}")
    print(f"Showing {len(critical_available)} critical functions")

def main():
    """Main function with command-line interface."""
    parser = argparse.ArgumentParser(description='Generate hierarchical DOT files from SLATEC dependency analysis')
    parser.add_argument('--data-file', default='dependency_analysis.json',
                       help='JSON file with dependency analysis (default: dependency_analysis.json)')
    parser.add_argument('--hierarchical', action='store_true',
                       help='Generate hierarchical dependency graph')
    parser.add_argument('--critical-path', action='store_true',
                       help='Generate critical path graph')
    parser.add_argument('--all', action='store_true',
                       help='Generate all hierarchical graphs')
    
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
    if args.all or args.hierarchical:
        generate_hierarchical_dot(data)
    
    if args.all or args.critical_path:
        generate_critical_path_dot(data)
    
    # Default behavior
    if not (args.hierarchical or args.critical_path or args.all):
        print("Generating default hierarchical graphs...")
        generate_hierarchical_dot(data)
        generate_critical_path_dot(data)
    
    print("\nTo visualize the DOT files, use:")
    print("  dot -Tpng filename.dot -o filename.png")
    print("  dot -Tsvg filename.dot -o filename.svg")
    print("  dot -Tpdf filename.dot -o filename.pdf")
    
    return 0

if __name__ == '__main__':
    exit(main())
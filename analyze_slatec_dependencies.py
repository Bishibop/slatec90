#!/usr/bin/env python3
"""
SLATEC Dependency Analysis Tool

This script analyzes the complete SLATEC library to create:
1. Comprehensive dependency graph
2. Function categorization by mathematical domain
3. Test coverage mapping
4. Migration priority analysis
"""

import re
import json
import os
from collections import defaultdict, deque
from typing import Dict, List, Set, Tuple
import networkx as nx
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from pathlib import Path

class SLATECAnalyzer:
    def __init__(self, tree_file: str = "tree"):
        self.tree_file = tree_file
        self.dependencies = {}
        self.zero_dependency_functions = set()
        self.function_categories = {}
        self.test_coverage = {}
        self.migration_priorities = {}
        
    def parse_dependency_tree(self):
        """Parse the dependency tree file"""
        print("Parsing dependency tree...")
        
        with open(self.tree_file, 'r') as f:
            lines = f.readlines()
        
        current_function = None
        
        for line in lines:
            line = line.strip()
            if not line:
                continue
                
            # Check if this is a function definition line (starts with function name)
            match = re.match(r'^(\w+)\s+(.+)$', line)
            if match:
                current_function = match.group(1)
                deps_str = match.group(2)
                
                if deps_str == "(NONE)":
                    self.dependencies[current_function] = []
                    self.zero_dependency_functions.add(current_function)
                else:
                    # Parse dependencies (they might span multiple lines)
                    deps = [dep.strip() for dep in deps_str.split() if dep.strip()]
                    self.dependencies[current_function] = deps
                    
            elif current_function and line:
                # This is a continuation line with more dependencies
                deps = [dep.strip() for dep in line.split() if dep.strip()]
                self.dependencies[current_function].extend(deps)
        
        print(f"Parsed {len(self.dependencies)} functions")
        print(f"Found {len(self.zero_dependency_functions)} zero-dependency functions")
        
    def categorize_functions(self):
        """Categorize functions by mathematical domain"""
        print("Categorizing functions by domain...")
        
        # Define categorization patterns
        categories = {
            'Machine Constants': {
                'patterns': [r'.*MACH$', r'PIMACH'],
                'functions': []
            },
            'Error Handling': {
                'patterns': [r'^XER.*', r'FDUMP', r'J4SAVE'],
                'functions': []
            },
            'BLAS Level 1': {
                'patterns': [r'SAXPY', r'DAXPY', r'SCOPY', r'DCOPY', r'SDOT', r'DDOT', 
                           r'SNRM2', r'DNRM2', r'SSCAL', r'DSCAL', r'SASUM', r'DASUM',
                           r'ISAMAX', r'IDAMAX', r'LSAME', r'ENORM', r'DENORM'],
                'functions': []
            },
            'BLAS Level 2': {
                'patterns': [r'SGEMV', r'DGEMV', r'SGBMV', r'DGBMV', r'SGER', r'DGER',
                           r'SSYMV', r'DSYMV', r'SSBMV', r'DSBMV'],
                'functions': []
            },
            'BLAS Level 3': {
                'patterns': [r'SGEMM', r'DGEMM', r'SSYMM', r'DSYMM', r'STRMM', r'DTRMM'],
                'functions': []
            },
            'Bessel Functions': {
                'patterns': [r'BES[IJKY].*', r'BESI.*', r'BESJ.*', r'BESK.*', r'BESY.*'],
                'functions': []
            },
            'Special Functions': {
                'patterns': [r'GAMMA', r'ALNGAM', r'.*GAM.*', r'ERF.*', r'ERFC.*',
                           r'AIRY', r'.*AIRY.*', r'PSI.*'],
                'functions': []
            },
            'Hyperbolic/Trig': {
                'patterns': [r'SINH', r'COSH', r'TANH', r'ASINH', r'ACOSH', r'ATANH'],
                'functions': []
            },
            'Complex Arithmetic': {
                'patterns': [r'^C.*', r'^Z.*', r'CDIV', r'ZDIV', r'CMLT', r'ZMLT',
                           r'ZABS', r'CABS'],
                'functions': []
            },
            'Linear Algebra': {
                'patterns': [r'.*GE[FLS].*', r'.*SV.*', r'.*LU.*', r'.*QR.*',
                           r'SGESL', r'DGESL', r'SGEFA', r'DGEFA'],
                'functions': []
            },
            'Integration': {
                'patterns': [r'QAG.*', r'QAWS.*', r'QAWO.*', r'QAWF.*', r'QNG.*'],
                'functions': []
            },
            'ODE Solvers': {
                'patterns': [r'.*RIV.*', r'ODE.*', r'.*SSL.*', r'.*BDF.*'],
                'functions': []
            },
            'Optimization': {
                'patterns': [r'.*NLS.*', r'.*NSQ.*', r'.*MIN.*', r'.*OPT.*'],
                'functions': []
            },
            'Splines/Interpolation': {
                'patterns': [r'.*SPL.*', r'.*INT.*', r'.*VAL.*', r'AVINT'],
                'functions': []
            },
            'Polynomials': {
                'patterns': [r'POL.*', r'CHEB.*'],
                'functions': []
            },
            'Utilities': {
                'patterns': [r'SORT', r'PYTHAG', r'FDUMP', r'MOUT', r'VOUT'],
                'functions': []
            },
            'Documentation': {
                'patterns': [r'.*DOC$', r'AAAAAA'],
                'functions': []
            }
        }
        
        # Categorize each function
        categorized = set()
        
        for func_name in self.dependencies.keys():
            for category, info in categories.items():
                for pattern in info['patterns']:
                    if re.match(pattern, func_name, re.IGNORECASE):
                        info['functions'].append(func_name)
                        self.function_categories[func_name] = category
                        categorized.add(func_name)
                        break
                if func_name in categorized:
                    break
        
        # Functions that didn't match any pattern
        uncategorized = set(self.dependencies.keys()) - categorized
        categories['Uncategorized'] = {'patterns': [], 'functions': list(uncategorized)}
        
        for func in uncategorized:
            self.function_categories[func] = 'Uncategorized'
        
        # Store the category information
        self.categories = categories
        
        print("Function categorization complete:")
        for category, info in categories.items():
            print(f"  {category}: {len(info['functions'])} functions")
    
    def analyze_test_coverage(self):
        """Analyze which functions are covered by test programs"""
        print("Analyzing test coverage...")
        
        test_files = [f"test{i:02d}.f" for i in range(1, 55)]
        
        for test_file in test_files:
            if os.path.exists(test_file):
                self.test_coverage[test_file] = self._extract_functions_from_test(test_file)
        
        print(f"Analyzed {len(self.test_coverage)} test files")
    
    def _extract_functions_from_test(self, test_file: str) -> List[str]:
        """Extract function names called from a test file"""
        functions = []
        try:
            with open(test_file, 'r') as f:
                content = f.read().upper()
                
                # Look for CALL statements and function references
                call_matches = re.findall(r'CALL\s+(\w+)', content)
                functions.extend(call_matches)
                
                # Look for external function references
                external_matches = re.findall(r'EXTERNAL\s+(\w+)', content)
                functions.extend(external_matches)
                
                # Look for function assignments
                func_matches = re.findall(r'(\w+)\s*=\s*(\w+)\s*\(', content)
                functions.extend([match[1] for match in func_matches])
                
        except FileNotFoundError:
            pass
        
        return list(set(functions))
    
    def calculate_migration_priorities(self):
        """Calculate migration priorities based on dependencies and test enablement"""
        print("Calculating migration priorities...")
        
        # Create dependency graph
        G = nx.DiGraph()
        for func, deps in self.dependencies.items():
            G.add_node(func)
            for dep in deps:
                G.add_edge(dep, func)  # dep -> func (dep is required for func)
        
        priorities = {}
        
        for func in self.dependencies.keys():
            priority_score = 0
            
            # Factor 1: Number of functions that depend on this one (importance)
            dependents = len([f for f in self.dependencies.keys() 
                            if func in self.dependencies[f]])
            priority_score += dependents * 10
            
            # Factor 2: Inverse of number of dependencies (easier to implement)
            num_deps = len(self.dependencies[func])
            if num_deps == 0:
                priority_score += 100  # Zero dependencies are highest priority
            else:
                priority_score += max(0, 50 - num_deps * 2)
            
            # Factor 3: Test enablement potential
            test_impact = sum(1 for test_funcs in self.test_coverage.values() 
                            if func in test_funcs)
            priority_score += test_impact * 5
            
            # Factor 4: Category bonus
            category = self.function_categories.get(func, 'Uncategorized')
            category_bonuses = {
                'Machine Constants': 50,
                'Error Handling': 40,
                'BLAS Level 1': 30,
                'Utilities': 20,
                'Complex Arithmetic': 15,
                'Special Functions': 10
            }
            priority_score += category_bonuses.get(category, 0)
            
            priorities[func] = {
                'score': priority_score,
                'dependents': dependents,
                'dependencies': num_deps,
                'test_impact': test_impact,
                'category': category
            }
        
        self.migration_priorities = priorities
        
        # Sort by priority score
        sorted_priorities = sorted(priorities.items(), 
                                 key=lambda x: x[1]['score'], 
                                 reverse=True)
        
        print("Top 20 migration priorities:")
        for i, (func, info) in enumerate(sorted_priorities[:20]):
            print(f"  {i+1:2d}. {func:12s} (score: {info['score']:3d}, "
                  f"deps: {info['dependencies']:2d}, "
                  f"dependents: {info['dependents']:2d}, "
                  f"tests: {info['test_impact']:2d})")
    
    def generate_comprehensive_report(self):
        """Generate comprehensive analysis report"""
        report = {
            'summary': {
                'total_functions': len(self.dependencies),
                'zero_dependency_functions': len(self.zero_dependency_functions),
                'categories': len(self.categories),
                'test_files_analyzed': len(self.test_coverage)
            },
            'categories': {},
            'zero_dependency_functions': list(self.zero_dependency_functions),
            'migration_priorities': self.migration_priorities,
            'test_coverage': self.test_coverage
        }
        
        # Add category details
        for category, info in self.categories.items():
            report['categories'][category] = {
                'count': len(info['functions']),
                'functions': info['functions']
            }
        
        return report
    
    def save_analysis(self, filename: str = "slatec_comprehensive_analysis.json"):
        """Save analysis results to JSON file"""
        report = self.generate_comprehensive_report()
        
        with open(filename, 'w') as f:
            json.dump(report, f, indent=2)
        
        print(f"Analysis saved to {filename}")
    
    def create_dependency_graph_visualization(self, output_file: str = "slatec_dependencies.png"):
        """Create a visual dependency graph (for smaller subsets)"""
        print("Creating dependency graph visualization...")
        
        # Create graph for zero-dependency and level-1 functions only
        G = nx.DiGraph()
        
        # Add zero-dependency functions
        level_0 = list(self.zero_dependency_functions)[:20]  # Limit for visualization
        
        # Add level-1 functions (depend only on level-0)
        level_1 = []
        for func, deps in self.dependencies.items():
            if len(deps) > 0 and all(dep in self.zero_dependency_functions for dep in deps):
                if len(level_1) < 30:  # Limit for visualization
                    level_1.append(func)
        
        # Build graph
        for func in level_0 + level_1:
            G.add_node(func)
            if func in self.dependencies:
                for dep in self.dependencies[func]:
                    if dep in level_0 + level_1:
                        G.add_edge(dep, func)
        
        # Create visualization
        plt.figure(figsize=(16, 12))
        
        # Position nodes using hierarchical layout
        pos = nx.spring_layout(G, k=3, iterations=50)
        
        # Color nodes by category
        colors = {
            'Machine Constants': 'red',
            'Error Handling': 'orange',
            'BLAS Level 1': 'blue',
            'Utilities': 'green',
            'Complex Arithmetic': 'purple',
            'Special Functions': 'brown',
            'Uncategorized': 'gray'
        }
        
        node_colors = [colors.get(self.function_categories.get(node, 'Uncategorized'), 'gray') 
                      for node in G.nodes()]
        
        # Draw the graph
        nx.draw(G, pos, 
                node_color=node_colors,
                node_size=1000,
                font_size=8,
                font_weight='bold',
                arrows=True,
                arrowsize=20,
                edge_color='gray',
                alpha=0.8)
        
        # Add labels
        nx.draw_networkx_labels(G, pos, font_size=6)
        
        # Create legend
        legend_elements = [mpatches.Patch(color=color, label=category) 
                          for category, color in colors.items()]
        plt.legend(handles=legend_elements, loc='upper left', bbox_to_anchor=(1, 1))
        
        plt.title("SLATEC Dependency Graph (Level 0 & 1 Functions)", fontsize=16)
        plt.tight_layout()
        plt.savefig(output_file, dpi=300, bbox_inches='tight')
        plt.close()
        
        print(f"Dependency graph saved to {output_file}")

def main():
    analyzer = SLATECAnalyzer()
    
    # Run complete analysis
    analyzer.parse_dependency_tree()
    analyzer.categorize_functions()
    analyzer.analyze_test_coverage()
    analyzer.calculate_migration_priorities()
    
    # Generate outputs
    analyzer.save_analysis()
    analyzer.create_dependency_graph_visualization()
    
    # Print summary
    print("\n" + "="*60)
    print("SLATEC COMPREHENSIVE ANALYSIS COMPLETE")
    print("="*60)
    
    report = analyzer.generate_comprehensive_report()
    print(f"Total Functions: {report['summary']['total_functions']}")
    print(f"Zero Dependencies: {report['summary']['zero_dependency_functions']}")
    print(f"Categories: {report['summary']['categories']}")
    
    print("\nFunction Categories:")
    for category, info in report['categories'].items():
        print(f"  {category}: {info['count']} functions")
    
    print(f"\nTop Zero-Dependency Functions (Migration Candidates):")
    zero_deps_with_scores = [(func, analyzer.migration_priorities[func]['score']) 
                           for func in analyzer.zero_dependency_functions]
    zero_deps_sorted = sorted(zero_deps_with_scores, key=lambda x: x[1], reverse=True)
    
    for i, (func, score) in enumerate(zero_deps_sorted[:15]):
        category = analyzer.function_categories.get(func, 'Unknown')
        print(f"  {i+1:2d}. {func:12s} (score: {score:3d}) [{category}]")

if __name__ == "__main__":
    main()
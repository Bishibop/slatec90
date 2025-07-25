#!/usr/bin/env python3
"""
Analyze SLATEC functions and categorize by complexity levels with detailed output.
"""

import os
import re
import json
from collections import defaultdict
from typing import Dict, List, Set, Tuple

class FunctionAnalyzer:
    def __init__(self):
        self.functions = {}
        self.complexity_levels = {
            0: {"name": "Trivial", "functions": [], "description": "< 5 params, no arrays/state, pure math"},
            1: {"name": "Simple", "functions": [], "description": "< 10 params, simple arrays, may have SAVE"},
            2: {"name": "Moderate", "functions": [], "description": "Work arrays, COMMON blocks, iterative"},
            3: {"name": "Complex", "functions": [], "description": "EXTERNAL params, complex arrays"},
            4: {"name": "Stateful", "functions": [], "description": "INDEX pattern, sequential calls"},
        }
        
    def analyze_file(self, filepath: str) -> Dict:
        """Analyze a single Fortran file."""
        with open(filepath, 'r', encoding='latin-1') as f:
            content = f.read()
            
        # Extract function/subroutine name - handle various formats
        func_patterns = [
            r'^\s*(?:SUBROUTINE|FUNCTION)\s+(\w+)',
            r'^\s*(?:REAL|INTEGER|DOUBLE\s+PRECISION|COMPLEX|LOGICAL)\s+FUNCTION\s+(\w+)',
            r'^\s*(?:DOUBLE\s+COMPLEX)\s+FUNCTION\s+(\w+)'
        ]
        
        func_match = None
        for pattern in func_patterns:
            func_match = re.search(pattern, content, re.MULTILINE | re.IGNORECASE)
            if func_match:
                break
                
        if not func_match:
            # Check if this might be a BLOCK DATA
            block_match = re.search(r'^\s*BLOCK\s+DATA\s*(\w*)', content, re.MULTILINE | re.IGNORECASE)
            if block_match:
                return None  # Skip BLOCK DATA
            return None
            
        func_name = func_match.group(1).upper()
        
        # Extract parameters
        param_match = re.search(r'^\s*(?:SUBROUTINE|FUNCTION)\s+\w+\s*\((.*?)\)', 
                               content, re.MULTILINE | re.IGNORECASE | re.DOTALL)
        parameters = []
        if param_match:
            param_str = param_match.group(1)
            # Split by comma but handle continuation lines
            param_str = re.sub(r'\s+', ' ', param_str)
            parameters = [p.strip() for p in param_str.split(',') if p.strip()]
            
        # Check for various features
        has_external = bool(re.search(r'^\s*EXTERNAL\s+', content, re.MULTILINE | re.IGNORECASE))
        has_common = bool(re.search(r'^\s*COMMON\s*/\w+/', content, re.MULTILINE | re.IGNORECASE))
        has_save = bool(re.search(r'^\s*SAVE\s+', content, re.MULTILINE | re.IGNORECASE))
        
        # Check for DIMENSION statements (arrays)
        dimension_matches = re.findall(r'^\s*(?:DIMENSION|REAL|INTEGER|DOUBLE\s+PRECISION|COMPLEX)\s+.*?\((.*?)\)', 
                                      content, re.MULTILINE | re.IGNORECASE)
        has_arrays = len(dimension_matches) > 0
        has_complex_arrays = any('*' in dim or '+' in dim or '-' in dim for dim in dimension_matches)
        
        # Check for work arrays (common pattern: WORK(*) or WORK(LWORK))
        has_work_arrays = bool(re.search(r'\bWORK\s*\(', content, re.IGNORECASE))
        
        # Check for INDEX pattern - more comprehensive search
        index_patterns = [
            r'\bINDEX\s*\.EQ\.\s*[12]',
            r'\bIF\s*\(\s*INDEX\s*\.EQ\.\s*1\s*\)',
            r'\bIF\s*\(\s*INDEX\s*-\s*1\s*\)',
            r'\bINDEX\s*=\s*[12]\b',
            r'\bGO\s+TO\s*\(\s*\d+\s*,\s*\d+\s*\)\s*,?\s*INDEX'
        ]
        has_index_pattern = any(re.search(pattern, content, re.IGNORECASE) for pattern in index_patterns)
        
        # Check for iterative algorithms (DO loops)
        do_loops = len(re.findall(r'^\s*DO\s+\d+', content, re.MULTILINE | re.IGNORECASE))
        
        # Extract purpose from prologue
        purpose_match = re.search(r'\*\*\*PURPOSE\s+(.+?)(?:\n\*\*\*|\n\s*C)', content, re.DOTALL)
        purpose = purpose_match.group(1).strip() if purpose_match else ""
        purpose = re.sub(r'^\s*C\s*', '', purpose, flags=re.MULTILINE)
        purpose = ' '.join(purpose.split())[:100] + "..." if len(purpose) > 100 else purpose
        
        return {
            'name': func_name,
            'filepath': filepath,
            'param_count': len(parameters),
            'parameters': parameters,
            'has_external': has_external,
            'has_common': has_common,
            'has_save': has_save,
            'has_arrays': has_arrays,
            'has_complex_arrays': has_complex_arrays,
            'has_work_arrays': has_work_arrays,
            'has_index_pattern': has_index_pattern,
            'do_loops': do_loops,
            'purpose': purpose
        }
        
    def categorize_function(self, func_info: Dict) -> int:
        """Categorize function into complexity level 0-4."""
        # Level 4: Stateful (INDEX pattern)
        if func_info['has_index_pattern']:
            return 4
            
        # Level 3: Complex (EXTERNAL parameters or complex patterns)
        if func_info['has_external']:
            return 3
            
        # Check for complex work arrays with formulas
        if func_info['has_work_arrays'] and (func_info['has_common'] or func_info['do_loops'] > 3):
            return 3
            
        # Level 2: Moderate (work arrays, many parameters, iterative)
        if func_info['has_work_arrays'] or func_info['has_complex_arrays']:
            return 2
            
        if func_info['param_count'] >= 10:
            return 2
            
        if func_info['has_common'] and func_info['do_loops'] > 1:
            return 2
            
        # Level 1: Simple (simple arrays, moderate parameters)
        if func_info['has_arrays'] or func_info['has_save']:
            if func_info['param_count'] < 10 and func_info['do_loops'] <= 2:
                return 1
            return 2
            
        # Level 0: Trivial (few parameters, no state)
        if func_info['param_count'] < 5 and not func_info['has_save'] and func_info['do_loops'] <= 1:
            return 0
            
        return 1  # Default to Simple
        
    def analyze_directory(self, directory: str):
        """Analyze all .f files in directory."""
        for root, dirs, files in os.walk(directory):
            for file in files:
                if file.endswith('.f'):
                    filepath = os.path.join(root, file)
                    try:
                        func_info = self.analyze_file(filepath)
                        if func_info:
                            level = self.categorize_function(func_info)
                            func_info['level'] = level
                            self.functions[func_info['name']] = func_info
                            self.complexity_levels[level]['functions'].append(func_info)
                    except Exception as e:
                        print(f"Error analyzing {filepath}: {e}")
                        
    def save_results(self, output_file: str):
        """Save detailed results to JSON file."""
        results = {
            'total_functions': len(self.functions),
            'complexity_levels': {}
        }
        
        for level, info in self.complexity_levels.items():
            results['complexity_levels'][level] = {
                'name': info['name'],
                'description': info['description'],
                'count': len(info['functions']),
                'percentage': (len(info['functions']) / len(self.functions) * 100) if self.functions else 0,
                'functions': sorted([{
                    'name': f['name'],
                    'params': f['param_count'],
                    'features': {
                        'external': f['has_external'],
                        'common': f['has_common'],
                        'save': f['has_save'],
                        'work_arrays': f['has_work_arrays'],
                        'index_pattern': f['has_index_pattern']
                    },
                    'purpose': f['purpose']
                } for f in info['functions']], key=lambda x: x['name'])
            }
            
        with open(output_file, 'w') as f:
            json.dump(results, f, indent=2)
            
    def print_summary(self):
        """Print analysis summary."""
        print("SLATEC Function Complexity Analysis")
        print("=" * 80)
        print(f"Total functions analyzed: {len(self.functions)}")
        print()
        
        for level, info in self.complexity_levels.items():
            count = len(info['functions'])
            percentage = (count / len(self.functions) * 100) if self.functions else 0
            print(f"Level {level} - {info['name']}: {count} functions ({percentage:.1f}%)")
            print(f"  Description: {info['description']}")
            
        print("\n" + "=" * 80)
        print("\nDetailed Examples from Each Level:")
        print("=" * 80)
        
        for level, info in self.complexity_levels.items():
            print(f"\nLevel {level} - {info['name']}:")
            print(f"Description: {info['description']}")
            print("-" * 80)
            
            # Show up to 15 examples
            examples = sorted(info['functions'], key=lambda x: x['name'])[:15]
            for func_info in examples:
                print(f"  {func_info['name']:12} | {func_info['param_count']:2} params", end="")
                
                features = []
                if func_info['has_external']:
                    features.append("EXTERNAL")
                if func_info['has_common']:
                    features.append("COMMON")
                if func_info['has_save']:
                    features.append("SAVE")
                if func_info['has_work_arrays']:
                    features.append("WORK")
                if func_info['has_index_pattern']:
                    features.append("INDEX")
                    
                if features:
                    print(f" | {', '.join(features):20}", end="")
                else:
                    print(f" | {'':20}", end="")
                    
                if func_info['purpose']:
                    print(f" | {func_info['purpose'][:50]}...")
                else:
                    print()

if __name__ == "__main__":
    analyzer = FunctionAnalyzer()
    
    # Analyze src directory
    src_dir = "/Users/nicholasmullen/Code/gauntlet/slatec_test/src"
    if os.path.exists(src_dir):
        analyzer.analyze_directory(src_dir)
    else:
        print(f"Directory not found: {src_dir}")
        
    analyzer.print_summary()
    
    # Save detailed results
    analyzer.save_results("slatec_complexity_analysis.json")
    print(f"\nDetailed results saved to: slatec_complexity_analysis.json")
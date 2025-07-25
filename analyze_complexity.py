#!/usr/bin/env python3
"""
Analyze SLATEC functions and categorize by complexity levels.
"""

import os
import re
from collections import defaultdict
from typing import Dict, List, Set, Tuple

class FunctionAnalyzer:
    def __init__(self):
        self.functions = {}
        self.complexity_levels = {
            0: {"name": "Trivial", "functions": []},
            1: {"name": "Simple", "functions": []},
            2: {"name": "Moderate", "functions": []},
            3: {"name": "Complex", "functions": []},
            4: {"name": "Stateful", "functions": []},
        }
        
    def analyze_file(self, filepath: str) -> Dict:
        """Analyze a single Fortran file."""
        with open(filepath, 'r', encoding='latin-1') as f:
            content = f.read()
            
        # Extract function/subroutine name
        func_match = re.search(r'^\s*(?:SUBROUTINE|FUNCTION)\s+(\w+)', content, re.MULTILINE | re.IGNORECASE)
        if not func_match:
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
            'content_preview': content[:500]
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
                            self.complexity_levels[level]['functions'].append(func_info['name'])
                    except Exception as e:
                        print(f"Error analyzing {filepath}: {e}")
                        
    def print_summary(self):
        """Print analysis summary."""
        print("SLATEC Function Complexity Analysis")
        print("=" * 60)
        print(f"Total functions analyzed: {len(self.functions)}")
        print()
        
        for level, info in self.complexity_levels.items():
            count = len(info['functions'])
            percentage = (count / len(self.functions) * 100) if self.functions else 0
            print(f"Level {level} - {info['name']}: {count} functions ({percentage:.1f}%)")
            
        print("\n" + "=" * 60)
        print("\nExamples from each level:")
        print("=" * 60)
        
        for level, info in self.complexity_levels.items():
            print(f"\nLevel {level} - {info['name']}:")
            # Show up to 10 examples
            examples = sorted(info['functions'])[:10]
            for func_name in examples:
                func_info = self.functions[func_name]
                print(f"  - {func_name}: {func_info['param_count']} params", end="")
                features = []
                if func_info['has_external']:
                    features.append("EXTERNAL")
                if func_info['has_common']:
                    features.append("COMMON")
                if func_info['has_save']:
                    features.append("SAVE")
                if func_info['has_work_arrays']:
                    features.append("WORK arrays")
                if func_info['has_index_pattern']:
                    features.append("INDEX pattern")
                if features:
                    print(f" [{', '.join(features)}]")
                else:
                    print()
                    
        # Print detailed statistics
        print("\n" + "=" * 60)
        print("\nDetailed Feature Statistics:")
        print("=" * 60)
        
        feature_counts = {
            'has_external': 0,
            'has_common': 0,
            'has_save': 0,
            'has_arrays': 0,
            'has_work_arrays': 0,
            'has_index_pattern': 0
        }
        
        for func_info in self.functions.values():
            for feature in feature_counts:
                if func_info[feature]:
                    feature_counts[feature] += 1
                    
        for feature, count in feature_counts.items():
            percentage = (count / len(self.functions) * 100) if self.functions else 0
            feature_name = feature.replace('has_', '').replace('_', ' ').title()
            print(f"{feature_name}: {count} functions ({percentage:.1f}%)")

if __name__ == "__main__":
    analyzer = FunctionAnalyzer()
    
    # Analyze src directory
    src_dir = "/Users/nicholasmullen/Code/gauntlet/slatec_test/src"
    if os.path.exists(src_dir):
        analyzer.analyze_directory(src_dir)
    else:
        print(f"Directory not found: {src_dir}")
        
    analyzer.print_summary()
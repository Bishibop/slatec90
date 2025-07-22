#!/usr/bin/env python3
"""
Find ideal functions to start migration with
"""

import glob
import os

def analyze_functions():
    no_deps = []
    simple_deps = []
    
    for f77_file in glob.glob('src/*.f'):
        func_name = os.path.basename(f77_file).replace('.f', '')
        
        try:
            with open(f77_file, 'r') as f:
                content = f.read()
                
            # Skip precision variants for now
            if func_name.startswith(('d', 'c', 'z')) and func_name[1:] + '.f' in os.listdir('src'):
                continue
                
            # Look for dependencies
            if '***ROUTINES CALLED  (NONE)' in content:
                # Also check if it's a simple function (not too long)
                lines = content.split('\n')
                if len(lines) < 200:  # Short functions are easier
                    no_deps.append((func_name, len(lines)))
                    
            elif '***ROUTINES CALLED' in content:
                for i, line in enumerate(content.split('\n')):
                    if '***ROUTINES CALLED' in line:
                        next_line = content.split('\n')[i+1] if i+1 < len(content.split('\n')) else ''
                        deps = next_line.replace('C', '').strip()
                        # Count dependencies
                        dep_list = [d.strip() for d in deps.split(',') if d.strip()]
                        if len(dep_list) == 1 and len(content.split('\n')) < 200:
                            simple_deps.append((func_name, dep_list[0], len(content.split('\n'))))
                        break
                        
        except:
            pass
    
    # Sort by line count (shorter first)
    no_deps.sort(key=lambda x: x[1])
    simple_deps.sort(key=lambda x: x[2])
    
    print("Functions with NO dependencies (good starting points):")
    print("-" * 50)
    for func, lines in no_deps[:10]:
        print(f"{func:15} ({lines:3} lines)")
    
    print("\nFunctions with ONE dependency:")
    print("-" * 50)
    for func, dep, lines in simple_deps[:10]:
        print(f"{func:15} -> {dep:15} ({lines:3} lines)")
        
    # Write to file for reference
    with open('ideal_starting_functions.txt', 'w') as f:
        f.write("NO DEPENDENCIES:\n")
        for func, lines in no_deps:
            f.write(f"{func}\n")
        f.write("\nONE DEPENDENCY:\n")
        for func, dep, lines in simple_deps:
            f.write(f"{func} -> {dep}\n")

if __name__ == '__main__':
    analyze_functions()
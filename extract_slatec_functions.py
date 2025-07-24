#!/usr/bin/env python3
"""
Extract function names and descriptions from SLATEC library source files.
"""

import os
import re
from pathlib import Path
import json

def extract_function_info(file_path):
    """Extract function/subroutine name and purpose from a Fortran file."""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception as e:
        print(f"Error reading {file_path}: {e}")
        return None
    
    # Find the subroutine/function declaration
    func_match = re.search(r'^\s*(?:SUBROUTINE|FUNCTION)\s+(\w+)', content, re.MULTILINE | re.IGNORECASE)
    if not func_match:
        return None
    
    func_name = func_match.group(1).upper()
    
    # Find the PURPOSE line - look for the pattern with proper handling of multiline
    purpose_match = re.search(r'^C\*\*\*PURPOSE\s+(.+?)$', content, re.MULTILINE)
    if not purpose_match:
        # Try alternative patterns
        purpose_match = re.search(r'^C\s*PURPOSE\s*[-:]?\s*(.+?)$', content, re.MULTILINE | re.IGNORECASE)
    
    if purpose_match:
        purpose = purpose_match.group(1).strip()
        # Handle multi-line purposes - continue reading lines after the match
        start_pos = purpose_match.end()
        lines = content[start_pos:].split('\n')
        
        for line in lines:
            # Check if it's a continuation line (starts with C and has multiple spaces)
            if re.match(r'^C\s{12,}', line):  # Continuation line with proper indentation
                continuation_text = re.sub(r'^C\s+', '', line).strip()
                if continuation_text:  # Only add non-empty continuations
                    purpose += ' ' + continuation_text
            elif re.match(r'^C\*\*\*', line):  # Next section marker
                break
            elif line.strip() == 'C' or line.strip() == '':  # Empty comment or blank line
                continue
            else:  # Any other pattern means end of PURPOSE section
                break
        
        # Clean up the purpose text
        purpose = ' '.join(purpose.split())  # Normalize whitespace
        purpose = purpose.rstrip('.')  # Remove trailing periods for consistency
        
        return {
            'name': func_name,
            'purpose': purpose,
            'file': os.path.basename(file_path)
        }
    
    return {
        'name': func_name,
        'purpose': 'No purpose description found',
        'file': os.path.basename(file_path)
    }

def main():
    src_dir = Path('/Users/nicholasmullen/Code/gauntlet/slatec_test/src')
    functions = {}
    
    # Get all .f files
    f_files = sorted(src_dir.glob('*.f'))
    
    print(f"Found {len(f_files)} Fortran files to process...")
    
    for f_file in f_files:
        if f_file.name == 'changes':  # Skip non-Fortran files
            continue
            
        info = extract_function_info(f_file)
        if info:
            functions[info['name']] = {
                'purpose': info['purpose'],
                'file': info['file']
            }
    
    # Sort functions alphabetically
    sorted_functions = dict(sorted(functions.items()))
    
    # Save to JSON file
    output_file = src_dir.parent / 'slatec_functions.json'
    with open(output_file, 'w') as f:
        json.dump(sorted_functions, f, indent=2)
    
    print(f"\nExtracted {len(sorted_functions)} functions")
    print(f"Results saved to: {output_file}")
    
    # Also create a human-readable text file
    text_file = src_dir.parent / 'slatec_functions.txt'
    with open(text_file, 'w') as f:
        f.write("SLATEC Library Functions\n")
        f.write("=" * 80 + "\n\n")
        
        for name, info in sorted_functions.items():
            f.write(f"{name}\n")
            f.write(f"  Purpose: {info['purpose']}\n")
            f.write(f"  File: {info['file']}\n")
            f.write("\n")
    
    print(f"Human-readable list saved to: {text_file}")
    
    # Print first 10 as examples
    print("\nFirst 10 functions as examples:")
    print("-" * 80)
    for i, (name, info) in enumerate(sorted_functions.items()):
        if i >= 10:
            break
        print(f"{name}: {info['purpose']}")

if __name__ == '__main__':
    main()
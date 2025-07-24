#!/usr/bin/env python3
"""
Extract missing SLATEC functions that were skipped due to different PURPOSE format.
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
    func_match = re.search(r'^\s*(?:SUBROUTINE|FUNCTION|DOUBLE PRECISION FUNCTION|REAL FUNCTION|INTEGER FUNCTION|LOGICAL FUNCTION|COMPLEX FUNCTION)\s+(\w+)', 
                          content, re.MULTILINE | re.IGNORECASE)
    if not func_match:
        # Try to find just the function name after FUNCTION keyword
        func_match = re.search(r'FUNCTION\s+(\w+)', content, re.IGNORECASE)
        if not func_match:
            print(f"No function/subroutine found in {file_path}")
            return None
    
    func_name = func_match.group(1).upper()
    
    # Find the PURPOSE line - handle both C*** and C***** patterns
    purpose_match = re.search(r'^C\*{3,5}PURPOSE\s+(.+?)$', content, re.MULTILINE)
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
            elif re.match(r'^C\*{3,}', line):  # Next section marker
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
    # List of missing files
    missing_files = [
        'cv.f', 'd1mach.f', 'd1mach_ieee.f', 'dbvalu.f', 'dcv.f', 'ddanrm.f',
        'denorm.f', 'dgamln.f', 'dgamrn.f', 'dhvnrm.f', 'dppval.f', 'dprvec.f',
        'dpsixn.f', 'dqdota.f', 'dqdoti.f', 'dqwgtc.f', 'dqwgtf.f', 'dqwgts.f',
        'drc.f', 'drd.f', 'drf.f', 'drj.f', 'dvnrms.f', 'dwnlt2.f', 'dxpsi.f',
        'enorm.f', 'gamln.f', 'gamrn.f', 'i1mach.f', 'idloc.f', 'iploc.f',
        'lamw.f', 'lsame.f', 'pythag.f', 'qwgtc.f', 'qwgtf.f', 'qwgts.f',
        'r1mach.f', 'r1mach_ieee.f', 'rc.f', 'rd.f', 'rf.f', 'rj.f', 'sdanrm.f',
        'vnwrms.f', 'wnlt2.f', 'xpsi.f', 'zabs.f'
    ]
    
    src_dir = Path('/Users/nicholasmullen/Code/gauntlet/slatec_test/src')
    missing_functions = {}
    
    print(f"Processing {len(missing_files)} missing files...")
    
    for filename in missing_files:
        f_file = src_dir / filename
        if f_file.exists():
            info = extract_function_info(f_file)
            if info:
                missing_functions[info['name']] = {
                    'purpose': info['purpose'],
                    'file': info['file']
                }
                print(f"Extracted {info['name']} from {filename}")
            else:
                print(f"Failed to extract from {filename}")
        else:
            print(f"File not found: {filename}")
    
    # Sort functions alphabetically
    sorted_functions = dict(sorted(missing_functions.items()))
    
    # Save to JSON file
    output_file = src_dir.parent / 'missing_functions.json'
    with open(output_file, 'w') as f:
        json.dump(sorted_functions, f, indent=2)
    
    print(f"\nExtracted {len(sorted_functions)} missing functions")
    print(f"Results saved to: {output_file}")
    
    # Also create a human-readable text file
    text_file = src_dir.parent / 'missing_functions.txt'
    with open(text_file, 'w') as f:
        f.write("Missing SLATEC Functions\n")
        f.write("=" * 80 + "\n\n")
        
        for name, info in sorted_functions.items():
            f.write(f"{name}\n")
            f.write(f"  Purpose: {info['purpose']}\n")
            f.write(f"  File: {info['file']}\n")
            f.write("\n")
    
    print(f"Human-readable list saved to: {text_file}")
    
    # Print all extracted functions
    print("\nExtracted functions:")
    print("-" * 80)
    for name, info in sorted_functions.items():
        print(f"{name}: {info['purpose']}")

if __name__ == '__main__':
    main()
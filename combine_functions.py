#!/usr/bin/env python3
"""
Combine the original extracted functions with the missing ones.
"""

import json

def main():
    # Load original functions
    with open('slatec_functions.json', 'r') as f:
        original_functions = json.load(f)
    
    # Load missing functions
    with open('missing_functions.json', 'r') as f:
        missing_functions = json.load(f)
    
    # Combine them
    all_functions = original_functions.copy()
    
    # Add missing functions, noting duplicates
    duplicates = []
    for name, info in missing_functions.items():
        if name in all_functions:
            duplicates.append({
                'name': name,
                'original_file': all_functions[name]['file'],
                'new_file': info['file']
            })
        else:
            all_functions[name] = info
    
    # Sort alphabetically
    sorted_functions = dict(sorted(all_functions.items()))
    
    # Save combined functions
    with open('slatec_functions_complete.json', 'w') as f:
        json.dump(sorted_functions, f, indent=2)
    
    print(f"Original functions: {len(original_functions)}")
    print(f"Missing functions found: {len(missing_functions)}")
    print(f"Total unique functions: {len(sorted_functions)}")
    print(f"\nExpected total: 690 + 46 = 736")
    
    if duplicates:
        print(f"\nFound {len(duplicates)} duplicate function names:")
        for dup in duplicates:
            print(f"  {dup['name']}: {dup['original_file']} and {dup['new_file']}")
    
    # Create summary file
    with open('slatec_extraction_summary.txt', 'w') as f:
        f.write("SLATEC Function Extraction Summary\n")
        f.write("=" * 80 + "\n\n")
        f.write(f"Total .f files in src directory: 738\n")
        f.write(f"Functions extracted initially: 690\n")
        f.write(f"Missing files identified: 48\n")
        f.write(f"Missing functions extracted: 46\n")
        f.write(f"Total unique functions: {len(sorted_functions)}\n\n")
        
        f.write("Reason for discrepancy:\n")
        f.write("- 2 duplicate function names (D1MACH and R1MACH) each have two implementations\n")
        f.write("  - d1mach.f and d1mach_ieee.f both define D1MACH\n")
        f.write("  - r1mach.f and r1mach_ieee.f both define R1MACH\n\n")
        
        f.write("Missing functions were not extracted initially because:\n")
        f.write("- They use C***PURPOSE instead of C*****PURPOSE format\n")
        f.write("- The original extraction script only looked for the 5-asterisk pattern\n\n")
        
        f.write("Files with missing functions:\n")
        for name, info in sorted(missing_functions.items()):
            f.write(f"  {info['file']}: {name}\n")

if __name__ == '__main__':
    main()
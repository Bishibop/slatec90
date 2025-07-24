#!/usr/bin/env python3
"""Create a complete wall poster-friendly markdown file of ALL SLATEC functions."""

import json
import textwrap

def truncate_description(desc, max_length=80):
    """Truncate description to fit within column width."""
    if len(desc) <= max_length:
        return desc
    return desc[:max_length-3] + "..."

def create_wall_poster():
    # Load the complete function data
    with open('slatec_functions_complete.json', 'r') as f:
        functions = json.load(f)
    
    # Sort functions alphabetically
    sorted_functions = sorted(functions.items(), key=lambda x: x[0].upper())
    
    # Create markdown content
    content = []
    content.append("# SLATEC Function Library Reference - Complete Edition")
    content.append("")
    content.append("*Complete alphabetical listing of all 736 SLATEC mathematical functions*")
    content.append("")
    content.append("---")
    content.append("")
    
    # Create three-column layout
    # Divide functions into three roughly equal groups
    total = len(sorted_functions)
    per_column = (total + 2) // 3  # Round up
    
    column1 = sorted_functions[:per_column]
    column2 = sorted_functions[per_column:2*per_column]
    column3 = sorted_functions[2*per_column:]
    
    # Create header
    content.append("| Function | Description | Function | Description | Function | Description |")
    content.append("|----------|-------------|----------|-------------|----------|-------------|")
    
    # Create rows
    max_rows = max(len(column1), len(column2), len(column3))
    
    for i in range(max_rows):
        row_parts = []
        
        # Column 1
        if i < len(column1):
            name, info = column1[i]
            desc = truncate_description(info['purpose'], 40)
            row_parts.extend([f"**{name}**", desc])
        else:
            row_parts.extend(["", ""])
            
        # Column 2
        if i < len(column2):
            name, info = column2[i]
            desc = truncate_description(info['purpose'], 40)
            row_parts.extend([f"**{name}**", desc])
        else:
            row_parts.extend(["", ""])
            
        # Column 3
        if i < len(column3):
            name, info = column3[i]
            desc = truncate_description(info['purpose'], 40)
            row_parts.extend([f"**{name}**", desc])
        else:
            row_parts.extend(["", ""])
        
        content.append("| " + " | ".join(row_parts) + " |")
    
    content.append("")
    content.append("---")
    content.append("")
    content.append(f"**Total Functions: {total}**")
    content.append("")
    content.append("*SLATEC (Sandia, Los Alamos, Air Force Weapons Laboratory Technical Exchange Committee) Mathematical Library*")
    
    # Write to file
    with open('SLATEC_COMPLETE_WALL_POSTER.md', 'w') as f:
        f.write('\n'.join(content))
    
    print(f"Created SLATEC_COMPLETE_WALL_POSTER.md with {total} functions")
    
    # Also create a more detailed single-column version for reference
    content2 = []
    content2.append("# SLATEC Function Library - Complete Detailed Reference")
    content2.append("")
    content2.append("*Complete alphabetical listing with full descriptions - All 736 Functions*")
    content2.append("")
    
    current_letter = ''
    for name, info in sorted_functions:
        first_letter = name[0].upper()
        if first_letter != current_letter:
            current_letter = first_letter
            content2.append(f"\n## {current_letter}\n")
        
        content2.append(f"### {name}")
        content2.append(f"**Purpose:** {info['purpose']}")
        content2.append(f"**Source:** `{info['file']}`")
        content2.append("")
    
    with open('SLATEC_COMPLETE_DETAILED_REFERENCE.md', 'w') as f:
        f.write('\n'.join(content2))
    
    print(f"Also created SLATEC_COMPLETE_DETAILED_REFERENCE.md for detailed reference")

if __name__ == "__main__":
    create_wall_poster()
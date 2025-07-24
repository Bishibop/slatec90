#!/usr/bin/env python3
"""Create a truly printable wall poster of SLATEC functions."""

import json
import textwrap

def create_printable_poster():
    # Load the complete function data
    with open('slatec_functions_complete.json', 'r') as f:
        functions = json.load(f)
    
    # Sort functions alphabetically
    sorted_functions = sorted(functions.items(), key=lambda x: x[0].upper())
    
    # Create different format options
    
    # Format 1: Simple two-column list (function name + very brief description)
    content1 = []
    content1.append("# SLATEC Function Library Quick Reference")
    content1.append("")
    content1.append("*All 736 SLATEC mathematical functions*")
    content1.append("")
    
    # Group by first letter
    current_letter = ''
    for name, info in sorted_functions:
        first_letter = name[0].upper()
        if first_letter != current_letter:
            if current_letter:  # Add spacing between sections
                content1.append("")
            current_letter = first_letter
            content1.append(f"## {current_letter}")
            content1.append("")
        
        # Extract just the first part of the description
        desc = info['purpose']
        # Get first sentence or clause
        desc_parts = desc.split('.')
        short_desc = desc_parts[0]
        if len(short_desc) > 60:
            # Try to break at a comma or other natural break
            for sep in [',', ' for ', ' to ', ' of ', ' using ', ' with ']:
                if sep in short_desc[:60]:
                    short_desc = short_desc.split(sep)[0]
                    break
            else:
                # Just truncate
                short_desc = short_desc[:57] + "..."
        
        content1.append(f"**{name}** - {short_desc}")
    
    with open('SLATEC_POSTER_SIMPLE.md', 'w') as f:
        f.write('\n'.join(content1))
    
    # Format 2: Create a grid layout using HTML (which many markdown renderers support)
    content2 = []
    content2.append("# SLATEC Function Library Grid Reference")
    content2.append("")
    content2.append("*All 736 SLATEC mathematical functions in grid format*")
    content2.append("")
    content2.append("<style>")
    content2.append(".grid-container {")
    content2.append("  display: grid;")
    content2.append("  grid-template-columns: repeat(3, 1fr);")
    content2.append("  gap: 10px;")
    content2.append("  font-size: 10pt;")
    content2.append("}")
    content2.append(".function-box {")
    content2.append("  border: 1px solid #ddd;")
    content2.append("  padding: 5px;")
    content2.append("  background: #f9f9f9;")
    content2.append("}")
    content2.append(".func-name {")
    content2.append("  font-weight: bold;")
    content2.append("  color: #0066cc;")
    content2.append("}")
    content2.append(".func-desc {")
    content2.append("  font-size: 9pt;")
    content2.append("  color: #333;")
    content2.append("}")
    content2.append("</style>")
    content2.append("")
    content2.append("<div class='grid-container'>")
    
    for name, info in sorted_functions:
        desc = info['purpose']
        # Truncate to first 80 chars or first sentence
        if '.' in desc[:80]:
            short_desc = desc.split('.')[0] + '.'
        else:
            short_desc = desc[:77] + "..." if len(desc) > 80 else desc
            
        content2.append(f"<div class='function-box'>")
        content2.append(f"<div class='func-name'>{name}</div>")
        content2.append(f"<div class='func-desc'>{short_desc}</div>")
        content2.append(f"</div>")
    
    content2.append("</div>")
    
    with open('SLATEC_POSTER_GRID.md', 'w') as f:
        f.write('\n'.join(content2))
    
    # Format 3: Plain text columnar format (best for actual printing)
    content3 = []
    content3.append("SLATEC FUNCTION LIBRARY REFERENCE")
    content3.append("=================================")
    content3.append("Complete listing of all 736 functions")
    content3.append("")
    
    # Calculate layout
    total = len(sorted_functions)
    columns = 4  # 4 columns for landscape printing
    per_column = (total + columns - 1) // columns
    
    # Create columns
    cols = []
    for i in range(columns):
        start = i * per_column
        end = min(start + per_column, total)
        cols.append(sorted_functions[start:end])
    
    # Print row by row
    for row in range(per_column):
        line_parts = []
        for col_idx in range(columns):
            if row < len(cols[col_idx]):
                name, info = cols[col_idx][row]
                # Format: "FUNCNAME - Brief desc"
                entry = f"{name:8s}"
                line_parts.append(entry)
            else:
                line_parts.append(" " * 8)
        
        content3.append("  ".join(line_parts))
    
    content3.append("")
    content3.append(f"Total: {total} functions")
    
    with open('SLATEC_POSTER_PLAINTEXT.txt', 'w') as f:
        f.write('\n'.join(content3))
    
    # Format 4: Create a compact reference card style
    content4 = []
    content4.append("# SLATEC Quick Reference Card")
    content4.append("")
    
    categories = {
        "Bessel Functions": [],
        "Integration/Quadrature": [],
        "Linear Algebra": [],
        "Interpolation/Splines": [],
        "Differential Equations": [],
        "Special Functions": [],
        "Optimization": [],
        "FFT": [],
        "Other": []
    }
    
    # Categorize functions based on keywords
    for name, info in sorted_functions:
        desc_lower = info['purpose'].lower()
        categorized = False
        
        if any(word in desc_lower for word in ['bessel', 'airy']):
            categories["Bessel Functions"].append(name)
            categorized = True
        elif any(word in desc_lower for word in ['integral', 'integrate', 'quadrature']):
            categories["Integration/Quadrature"].append(name)
            categorized = True
        elif any(word in desc_lower for word in ['matrix', 'linear system', 'eigenvalue', 'factorization']):
            categories["Linear Algebra"].append(name)
            categorized = True
        elif any(word in desc_lower for word in ['interpolat', 'spline', 'b-spline']):
            categories["Interpolation/Splines"].append(name)
            categorized = True
        elif any(word in desc_lower for word in ['differential equation', 'ode']):
            categories["Differential Equations"].append(name)
            categorized = True
        elif any(word in desc_lower for word in ['gamma', 'psi', 'elliptic', 'exponential integral']):
            categories["Special Functions"].append(name)
            categorized = True
        elif any(word in desc_lower for word in ['minimiz', 'optimiz', 'least squares']):
            categories["Optimization"].append(name)
            categorized = True
        elif 'fft' in desc_lower or 'fourier' in desc_lower:
            categories["FFT"].append(name)
            categorized = True
        
        if not categorized:
            categories["Other"].append(name)
    
    # Output by category
    for category, funcs in categories.items():
        if funcs:
            content4.append(f"## {category} ({len(funcs)} functions)")
            content4.append("")
            # Print in columns within category
            for i in range(0, len(funcs), 6):  # 6 per line
                line = "  ".join(f"{func:10s}" for func in funcs[i:i+6])
                content4.append(line)
            content4.append("")
    
    with open('SLATEC_POSTER_CATEGORIZED.md', 'w') as f:
        f.write('\n'.join(content4))
    
    print("Created multiple poster formats:")
    print("1. SLATEC_POSTER_SIMPLE.md - Simple list with brief descriptions")
    print("2. SLATEC_POSTER_GRID.md - HTML grid layout")
    print("3. SLATEC_POSTER_PLAINTEXT.txt - Plain text columns for printing")
    print("4. SLATEC_POSTER_CATEGORIZED.md - Functions grouped by category")

if __name__ == "__main__":
    create_printable_poster()
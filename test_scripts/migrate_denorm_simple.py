#!/usr/bin/env python3
"""
Simple test - just convert denorm to modern Fortran
"""

from llm_config import call_llm
from strip_markdown import strip_markdown_blocks

# Read denorm source
with open("src/denorm.f", "r") as f:
    f77_source = f.read()

print("Converting DENORM to modern Fortran...")

# Generate modern version
prompt = f"""Convert this FORTRAN 77 function to modern Fortran (2018 standard).

Requirements:
1. Create a module named denorm_module
2. Use modern Fortran features:
   - implicit none
   - intent declarations
   - assumed-shape arrays where appropriate
   - no GOTO statements
3. Preserve EXACT numerical behavior (keep the three-sum algorithm)
4. Include F77-compatible wrapper function at the end

Original F77 code:
{f77_source}

Generate ONLY the complete modern Fortran code. No explanations or markdown.
"""

response = call_llm(prompt, model="o3-mini")
modern_code = strip_markdown_blocks(response)

# Save it
with open("modern/denorm_simple.f90", "w") as f:
    f.write(modern_code)

print("\nSaved to modern/denorm_simple.f90")

# Try to compile it
import subprocess
try:
    result = subprocess.run([
        "gfortran", "-std=f2018", "-c", 
        "modern/denorm_simple.f90",
        "-o", "modern/denorm_simple.o"
    ], capture_output=True, text=True)
    
    if result.returncode == 0:
        print("✓ Compilation successful!")
    else:
        print("✗ Compilation failed:")
        print(result.stderr)
except Exception as e:
    print(f"Error: {e}")
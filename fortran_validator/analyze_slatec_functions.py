#!/usr/bin/env python3
import os
import re
from collections import defaultdict

# Analyze SLATEC functions by category
categories = defaultdict(list)
purposes = {}

src_dir = "../src"
for filename in os.listdir(src_dir):
    if filename.endswith('.f'):
        func_name = filename[:-2]
        with open(os.path.join(src_dir, filename), 'r') as f:
            content = f.read()
            
            # Extract PURPOSE
            purpose_match = re.search(r'^C\*\*\*PURPOSE\s+(.*)$', content, re.MULTILINE)
            if purpose_match:
                purpose = purpose_match.group(1).strip()
                purposes[func_name] = purpose
                
                # Categorize based on keywords
                purpose_lower = purpose.lower()
                if 'bessel' in purpose_lower:
                    categories['Bessel Functions'].append(func_name)
                elif 'airy' in purpose_lower:
                    categories['Airy Functions'].append(func_name)
                elif 'integral' in purpose_lower or 'quadrature' in purpose_lower:
                    categories['Integration/Quadrature'].append(func_name)
                elif 'spline' in purpose_lower or 'interpolat' in purpose_lower:
                    categories['Interpolation/Splines'].append(func_name)
                elif 'differential equation' in purpose_lower or ' ode' in purpose_lower:
                    categories['Differential Equations'].append(func_name)
                elif 'linear' in purpose_lower and ('system' in purpose_lower or 'equation' in purpose_lower):
                    categories['Linear Algebra'].append(func_name)
                elif 'eigenvalue' in purpose_lower or 'eigenvector' in purpose_lower:
                    categories['Eigenvalue Problems'].append(func_name)
                elif 'fft' in purpose_lower or 'fourier' in purpose_lower:
                    categories['FFT/Fourier Transform'].append(func_name)
                elif 'error' in purpose_lower and ('handling' in purpose_lower or 'message' in purpose_lower):
                    categories['Error Handling'].append(func_name)
                elif 'machine constant' in purpose_lower:
                    categories['Machine Constants'].append(func_name)
                elif 'complex' in purpose_lower and ('arithmetic' in purpose_lower or 'quotient' in purpose_lower):
                    categories['Complex Arithmetic'].append(func_name)
                elif 'random' in purpose_lower or 'uniform' in purpose_lower:
                    categories['Random Numbers'].append(func_name)
                elif 'sort' in purpose_lower:
                    categories['Sorting'].append(func_name)
                elif 'documentation' in purpose_lower or 'disclaimer' in purpose_lower:
                    categories['Documentation'].append(func_name)
                elif 'subsidiary' in purpose_lower:
                    categories['Helper/Subsidiary'].append(func_name)
                else:
                    categories['Other'].append(func_name)

# Print summary
print("SLATEC Function Categories:")
print("=" * 60)
total = 0
for category, funcs in sorted(categories.items()):
    print(f"\n{category}: {len(funcs)} functions")
    if len(funcs) <= 10:
        print(f"  {', '.join(sorted(funcs))}")
    else:
        print(f"  {', '.join(sorted(funcs)[:10])}, ...")
    total += len(funcs)

print(f"\nTotal categorized: {total}")
print(f"Total available: {len(os.listdir(src_dir))}")

# Show some examples from "Other" category
print("\nSample 'Other' category functions:")
for func in sorted(categories['Other'])[:20]:
    if func in purposes:
        print(f"  {func}: {purposes[func][:60]}...")
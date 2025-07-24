#!/usr/bin/env python3
"""Check why CSHCH validation claims 100% pass"""

import sys

# Read the validation code and check for CSHCH handling
with open('slatec_test_helper.py', 'r') as f:
    content = f.read()
    
# Find the validate_modern method
start = content.find('def validate_modern(self):')
end = content.find('\n    def ', start + 1)
validate_method = content[start:end]

# Check if CSHCH is handled in the validation comparison
if 'elif self.func_name == "CSHCH"' in validate_method:
    print("CSHCH validation logic found")
else:
    print("WARNING: CSHCH validation logic NOT FOUND in validate_modern method!")
    print("\nThe method only handles these functions:")
    import re
    pattern = r'elif self\.func_name == "(\w+)"'
    matches = re.findall(pattern, validate_method)
    for func in matches:
        print(f"  - {func}")
    
    print("\nThis explains why validation shows 100% - it's not actually checking CSHCH results!")
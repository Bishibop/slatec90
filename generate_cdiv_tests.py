#!/usr/bin/env python3
"""Generate comprehensive test cases for CDIV (complex division)"""

import json
import math

def generate_cdiv_test_cases():
    """Generate test cases for complex division (ar,ai)/(br,bi) = (cr,ci)"""
    
    test_cases = []
    
    # Basic test cases
    # (1+0i) / (1+0i) = 1+0i
    test_cases.append({
        "description": "Real divided by real (1/1)",
        "inputs": [1.0, 0.0, 1.0, 0.0],
        "expected": None  # Will be filled by F77
    })
    
    # (2+0i) / (1+0i) = 2+0i  
    test_cases.append({
        "description": "Real divided by real (2/1)",
        "inputs": [2.0, 0.0, 1.0, 0.0],
        "expected": None
    })
    
    # (0+i) / (0+i) = 1+0i
    test_cases.append({
        "description": "Pure imaginary divided by itself",
        "inputs": [0.0, 1.0, 0.0, 1.0],
        "expected": None
    })
    
    # (1+i) / (1+0i) = 1+i
    test_cases.append({
        "description": "Complex divided by real",
        "inputs": [1.0, 1.0, 1.0, 0.0],
        "expected": None
    })
    
    # (1+0i) / (0+i) = 0-i
    test_cases.append({
        "description": "Real divided by pure imaginary",
        "inputs": [1.0, 0.0, 0.0, 1.0],
        "expected": None
    })
    
    # (1+i) / (1+i) = 1+0i
    test_cases.append({
        "description": "Complex divided by itself",
        "inputs": [1.0, 1.0, 1.0, 1.0],
        "expected": None
    })
    
    # (2+3i) / (4+5i) 
    test_cases.append({
        "description": "General complex division",
        "inputs": [2.0, 3.0, 4.0, 5.0],
        "expected": None
    })
    
    # Test with negative values
    test_cases.append({
        "description": "Negative real parts",
        "inputs": [-3.0, 4.0, -1.0, 2.0],
        "expected": None
    })
    
    # Edge cases with very small divisor
    test_cases.append({
        "description": "Small divisor",
        "inputs": [1.0, 0.0, 1e-10, 0.0],
        "expected": None
    })
    
    # Large numbers
    test_cases.append({
        "description": "Large dividend",
        "inputs": [1e10, 1e10, 2.0, 3.0],
        "expected": None
    })
    
    # Classic test: (3+4i) / (1+2i)
    # Should give (11-2i)/5 = 2.2-0.4i
    test_cases.append({
        "description": "Classic example (3+4i)/(1+2i)",
        "inputs": [3.0, 4.0, 1.0, 2.0],
        "expected": None
    })
    
    # Test symmetry: i / 1 = 0+i
    test_cases.append({
        "description": "i divided by 1",
        "inputs": [0.0, 1.0, 1.0, 0.0],
        "expected": None
    })
    
    # Test conjugate: (a+bi) / (a-bi)
    test_cases.append({
        "description": "Division by conjugate",
        "inputs": [3.0, 4.0, 3.0, -4.0],
        "expected": None
    })
    
    # More edge cases
    values = [0.0, 1.0, -1.0, 0.5, -0.5, 2.0, -2.0, 10.0, -10.0]
    
    # Generate combinations
    for ar in values[:5]:
        for ai in values[:5]:
            for br in values[1:5]:  # Skip 0 to avoid division by zero
                for bi in values[:3]:
                    if abs(br) + abs(bi) > 0:  # Ensure divisor is non-zero
                        test_cases.append({
                            "description": f"({ar:g}+{ai:g}i) / ({br:g}+{bi:g}i)",
                            "inputs": [ar, ai, br, bi],
                            "expected": None
                        })
    
    # Special patterns
    angles = [0, 30, 45, 60, 90, 120, 135, 150, 180, 210, 225, 240, 270, 300, 315, 330]
    for angle in angles:
        rad = math.radians(angle)
        # Create complex number with magnitude 5
        ar = 5 * math.cos(rad)
        ai = 5 * math.sin(rad)
        # Divide by unit complex number
        br = math.cos(math.radians(45))
        bi = math.sin(math.radians(45))
        test_cases.append({
            "description": f"Magnitude 5 at {angle}° / unit at 45°",
            "inputs": [ar, ai, br, bi],
            "expected": None
        })
    
    # Near-zero divisor tests (but not exactly zero)
    test_cases.append({
        "description": "Very small divisor magnitude",
        "inputs": [1.0, 1.0, 1e-15, 1e-15],
        "expected": None
    })
    
    # Equal real and imaginary parts
    for val in [1.0, 2.0, 3.0, 5.0, 7.0]:
        test_cases.append({
            "description": f"Equal parts ({val}+{val}i) / (1+1i)",
            "inputs": [val, val, 1.0, 1.0],
            "expected": None
        })
    
    return test_cases

# Generate test cases
test_cases = generate_cdiv_test_cases()
print(f"Generated {len(test_cases)} test cases for CDIV")

# Save to file
with open('cdiv_test_cases.json', 'w') as f:
    json.dump(test_cases, f, indent=2)

print("Test cases saved to cdiv_test_cases.json")
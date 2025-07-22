#!/usr/bin/env python3
"""Generate hundreds of test cases for PYTHAG"""

import json
import math
import random

def generate_pythag_tests():
    """Generate comprehensive test cases for PYTHAG"""
    tests = []
    
    # 1. Known Pythagorean triples (scaled versions too)
    triples = [(3,4,5), (5,12,13), (8,15,17), (7,24,25), (20,21,29)]
    for a, b, c in triples:
        for scale in [1, 0.1, 0.01, 10, 100, 1000]:
            tests.append({
                "inputs": [a*scale, b*scale],
                "description": f"Pythagorean triple ({a},{b},{c}) scaled by {scale}",
                "expected": c*scale
            })
    
    # 2. Powers of 2
    for i in range(-10, 11):
        val = 2**i
        tests.append({
            "inputs": [val, val],
            "description": f"Equal values: 2^{i}",
            "expected": val * math.sqrt(2)
        })
    
    # 3. One zero, one non-zero
    for val in [1, -1, 0.1, -0.1, 10, -10, 100, -100]:
        tests.append({
            "inputs": [0, val],
            "description": f"Zero and {val}",
            "expected": abs(val)
        })
        tests.append({
            "inputs": [val, 0],
            "description": f"{val} and zero",
            "expected": abs(val)
        })
    
    # 4. Near machine epsilon
    epsilon = 1.19e-7  # Single precision epsilon
    for factor in [0.5, 1, 2, 10]:
        val = epsilon * factor
        tests.append({
            "inputs": [val, val],
            "description": f"Near epsilon: {factor}*epsilon",
            "expected": val * math.sqrt(2)
        })
    
    # 5. Large values near overflow
    # Single precision max is ~3.4e38
    for exp in [35, 36, 37, 38]:
        val = 10**exp
        tests.append({
            "inputs": [val, 0],
            "description": f"Large value: 10^{exp}",
            "expected": val
        })
    
    # 6. Small values near underflow  
    # Single precision min normalized is ~1.18e-38
    for exp in [-35, -36, -37, -38]:
        val = 10**exp
        tests.append({
            "inputs": [val, val],
            "description": f"Small value: 10^{exp}",
            "expected": val * math.sqrt(2)
        })
    
    # 7. Random values - uniform distribution
    random.seed(42)  # Reproducible
    for i in range(50):
        a = random.uniform(-1000, 1000)
        b = random.uniform(-1000, 1000)
        tests.append({
            "inputs": [a, b],
            "description": f"Random uniform #{i+1}",
            "expected": math.sqrt(a**2 + b**2)
        })
    
    # 8. Random values - log scale
    for i in range(50):
        exp_a = random.uniform(-10, 10)
        exp_b = random.uniform(-10, 10)
        sign_a = random.choice([-1, 1])
        sign_b = random.choice([-1, 1])
        a = sign_a * (10 ** exp_a)
        b = sign_b * (10 ** exp_b)
        tests.append({
            "inputs": [a, b],
            "description": f"Random log scale #{i+1}",
            "expected": math.sqrt(a**2 + b**2)
        })
    
    # 9. Special patterns
    # Values that might cause issues in the iterative algorithm
    tests.extend([
        {"inputs": [1, 1e-20], "description": "Huge ratio", "expected": 1.0},
        {"inputs": [1e-20, 1], "description": "Tiny ratio", "expected": 1.0},
        {"inputs": [1e20, 1e-20], "description": "Extreme ratio", "expected": 1e20},
    ])
    
    # 10. Negative values (all combinations)
    for a, b in [(1, 2), (3, 4), (5, 12)]:
        for sign_a in [-1, 1]:
            for sign_b in [-1, 1]:
                tests.append({
                    "inputs": [sign_a * a, sign_b * b],
                    "description": f"Signs: ({sign_a}, {sign_b}) with ({a}, {b})",
                    "expected": math.sqrt(a**2 + b**2)
                })
    
    return tests

# Generate and save
tests = generate_pythag_tests()
print(f"Generated {len(tests)} test cases")

# Show a sample
print("\nFirst 5 tests:")
for i, test in enumerate(tests[:5]):
    print(f"{i+1}. {test['description']}")
    print(f"   Inputs: {test['inputs']}")
    print(f"   Expected: {test['expected']}")

print("\nLast 5 tests:")
for i, test in enumerate(tests[-5:], len(tests)-4):
    print(f"{i}. {test['description']}")
    print(f"   Inputs: {test['inputs']}")
    print(f"   Expected: {test['expected']}")

# Save to JSON
with open('pythag_test_cases.json', 'w') as f:
    json.dump(tests, f, indent=2)
print(f"\nSaved {len(tests)} test cases to pythag_test_cases.json")
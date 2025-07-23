#!/usr/bin/env python3
"""
Compare original and enhanced DENORM test suites
"""
import json

# Load original tests
with open('test_data/denorm_tests.json') as f:
    original = json.load(f)

# Load enhanced tests  
with open('test_data/denorm_tests_enhanced.json') as f:
    enhanced = json.load(f)

print("DENORM Test Suite Enhancement Summary")
print("=" * 50)
print(f"Original test count: {original['total_tests']}")
print(f"Enhanced test count: {enhanced['total_tests']}")
print(f"New tests added: {enhanced['total_tests'] - original['total_tests']}")
print(f"Percentage increase: {((enhanced['total_tests'] - original['total_tests']) / original['total_tests'] * 100):.1f}%")

# Analyze test categories
print("\nNew Test Categories Added:")
print("-" * 30)

new_test_start = original['total_tests']
categories = {
    "Subnormal/Denormal": 0,
    "IEEE Boundaries": 0,
    "Machine Epsilon": 0,
    "Large Vectors (N>100)": 0,
    "Mathematical Constants": 0,
    "Random/Statistical": 0,
    "Physical/Scientific": 0,
    "Extreme Ranges": 0
}

for test in enhanced['test_cases'][new_test_start:]:
    desc = test['description'].lower()
    n = test['n']
    
    if 'subnormal' in desc or 'denormal' in desc:
        categories["Subnormal/Denormal"] += 1
    if 'ieee' in desc or 'dbl_' in desc:
        categories["IEEE Boundaries"] += 1
    if 'epsilon' in desc or 'precision' in desc:
        categories["Machine Epsilon"] += 1
    if n > 100:
        categories["Large Vectors (N>100)"] += 1
    if any(x in desc for x in ['chebyshev', 'taylor', 'fibonacci', 'prime']):
        categories["Mathematical Constants"] += 1
    if any(x in desc for x in ['random', 'normal', 'uniform', 'exponential', 'cauchy']):
        categories["Random/Statistical"] += 1
    if any(x in desc for x in ['velocity', 'electric', 'physical']):
        categories["Physical/Scientific"] += 1
    if any(x in desc for x in ['1e-', '1e+']) and any(x in desc for x in ['100', '150', '200', '300']):
        categories["Extreme Ranges"] += 1

for category, count in sorted(categories.items(), key=lambda x: x[1], reverse=True):
    if count > 0:
        print(f"  {category}: {count} tests")

# Show vector size distribution
print("\nVector Size Distribution (New Tests):")
print("-" * 30)
size_dist = {}
for test in enhanced['test_cases'][new_test_start:]:
    n = test['n']
    if n == 0:
        key = "N=0"
    elif n == 1:
        key = "N=1"
    elif 2 <= n <= 5:
        key = "N=2-5"
    elif 6 <= n <= 10:
        key = "N=6-10"
    elif 11 <= n <= 50:
        key = "N=11-50"
    elif 51 <= n <= 100:
        key = "N=51-100"
    else:
        key = "N>100"
    
    size_dist[key] = size_dist.get(key, 0) + 1

for size, count in sorted(size_dist.items()):
    print(f"  {size}: {count} tests")

print("\nEnhancement Complete!")
print("The new test suite provides comprehensive coverage of:")
print("- Extreme edge cases and IEEE boundaries")
print("- Precision and rounding error scenarios")
print("- Large-scale vectors and statistical distributions")
print("- Mathematical identities and physical problems")
print("- Algorithm stress tests and numerical stability")
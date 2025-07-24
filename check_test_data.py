import json
import numpy as np

# Load the test data
with open('test_data/cshch_tests.json', 'r') as f:
    cshch_data = json.load(f)

# Check for issues in CSHCH tests
print("=== CSHCH Test Data Analysis ===\n")

# Find test cases with both real and imaginary parts
complex_cases = []
for test in cshch_data['test_cases']:
    inputs = test['inputs']
    if inputs[0] != 0.0 and inputs[1] != 0.0:
        complex_cases.append(test)
        if len(complex_cases) >= 5:
            break

print('Complex number test cases (with both real and imaginary parts):')
for test in complex_cases:
    print(f'\nTest {test["test_id"]}: {test["description"]}')
    print(f'  Input: z = {test["inputs"][0]} + {test["inputs"][1]}i')
    print(f'  Expected sinh: {test["expected"]["sinh_real"]:.6f} + {test["expected"]["sinh_imag"]:.6f}i')
    print(f'  Expected cosh: {test["expected"]["cosh_real"]:.6f} + {test["expected"]["cosh_imag"]:.6f}i')

# Check specific problematic case
print("\n\n=== Checking Specific Issue ===")
print("For z = 0 + i*π/2:")
z = 1j * np.pi/2
sinh_z = np.sinh(z)
cosh_z = np.cosh(z)
print(f'  Numpy sinh(z) = {sinh_z.real:.10e} + {sinh_z.imag:.10f}i')
print(f'  Numpy cosh(z) = {cosh_z.real:.10e} + {cosh_z.imag:.10f}i')

# Find this test case
for test in cshch_data['test_cases']:
    if abs(test['inputs'][1] - np.pi/2) < 1e-10 and test['inputs'][0] == 0.0:
        print(f'\n  Test data expects:')
        print(f'  sinh: {test["expected"]["sinh_real"]:.10e} + {test["expected"]["sinh_imag"]:.10f}i')
        print(f'  cosh: {test["expected"]["cosh_real"]:.10e} + {test["expected"]["cosh_imag"]:.10f}i')
        print(f'\n  Error in cosh_real: {abs(cosh_z.real - test["expected"]["cosh_real"]):.10e}')
        break

# Check BSPLVN test data
print("\n\n=== BSPLVN Test Data Analysis ===\n")

with open('test_data/bsplvn_tests.json', 'r') as f:
    bsplvn_data = json.load(f)

# Check a few cubic B-spline cases
print("Checking cubic B-spline case (uniform knots [0,1,2,3,4,5], x=2.5):")
for test in bsplvn_data['test_cases']:
    if test['description'] == "Cubic B-spline, uniform knots [0,1,2,3,4,5], x=2.5":
        print(f"  Expected values: {test['expected']}")
        # For cubic B-splines, values should sum to 1
        total = sum(test['expected'])
        print(f"  Sum of basis functions: {total:.10f} (should be 1.0)")
        break

# Check for negative values (B-splines should be non-negative)
print("\n\nChecking for negative B-spline values:")
negative_found = False
for test in bsplvn_data['test_cases']:
    for val in test['expected']:
        if val < 0:
            print(f"  ISSUE: Negative value {val} in test {test['test_id']}: {test['description']}")
            negative_found = True
            break
    if negative_found:
        break

if not negative_found:
    print("  No negative values found (good!)")
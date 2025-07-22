#!/usr/bin/env python3
"""
Generate high-precision test cases for Lambert W function implementation.
Uses scipy.special.lambertw for reference values.
"""

import numpy as np
from scipy.special import lambertw
import math

def generate_test_cases():
    print("Lambert W Function - High Precision Test Cases")
    print("=" * 50)
    
    # Special values
    print("\nSPECIAL VALUES:")
    print(f"W(0) = {float(lambertw(0, k=0).real):.16f}")
    print(f"W(e) = {float(lambertw(math.e, k=0).real):.16f}")
    print(f"W(-1/e) = {float(lambertw(-1/math.e, k=0).real):.16f}")
    print(f"W(1) = {float(lambertw(1, k=0).real):.16f} (Omega constant)")
    
    # Principal branch W_0 test cases
    print("\nPRINCIPAL BRANCH W_0(x) for x >= -1/e:")
    print(f"{'x':>15} {'W_0(x)':>20} {'Verification':>20}")
    print("-" * 55)
    
    x_values = [-1/math.e, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.5, 
                1.0, 2.0, math.e, 5.0, 10.0, 20.0, 50.0, 100.0, 
                1000.0, 10000.0, 100000.0]
    
    for x in x_values:
        w = float(lambertw(x, k=0).real)
        # Verify: x = W(x) * exp(W(x))
        verify = w * np.exp(w)
        print(f"{x:15.10f} {w:20.16f} {verify:20.16f}")
    
    # Branch W_{-1} test cases
    print("\nBRANCH W_{-1}(x) for -1/e <= x < 0:")
    print(f"{'x':>15} {'W_{-1}(x)':>20} {'Verification':>20}")
    print("-" * 55)
    
    x_values_branch = [-1/math.e, -0.35, -0.3, -0.25, -0.2, -0.15, 
                       -0.1, -0.05, -0.01, -0.001, -0.0001, -0.00001]
    
    for x in x_values_branch:
        w = float(lambertw(x, k=-1).real)
        verify = w * np.exp(w)
        print(f"{x:15.10f} {w:20.16f} {verify:20.16f}")
    
    # Series expansion coefficients near x=0
    print("\nSERIES EXPANSION NEAR x=0:")
    print("W(x) = x - x^2 + 3/2*x^3 - 8/3*x^4 + 125/24*x^5 - ...")
    print("\nVerification for small x:")
    for x in [0.001, 0.01, 0.1]:
        w_exact = float(lambertw(x, k=0).real)
        w_series = x - x**2 + 1.5*x**3 - 8/3*x**4 + 125/24*x**5
        error = abs(w_exact - w_series)
        print(f"x={x:6.3f}: exact={w_exact:.10f}, series={w_series:.10f}, error={error:.2e}")
    
    # Derivative test cases
    print("\nDERIVATIVE TEST CASES:")
    print("W'(x) = W(x) / (x * (1 + W(x))) for x != 0")
    print(f"{'x':>10} {'W(x)':>15} {'Wprime numeric':>15} {'Wprime formula':>15}")
    print("-" * 55)
    
    for x in [0.1, 0.5, 1.0, 2.0, 5.0]:
        w = float(lambertw(x, k=0).real)
        # Numerical derivative
        h = 1e-8
        w_plus = float(lambertw(x + h, k=0).real)
        w_minus = float(lambertw(x - h, k=0).real)
        deriv_numeric = (w_plus - w_minus) / (2 * h)
        # Analytical derivative
        deriv_formula = w / (x * (1 + w))
        print(f"{x:10.6f} {w:15.10f} {deriv_numeric:15.10f} {deriv_formula:15.10f}")
    
    # Edge cases and error conditions
    print("\nEDGE CASES:")
    print(f"Smallest valid input: x = -1/e = {-1/math.e:.16f}")
    print(f"At branch point: W_0(-1/e) = W_{{-1}}(-1/e) = -1")
    print("\nFor x < -1/e: No real solutions exist")
    
    # Fortran array initialization format
    print("\nFORTRAN ARRAY INITIALIZATION:")
    print("For principal branch W_0:")
    x_fort = [-0.3678794, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.5, 
              1.0, 2.0, 2.718282, 5.0, 10.0, 20.0, 50.0, 100.0]
    
    print("      DATA XTEST /", end="")
    for i, x in enumerate(x_fort):
        if i % 4 == 0 and i > 0:
            print()
            print("     &           ", end="")
        print(f" {x:9.6f}", end="")
        if i < len(x_fort) - 1:
            print(",", end="")
    print(" /")
    
    print("\n      DATA W0TEST /", end="")
    for i, x in enumerate(x_fort):
        w = float(lambertw(x, k=0).real)
        if i % 4 == 0 and i > 0:
            print()
            print("     &            ", end="")
        print(f" {w:9.6f}", end="")
        if i < len(x_fort) - 1:
            print(",", end="")
    print(" /")

if __name__ == "__main__":
    generate_test_cases()
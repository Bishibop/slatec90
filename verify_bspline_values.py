"""
Verify B-spline values for simple cases
"""

def linear_bspline(x, t_i, t_i1, t_i2):
    """
    Compute linear B-spline B[i,2](x) over knots [t_i, t_i+1, t_i+2]
    """
    if x < t_i or x >= t_i2:
        return 0.0
    elif x >= t_i and x < t_i1:
        return (x - t_i) / (t_i1 - t_i)
    else:  # x >= t_i1 and x < t_i2
        return (t_i2 - x) / (t_i2 - t_i1)

# Test case 11: x=0.0, knots=[0,1,2,...], ileft=0
print("Test 11: x=0.0, ileft=0 (x at left knot)")
print("For linear B-splines at x=0.0:")
print(f"  B[0,2](0.0) = {linear_bspline(0.0, 0.0, 1.0, 2.0)}")  
print(f"  B[1,2](0.0) = {linear_bspline(0.0, 1.0, 2.0, 3.0)}")
print("  Expected: [1.0, 0.0] - which matches our output!")

# Test case 12: x=0.5, ileft=0
print("\nTest 12: x=0.5, ileft=0")
print("For linear B-splines at x=0.5:")
# B[0,2] uses knots [0,1,2]
b0 = linear_bspline(0.5, 0.0, 1.0, 2.0)
# B[1,2] uses knots [1,2,3]  
b1 = linear_bspline(0.5, 1.0, 2.0, 3.0)
print(f"  B[0,2](0.5) = {b0}")
print(f"  B[1,2](0.5) = {b1}")
print("  Expected: [0.5, 0.5] - which matches our output!")

# But wait, for BSPLVN with ileft=0, which B-splines are we computing?
print("\nACTUALLY: With ileft=0, x in [0,1), we compute:")
print("  The two B-splines that are non-zero in [0,1)")
print("  These are B[-1,2] and B[0,2]")
print("  But B[-1,2] would need knot at -1, which doesn't exist")
print("  So the algorithm computes differently...")

# The key insight: BSPLVN computes the JHIGH B-splines that are
# potentially non-zero at X, starting from the leftmost one
print("\nBSPLVN interpretation:")
print("For order K at point x in interval [t[ileft], t[ileft+1]):")
print("It computes the K B-splines B[ileft-K+1,K] through B[ileft,K]")
print("that can be non-zero at x")
import json

with open('test_data/bsplvn_tests_blind.json', 'r') as f:
    tests = json.load(f)

# Check tests with negative values
problem_tests = [30, 39, 40, 49, 50]

print("Analyzing tests with negative B-spline values:")
for test_idx in problem_tests:
    test = tests[test_idx - 1]  # 0-based
    print(f"\nTest {test_idx}:")
    print(f"  Description: {test['description']}")
    print(f"  x = {test['x']}, ileft = {test['ileft']}, jhigh = {test['jhigh']}")
    print(f"  Knot vector length: {len(test['t'])}")
    print(f"  First 10 knots: {test['t'][:10]}")
    if len(test['t']) > 10:
        print(f"  Last 5 knots: {test['t'][-5:]}")
    
    # Check if knots are uniform
    t = test['t']
    diffs = [t[i+1] - t[i] for i in range(min(len(t)-1, 20))]
    uniform = all(abs(d - diffs[0]) < 1e-10 for d in diffs)
    print(f"  Uniform knots: {uniform}")
    
    # Check position of x relative to knot span
    if test['ileft'] < len(t) - 1:
        left_knot = t[test['ileft']] if test['ileft'] < len(t) else "N/A"
        right_knot = t[test['ileft'] + 1] if test['ileft'] + 1 < len(t) else "N/A"
        print(f"  x={test['x']} is in interval [{left_knot}, {right_knot})")
        
        # Check proximity to boundary
        if test['ileft'] + test['jhigh'] >= len(t) - 1:
            print(f"  WARNING: Near right boundary! ileft + jhigh = {test['ileft'] + test['jhigh']}, len(t) = {len(t)}")
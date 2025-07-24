import json

with open('test_data/bsplvn_tests_blind.json', 'r') as f:
    tests = json.load(f)

# Check test 11 (index 10)
test = tests[10]
print(f"Test 11 details:")
print(f"  Description: {test['description']}")
print(f"  x = {test['x']}")
print(f"  ileft = {test['ileft']}")
print(f"  jhigh = {test['jhigh']}")
print(f"  vnikx_size = {test['vnikx_size']}")
print(f"  knots = {test['t']}")

# For x=0.0, ileft=0, jhigh=2
# This should compute linear B-splines
# But x=0.0 is exactly at the knot!
print(f"\nx={test['x']} is at knot boundary t[{test['ileft']}]={test['t'][test['ileft']]}")
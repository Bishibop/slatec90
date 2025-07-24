import json

with open('test_data/bsplvn_tests_blind.json', 'r') as f:
    tests = json.load(f)

# Check if vnikx_size matches jhigh + 1
mismatches = []
for i, test in enumerate(tests):
    jhigh = test['jhigh']
    vnikx_size = test['vnikx_size']
    expected_size = jhigh + 1
    if vnikx_size != expected_size:
        mismatches.append((i+1, jhigh, vnikx_size, expected_size))

print(f'Total tests: {len(tests)}')
print(f'Found {len(mismatches)} cases where vnikx_size != jhigh + 1')

if mismatches:
    print('\nFirst 10 mismatches:')
    for test_num, jhigh, actual, expected in mismatches[:10]:
        print(f'  Test {test_num}: jhigh={jhigh}, vnikx_size={actual}, expected={expected}')
else:
    print('All vnikx_size values match jhigh + 1')

# Also check for test 30 which had issues
print(f"\nTest 30 details:")
test30 = tests[29]  # 0-based index
print(f"  jhigh={test30['jhigh']}, vnikx_size={test30['vnikx_size']}")
print(f"  x={test30['x']}, ileft={test30['ileft']}")
print(f"  First few knots: {test30['t'][:10]}")
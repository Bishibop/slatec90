#!/usr/bin/env python3
"""Manual validation of refined BSPLVN implementation"""

import json
import subprocess
import sys
from pathlib import Path

def validate_bsplvn():
    """Manually validate BSPLVN by running a subset of tests"""
    
    # Load test cases
    with open('../test_data/bsplvn_tests.json', 'r') as f:
        data = json.load(f)
    
    test_cases = data['test_cases'][:10]  # Test first 10 cases
    
    # Create test program
    test_program = """program test_bsplvn_refined
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(20), vnikx(20), x
    integer :: jhigh, index, ileft, i, j
    
"""
    
    for idx, test in enumerate(test_cases):
        t_array = test['inputs'][0]
        jhigh = test['inputs'][1]
        index = test['inputs'][2]
        x = test['inputs'][3]
        ileft = test['inputs'][4]
        expected = test['expected']
        
        test_program += f"""    ! Test {idx+1}: {test['description']}
    t = 0.0
"""
        for i, val in enumerate(t_array):
            test_program += f"    t({i+1}) = {val}\n"
        
        test_program += f"""    vnikx = 0.0
    jhigh = {jhigh}
    index = {index}
    x = {x}
    ileft = {ileft}
    call bsplvn(t, jhigh, index, x, ileft, vnikx)
    
    write(*,'(A,I3,A)') 'Test ', {idx+1}, ':'
    write(*,'(A)') '  Results:'
    do j = 1, {len(expected)}
        write(*,'(A,I2,A,F12.8)') '    vnikx(', j, ') = ', vnikx(j)
    end do
    write(*,'(A)') '  Expected:'
"""
        for j, val in enumerate(expected):
            test_program += f"    write(*,'(A,I2,A,F12.8)') '    vnikx(', {j+1}, ') = ', {val}\n"
        
        test_program += "    write(*,*)\n\n"
    
    test_program += "end program test_bsplvn_refined"
    
    # Write test program
    with open('test_refined.f90', 'w') as f:
        f.write(test_program)
    
    # Compile and run
    print("Compiling test program...")
    result = subprocess.run([
        'gfortran', '-o', 'test_refined',
        '../modern/bsplvn_o3mini_refined.f90',
        'test_refined.f90'
    ], capture_output=True, text=True)
    
    if result.returncode != 0:
        print("Compilation failed:")
        print(result.stderr)
        return False
    
    print("Running tests...\n")
    result = subprocess.run(['./test_refined'], capture_output=True, text=True)
    print(result.stdout)
    
    return True

if __name__ == "__main__":
    success = validate_bsplvn()
    sys.exit(0 if success else 1)
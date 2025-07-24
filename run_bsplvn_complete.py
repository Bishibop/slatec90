#!/usr/bin/env python3
import json
import subprocess
import os

# Read the reference test data  
with open('test_data/bsplvn_tests.json', 'r') as f:
    data = json.load(f)
    test_cases = data['test_cases']

# Create a Fortran program to run all tests
fortran_code = """program test_bsplvn_complete
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(100), vnikx(30), x
    integer :: jhigh, index_val, ileft
    integer :: i, j, expected_length, num_index2_calls
    
"""

for idx, test in enumerate(test_cases):
    test_num = test.get('test_id', idx + 1)
    inputs = test['inputs']
    
    # Extract inputs: [t_array, jhigh, index, x, ileft]
    t_values = inputs[0]
    jhigh = inputs[1]
    index_val = inputs[2]
    x = inputs[3]
    ileft = inputs[4]
    # Skip tests with no expected values
    if 'expected' not in test or test['expected'] is None:
        continue
    expected_length = len(test['expected'])
    
    fortran_code += f"    ! Test {test_num}: {test['description']}\n"
    
    # Initialize t array
    fortran_code += "    t = 0.0\n"
    for i, t_val in enumerate(t_values):
        fortran_code += f"    t({i+1}) = {t_val}\n"
    
    # Pad the knot vector if needed
    last_knot_idx = len(t_values)
    needed_knots = ileft + expected_length + 10  # Extra padding
    if needed_knots > last_knot_idx:
        last_val = t_values[-1]
        for i in range(last_knot_idx, min(needed_knots, 100)):
            fortran_code += f"    t({i+1}) = {last_val}\n"
    
    # Set parameters
    fortran_code += f"    jhigh = {jhigh}\n"
    fortran_code += f"    x = {x}\n"
    fortran_code += f"    ileft = {ileft}\n"
    fortran_code += f"    expected_length = {expected_length}\n"
    
    # Initialize vnikx
    fortran_code += "    vnikx = 0.0\n"
    
    # Call BSPLVN with INDEX=1
    fortran_code += f"    call bsplvn(t, jhigh, 1, x, ileft, vnikx)\n"
    
    # If expected_length > jhigh, we need INDEX=2 calls
    fortran_code += f"    if (expected_length > jhigh) then\n"
    fortran_code += f"        num_index2_calls = expected_length - jhigh\n"
    fortran_code += f"        do i = 1, num_index2_calls\n"
    fortran_code += f"            call bsplvn(t, jhigh, 2, x, ileft, vnikx)\n"
    fortran_code += f"        end do\n"
    fortran_code += f"    end if\n"
    
    # Output results - output expected_length values
    fortran_code += f"    write(*,'(A,I5,A)', advance='no') 'TEST_', {test_num}, '_RESULT:'\n"
    fortran_code += f"    do j = 1, expected_length\n"
    fortran_code += "        write(*,'(1X,ES20.13)', advance='no') vnikx(j)\n"
    fortran_code += "    end do\n"
    fortran_code += "    write(*,*)\n\n"

fortran_code += "end program test_bsplvn_complete\n"

# Write the Fortran program
with open('test_bsplvn_complete.f90', 'w') as f:
    f.write(fortran_code)

# Compile and run
print("Compiling test program...")
compile_result = subprocess.run(['gfortran', '-o', 'test_bsplvn_complete', 
                                'modern/bsplvn_modern.f90', 'test_bsplvn_complete.f90'],
                               capture_output=True, text=True)

if compile_result.returncode != 0:
    print("Compilation failed:")
    print(compile_result.stderr)
    exit(1)

print("Running tests...")
run_result = subprocess.run(['./test_bsplvn_complete'], capture_output=True, text=True)

if run_result.returncode != 0:
    print("Run failed:")
    print(run_result.stderr) 
    exit(1)

# Save results
with open('bsplvn_results.txt', 'w') as f:
    f.write(run_result.stdout)

# Count results
result_lines = [line for line in run_result.stdout.split('\n') if line.startswith('TEST_')]
print(f"Generated results for {len(result_lines)} test cases")
print("Results saved to bsplvn_results.txt")
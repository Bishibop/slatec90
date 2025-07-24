#!/usr/bin/env python3
import json
import subprocess
import os

# Read the reference test data  
with open('test_data/bsplvn_tests.json', 'r') as f:
    data = json.load(f)
    test_cases = data['test_cases']

# Create a Fortran program to run all tests
fortran_code = """program test_bsplvn_reference_all
    use bsplvn_module, only: bsplvn
    implicit none
    
    real :: t(100), vnikx(30), x
    integer :: jhigh, index_val, ileft
    integer :: i, j
    
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
    
    fortran_code += f"    ! Test {test_num}: {test['description']}\n"
    
    # Initialize t array
    fortran_code += "    t = 0.0\n"
    for i, t_val in enumerate(t_values):
        fortran_code += f"    t({i+1}) = {t_val}\n"
    
    # Pad the knot vector if needed
    last_knot_idx = len(t_values)
    needed_knots = ileft + jhigh + 2  
    if needed_knots > last_knot_idx:
        last_val = t_values[-1]
        for i in range(last_knot_idx, min(needed_knots, 100)):
            fortran_code += f"    t({i+1}) = {last_val}\n"
    
    # Set parameters
    fortran_code += f"    jhigh = {jhigh}\n"
    fortran_code += f"    index_val = {index_val}\n"
    fortran_code += f"    x = {x}\n"
    fortran_code += f"    ileft = {ileft}\n"
    
    # Initialize vnikx
    fortran_code += "    vnikx = 0.0\n"
    
    # Call BSPLVN
    fortran_code += "    call bsplvn(t, jhigh, index_val, x, ileft, vnikx)\n"
    
    # Output results - output jhigh values
    fortran_code += f"    write(*,'(A,I5,A)', advance='no') 'TEST_', {test_num}, '_RESULT:'\n"
    fortran_code += f"    do j = 1, jhigh\n"
    fortran_code += "        write(*,'(1X,ES20.13)', advance='no') vnikx(j)\n"
    fortran_code += "    end do\n"
    fortran_code += "    write(*,*)\n\n"

fortran_code += "end program test_bsplvn_reference_all\n"

# Write the Fortran program
with open('test_bsplvn_reference_all.f90', 'w') as f:
    f.write(fortran_code)

# Compile and run
print("Compiling test program...")
compile_result = subprocess.run(['gfortran', '-o', 'test_bsplvn_reference_all', 
                                'modern/bsplvn_modern.f90', 'test_bsplvn_reference_all.f90'],
                               capture_output=True, text=True)

if compile_result.returncode != 0:
    print("Compilation failed:")
    print(compile_result.stderr)
    exit(1)

print("Running tests...")
run_result = subprocess.run(['./test_bsplvn_reference_all'], capture_output=True, text=True)

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
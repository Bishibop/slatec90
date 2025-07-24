#!/usr/bin/env python3
import json
import subprocess
import tempfile
import os

# Read blind test data
with open('test_data/cshch_tests_blind.json', 'r') as f:
    blind_data = json.load(f)

# Create test program for a few specific cases
test_program = """
program test_cshch_manual
    use cshch_module
    implicit none
    
    complex :: z, csh, cch
    real :: z_real, z_imag
    integer :: i
    
    ! Test cases from blind data
    real, dimension(2, 10) :: test_inputs = reshape([ &
        0.0, 0.0, &     ! Test 1
        0.5, 0.0, &     ! Test 2
        1.0, 0.0, &     ! Test 3
        0.0, 1.5708, &  ! Test case that should fail (π/2)
        0.0, 3.1416, &  ! Test case that should fail (π)
        100.0, 0.0, &   ! Overflow case
        0.5, 0.5, &     ! Identity test
        1.0, 1.0, &     ! Identity test
        0.0, 1.0, &     ! Pure imaginary
        2.0, 1.0 &      ! Mixed case
    ], [2, 10])
    
    do i = 1, 10
        z_real = test_inputs(1, i)
        z_imag = test_inputs(2, i)
        z = cmplx(z_real, z_imag)
        
        call cshch(z, csh, cch)
        
        print '(I3, 6E20.12)', i, z_real, z_imag, real(csh), aimag(csh), real(cch), aimag(cch)
    end do
    
end program test_cshch_manual
"""

# Write and compile test program
with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
    f.write(test_program)
    test_file = f.name

try:
    # Compile
    compile_cmd = f"gfortran -o test_cshch_manual modern/cshch_modern.f90 {test_file}"
    result = subprocess.run(compile_cmd, shell=True, capture_output=True, text=True)
    if result.returncode != 0:
        print("Compilation failed:")
        print(result.stderr)
        exit(1)
    
    # Run
    result = subprocess.run("./test_cshch_manual", shell=True, capture_output=True, text=True)
    if result.returncode != 0:
        print("Execution failed:")
        print(result.stderr)
        exit(1)
    
    print("Test outputs from modern implementation:")
    print("Test#  z_real       z_imag       csh_real     csh_imag     cch_real     cch_imag")
    print(result.stdout)
    
finally:
    # Cleanup
    if os.path.exists(test_file):
        os.remove(test_file)
    if os.path.exists("test_cshch_manual"):
        os.remove("test_cshch_manual")

print("\nKey observations:")
print("- Test 4 (z=0+1.5708i): cch_real should be ~-3.6e-06, not 0")
print("- Test 5 (z=0+3.1416i): csh_imag should be ~-7.2e-06, not 0")
print("- Test 6 (z=100+0i): Should return large finite values, not Inf")
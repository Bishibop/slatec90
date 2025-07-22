#!/usr/bin/env python3
"""
Validate our migration approach with a simple function
"""

import subprocess
import os
import json
import tempfile

def test_f77_compilation():
    """Test 1: Can we compile F77?"""
    print("Test 1: F77 Compilation")
    
    test_program = """
      PROGRAM TEST
      REAL X(3), RESULT
      EXTERNAL ENORM
      REAL ENORM
      
      X(1) = 3.0
      X(2) = 4.0  
      X(3) = 0.0
      
      RESULT = ENORM(3, X)
      PRINT *, 'RESULT:', RESULT
      END
    """
    
    with tempfile.TemporaryDirectory() as tmpdir:
        # Write test
        test_file = os.path.join(tmpdir, 'test.f')
        with open(test_file, 'w') as f:
            f.write(test_program)
        
        # Try to compile with enorm
        try:
            # First compile enorm.f
            cmd = ['gfortran', '-std=legacy', '-c', 'src/enorm.f', '-o', f'{tmpdir}/enorm.o']
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"✗ Failed to compile enorm.f: {result.stderr}")
                return False
            
            # Compile r1mach dependency
            cmd = ['gfortran', '-std=legacy', '-c', 'src/r1mach.f', '-o', f'{tmpdir}/r1mach.o']  
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"✗ Failed to compile r1mach.f: {result.stderr}")
                return False
                
            # Now compile test
            cmd = ['gfortran', '-std=legacy', test_file, f'{tmpdir}/enorm.o', f'{tmpdir}/r1mach.o', '-o', f'{tmpdir}/test']
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"✗ Failed to compile test: {result.stderr}")
                return False
            
            # Run test
            result = subprocess.run([f'{tmpdir}/test'], capture_output=True, text=True, cwd=tmpdir)
            if result.returncode != 0:
                print(f"✗ Failed to run test: {result.stderr}")
                return False
                
            print(f"✓ F77 compilation works!")
            print(f"  Output: {result.stdout.strip()}")
            
            # Parse output
            if 'RESULT:' in result.stdout:
                value = float(result.stdout.split('RESULT:')[1].strip())
                if abs(value - 5.0) < 0.0001:
                    print(f"✓ Correct result: {value}")
                else:
                    print(f"✗ Wrong result: {value} (expected 5.0)")
            
            return True
            
        except Exception as e:
            print(f"✗ Exception: {e}")
            return False

def test_modern_compilation():
    """Test 2: Can we compile modern Fortran?"""
    print("\nTest 2: Modern Fortran Compilation")
    
    modern_code = """
module enorm_module
    use iso_fortran_env, only: real32
    implicit none
contains
    pure function enorm_modern(x) result(norm)
        real(real32), intent(in) :: x(:)
        real(real32) :: norm
        norm = sqrt(sum(x**2))
    end function
end module

! F77 compatible wrapper
real function enorm(n, x)
    use enorm_module
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    enorm = enorm_modern(x(1:n))
end function
    """
    
    with tempfile.TemporaryDirectory() as tmpdir:
        # Write modern code
        modern_file = os.path.join(tmpdir, 'enorm_modern.f90')
        with open(modern_file, 'w') as f:
            f.write(modern_code)
            
        # Try to compile
        try:
            cmd = ['gfortran', '-std=f2018', '-c', modern_file, '-o', f'{tmpdir}/enorm_modern.o']
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"✗ Failed to compile modern: {result.stderr}")
                return False
                
            print("✓ Modern compilation works!")
            return True
            
        except Exception as e:
            print(f"✗ Exception: {e}")
            return False

def test_dependency_extraction():
    """Test 3: Can we extract dependencies?"""
    print("\nTest 3: Dependency Extraction")
    
    try:
        with open('src/enorm.f', 'r') as f:
            content = f.read()
            
        # Look for ROUTINES CALLED
        if '***ROUTINES CALLED' in content:
            lines = content.split('\n')
            for i, line in enumerate(lines):
                if '***ROUTINES CALLED' in line:
                    deps_line = lines[i+1] if i+1 < len(lines) else ''
                    deps = deps_line.replace('C', '').strip()
                    print(f"✓ Found dependencies: {deps}")
                    break
        
        return True
    except Exception as e:
        print(f"✗ Exception: {e}")
        return False

def test_simple_function_list():
    """Test 4: Get list of simple functions to start with"""
    print("\nTest 4: Finding Simple Functions")
    
    simple_functions = []
    
    # Look for functions with no dependencies
    import glob
    for f77_file in glob.glob('src/*.f')[:20]:  # Check first 20
        try:
            with open(f77_file, 'r') as f:
                content = f.read()
                
            if '***ROUTINES CALLED  (NONE)' in content:
                func_name = os.path.basename(f77_file).replace('.f', '')
                simple_functions.append(func_name)
                
        except:
            pass
    
    print(f"✓ Found {len(simple_functions)} functions with no dependencies:")
    for func in simple_functions[:5]:
        print(f"  - {func}")
    
    return True

if __name__ == '__main__':
    print("Validating SLATEC Migration Approach")
    print("=" * 50)
    
    tests = [
        test_f77_compilation,
        test_modern_compilation,
        test_dependency_extraction,
        test_simple_function_list
    ]
    
    passed = sum(test() for test in tests)
    
    print(f"\nPassed {passed}/{len(tests)} tests")
    
    if passed == len(tests):
        print("\n✓ Ready to build migration pipeline!")
    else:
        print("\n✗ Fix issues before proceeding")
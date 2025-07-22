#!/usr/bin/env python3
"""
SLATEC Migration Test Helper

This script helps generate test cases, run F77 reference implementations,
and validate modern Fortran implementations for SLATEC functions.

Usage:
    # Generate test cases and get reference values from F77
    python slatec_test_helper.py generate FUNCNAME
    
    # Validate modern implementation against test data
    python slatec_test_helper.py validate FUNCNAME
"""

import json
import subprocess
import re
import sys
import math
from pathlib import Path


class SlatecTestHelper:
    def __init__(self, func_name):
        self.func_name = func_name.upper()
        self.test_file = f"test_data/{func_name.lower()}_tests.json"
        self.batch_size = 50  # F77 program size limit
        
    def generate_test_cases(self):
        """Generate test cases based on function type"""
        if self.func_name == "PYTHAG":
            return self._generate_pythag_tests()
        elif self.func_name == "CDIV":
            return self._generate_cdiv_tests()
        else:
            print(f"No test generator for {self.func_name} yet")
            print("Please implement a generator based on the function's purpose")
            return []
    
    def _generate_pythag_tests(self):
        """Generate test cases for PYTHAG (Pythagorean sum)"""
        tests = []
        
        # Pythagorean triples
        triples = [(3,4,5), (5,12,13), (8,15,17), (7,24,25)]
        for a, b, c in triples:
            for scale in [0.01, 0.1, 1, 10, 100, 1000]:
                tests.append({
                    "description": f"Pythagorean triple ({a},{b},{c}) scaled by {scale}",
                    "inputs": [a*scale, b*scale],
                    "expected": None
                })
        
        # Edge cases
        edge_values = [0, 1, -1, 1e-10, 1e10, 1e-38, 1e38]
        for a in edge_values[:4]:
            for b in edge_values[:4]:
                tests.append({
                    "description": f"Edge case: a={a}, b={b}",
                    "inputs": [a, b],
                    "expected": None
                })
        
        # Special patterns
        for i in range(-10, 11):
            val = 2**i
            tests.append({
                "description": f"Powers of 2: both {val}",
                "inputs": [val, val],
                "expected": None
            })
        
        return tests
    
    def _generate_cdiv_tests(self):
        """Generate test cases for CDIV (complex division)"""
        tests = []
        
        # Basic cases
        basic = [
            ("Real / real", [1.0, 0.0, 1.0, 0.0]),
            ("i / i = 1", [0.0, 1.0, 0.0, 1.0]),
            ("Complex / real", [1.0, 1.0, 2.0, 0.0]),
            ("Real / imaginary", [1.0, 0.0, 0.0, 1.0]),
            ("Complex / complex", [3.0, 4.0, 1.0, 2.0]),
        ]
        
        for desc, inputs in basic:
            tests.append({
                "description": desc,
                "inputs": inputs,
                "expected": None
            })
        
        # Angles
        for angle in range(0, 360, 30):
            rad = math.radians(angle)
            ar, ai = 5 * math.cos(rad), 5 * math.sin(rad)
            br, bi = math.cos(math.radians(45)), math.sin(math.radians(45))
            tests.append({
                "description": f"Magnitude 5 at {angle}° / unit at 45°",
                "inputs": [ar, ai, br, bi],
                "expected": None
            })
        
        return tests
    
    def run_f77_reference(self, test_cases):
        """Run F77 implementation to get reference values"""
        all_results = []
        
        for batch_start in range(0, len(test_cases), self.batch_size):
            batch_end = min(batch_start + self.batch_size, len(test_cases))
            batch = test_cases[batch_start:batch_end]
            
            # Generate F77 program for this batch
            program = self._generate_f77_program(batch, batch_start)
            
            # Write, compile, run
            test_file = "temp_test.f"
            with open(test_file, 'w') as f:
                f.write(program)
            
            exe_file = "temp_test"
            src_file = f"src/{self.func_name.lower()}.f"
            
            # Compile
            compile_result = subprocess.run(
                ['gfortran', '-o', exe_file, test_file, src_file],
                capture_output=True, text=True
            )
            
            if compile_result.returncode != 0:
                print(f"Compilation failed: {compile_result.stderr}")
                return None
            
            # Run
            run_result = subprocess.run(
                [f'./{exe_file}'], capture_output=True, text=True
            )
            
            if run_result.returncode != 0:
                print(f"Execution failed: {run_result.stderr}")
                return None
            
            # Parse results
            results = self._parse_f77_output(run_result.stdout)
            all_results.extend(results)
            
            # Cleanup
            Path(test_file).unlink(missing_ok=True)
            Path(exe_file).unlink(missing_ok=True)
            
            print(f"Batch {batch_start+1}-{batch_end}: {len(results)} results")
        
        return all_results
    
    def _generate_f77_program(self, test_cases, start_index):
        """Generate F77 test program based on function signature"""
        if self.func_name == "PYTHAG":
            return self._generate_pythag_f77(test_cases, start_index)
        elif self.func_name == "CDIV":
            return self._generate_cdiv_f77(test_cases, start_index)
        else:
            raise NotImplementedError(f"No F77 generator for {self.func_name}")
    
    def _generate_pythag_f77(self, test_cases, start_index):
        """Generate F77 program for PYTHAG"""
        program = f"""      PROGRAM TEST_PYTHAG
      REAL PYTHAG, A, B, RESULT
      EXTERNAL PYTHAG
      
"""
        for i, test in enumerate(test_cases):
            test_num = start_index + i + 1
            a, b = test['inputs']
            program += f"""C     Test {test_num}
      A = {a:e}
      B = {b:e}
      RESULT = PYTHAG(A, B)
      WRITE(*,'(A,I5,A,E20.10)') 'TEST_', {test_num}, '_RESULT: ', RESULT
      
"""
        program += "      END"
        return program
    
    def _generate_cdiv_f77(self, test_cases, start_index):
        """Generate F77 program for CDIV"""
        program = f"""      PROGRAM TEST_CDIV
      REAL AR, AI, BR, BI, CR, CI
      EXTERNAL CDIV
      
"""
        for i, test in enumerate(test_cases):
            test_num = start_index + i + 1
            ar, ai, br, bi = test['inputs']
            program += f"""C     Test {test_num}
      AR = {ar:e}
      AI = {ai:e}
      BR = {br:e}
      BI = {bi:e}
      CALL CDIV(AR, AI, BR, BI, CR, CI)
      WRITE(*,'(A,I5,A,E20.10,A,E20.10)') 'TEST_', {test_num}, 
     +    '_RESULT: ', CR, ', ', CI
      
"""
        program += "      END"
        return program
    
    def _parse_f77_output(self, output):
        """Parse F77 output to extract results"""
        results = []
        
        if self.func_name == "PYTHAG":
            # Single result per test
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+[eE][-+]?\d+)'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                value = float(match.group(2))
                results.append((test_num, value))
                
        elif self.func_name == "CDIV":
            # Two results per test (real, imaginary)
            pattern = r'TEST_\s*(\d+)_RESULT:\s*([-+]?\d*\.?\d+[eE][-+]?\d+),\s*([-+]?\d*\.?\d+[eE][-+]?\d+)'
            for match in re.finditer(pattern, output):
                test_num = int(match.group(1))
                real_part = float(match.group(2))
                imag_part = float(match.group(3))
                results.append((test_num, real_part, imag_part))
        
        return results
    
    def save_test_data(self, test_cases, results):
        """Save test cases with reference values"""
        # Update test cases with results
        if self.func_name == "PYTHAG":
            for (test_num, value), test_case in zip(results, test_cases):
                test_case['expected'] = value
                test_case['test_id'] = test_num
        elif self.func_name == "CDIV":
            for (test_num, real_part, imag_part), test_case in zip(results, test_cases):
                test_case['expected'] = [real_part, imag_part]
                test_case['test_id'] = test_num
        
        # Create output structure
        output_data = {
            "function": self.func_name.lower(),
            "signature": self._get_signature(),
            "description": self._get_description(),
            "total_tests": len(test_cases),
            "test_cases": test_cases
        }
        
        # Ensure test_data directory exists
        Path("test_data").mkdir(exist_ok=True)
        
        # Save
        with open(self.test_file, 'w') as f:
            json.dump(output_data, f, indent=2)
        
        print(f"Saved {len(test_cases)} test cases to {self.test_file}")
    
    def validate_modern(self):
        """Validate modern implementation against test data"""
        # Load test data
        with open(self.test_file, 'r') as f:
            test_data = json.load(f)
        
        test_cases = test_data['test_cases']
        print(f"Validating {len(test_cases)} test cases...")
        
        # Run modern implementation
        results = self._run_modern_implementation(test_cases)
        
        if not results:
            print("Failed to run modern implementation")
            return False
        
        # Compare results
        tolerance = 1e-6
        failures = 0
        
        for result, test_case in zip(results, test_cases):
            if self.func_name == "PYTHAG":
                actual = result[1]  # (test_num, value)
                expected = test_case['expected']
                
                if expected != 0:
                    rel_error = abs(actual - expected) / abs(expected)
                else:
                    rel_error = abs(actual - expected)
                
                if rel_error > tolerance:
                    failures += 1
                    if failures <= 5:
                        print(f"\nTest {result[0]} FAILED:")
                        print(f"  Expected: {expected}")
                        print(f"  Actual: {actual}")
                        print(f"  Error: {rel_error}")
                        
        print(f"\n{len(test_cases) - failures} tests PASSED")
        print(f"{failures} tests FAILED")
        
        return failures == 0
    
    def _run_modern_implementation(self, test_cases):
        """Run modern F90 implementation"""
        # Generate F90 test program
        program = self._generate_modern_test_program(test_cases)
        
        test_file = "test_modern.f90"
        with open(test_file, 'w') as f:
            f.write(program)
        
        module_file = f"modern/{self.func_name.lower()}_modern.f90"
        exe_file = "test_modern"
        
        # Compile
        compile_result = subprocess.run(
            ['gfortran', '-o', exe_file, module_file, test_file],
            capture_output=True, text=True
        )
        
        if compile_result.returncode != 0:
            print(f"Compilation failed: {compile_result.stderr}")
            return None
        
        # Run
        run_result = subprocess.run(
            [f'./{exe_file}'], capture_output=True, text=True
        )
        
        if run_result.returncode != 0:
            print(f"Execution failed: {run_result.stderr}")
            return None
        
        # Parse and cleanup
        results = self._parse_f77_output(run_result.stdout)
        
        Path(test_file).unlink(missing_ok=True)
        Path(exe_file).unlink(missing_ok=True)
        Path(f"{self.func_name.lower()}_module.mod").unlink(missing_ok=True)
        
        return results
    
    def _generate_modern_test_program(self, test_cases):
        """Generate modern F90 test program"""
        if self.func_name == "PYTHAG":
            return self._generate_pythag_modern_test(test_cases)
        elif self.func_name == "CDIV":
            return self._generate_cdiv_modern_test(test_cases)
        else:
            raise NotImplementedError(f"No modern test generator for {self.func_name}")
    
    def _generate_pythag_modern_test(self, test_cases):
        """Generate modern F90 test for PYTHAG"""
        program = f"""program test_pythag
    use pythag_module, only: pythag
    implicit none
    
    real :: a, b, result
    
"""
        for i, test in enumerate(test_cases[:self.batch_size]):  # Limit batch size
            a, b = test['inputs']
            program += f"""    ! Test {i+1}
    a = {a:e}
    b = {b:e}
    result = pythag(a, b)
    write(*,'(A,I5,A,E20.10)') 'TEST_', {i+1}, '_RESULT: ', result
    
"""
        program += "end program test_pythag"
        return program
    
    def _generate_cdiv_modern_test(self, test_cases):
        """Generate modern F90 test for CDIV"""
        program = f"""program test_cdiv
    use cdiv_module, only: cdiv
    implicit none
    
    real :: ar, ai, br, bi, cr, ci
    
"""
        for i, test in enumerate(test_cases[:self.batch_size]):
            ar, ai, br, bi = test['inputs']
            program += f"""    ! Test {i+1}
    ar = {ar:e}
    ai = {ai:e}
    br = {br:e}
    bi = {bi:e}
    call cdiv(ar, ai, br, bi, cr, ci)
    write(*,'(A,I5,A,E20.10,A,E20.10)') 'TEST_', {i+1}, &
        '_RESULT: ', cr, ', ', ci
    
"""
        program += "end program test_cdiv"
        return program
    
    def _get_signature(self):
        """Get function signature"""
        signatures = {
            "PYTHAG": "REAL FUNCTION PYTHAG(A, B)",
            "CDIV": "SUBROUTINE CDIV(AR, AI, BR, BI, CR, CI)"
        }
        return signatures.get(self.func_name, "Unknown")
    
    def _get_description(self):
        """Get function description"""
        descriptions = {
            "PYTHAG": "Compute sqrt(a^2 + b^2) without overflow",
            "CDIV": "Complex division: (CR,CI) = (AR,AI)/(BR,BI)"
        }
        return descriptions.get(self.func_name, "No description")


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(1)
    
    command = sys.argv[1]
    func_name = sys.argv[2]
    
    helper = SlatecTestHelper(func_name)
    
    if command == "generate":
        # Generate test cases
        test_cases = helper.generate_test_cases()
        print(f"Generated {len(test_cases)} test cases")
        
        # Run F77 to get reference values
        results = helper.run_f77_reference(test_cases)
        if results:
            helper.save_test_data(test_cases, results)
        
    elif command == "validate":
        # Validate modern implementation
        success = helper.validate_modern()
        sys.exit(0 if success else 1)
        
    else:
        print(f"Unknown command: {command}")
        print("Use: generate, validate")
        sys.exit(1)


if __name__ == "__main__":
    main()
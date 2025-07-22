#!/usr/bin/env python3
"""
Simple SLATEC Migration Pipeline using OpenAI o3-mini
"""

import os
import subprocess
import json
import tempfile
from pathlib import Path
from datetime import datetime
from llm_config import call_llm, MODELS, REASONING_EFFORT, estimate_cost

class SimpleMigrationPipeline:
    def __init__(self):
        self.src_dir = Path("src")
        self.modern_dir = Path("modern")
        self.log_dir = Path("logs")
        self.total_cost = 0.0
        
        # Error tracking
        self.last_compile_error = None
        self.last_runtime_error = None
        
        # Ensure directories exist
        self.modern_dir.mkdir(exist_ok=True)
        self.log_dir.mkdir(exist_ok=True)
        
    def migrate_function(self, func_name, max_iterations=5):
        """Migrate a single F77 function to modern Fortran"""
        
        print(f"\n{'='*60}")
        print(f"Migrating: {func_name}")
        print(f"{'='*60}")
        
        # 1. Read F77 source
        f77_path = self.src_dir / f"{func_name}.f"
        if not f77_path.exists():
            return {"success": False, "error": "Source file not found"}
            
        f77_source = f77_path.read_text()
        
        # 2. Extract dependencies from comments
        dependencies = self.extract_dependencies(f77_source)
        print(f"Dependencies: {dependencies if dependencies else 'None'}")
        
        # 3. Generate test cases
        print("\nGenerating test cases...")
        test_cases = self.generate_test_cases(func_name, f77_source)
        print(f"Generated {len(test_cases)} test cases")
        
        # 4. Generate reference values from F77
        print("\nGenerating reference values from F77...")
        reference_values = self.get_f77_reference_values(func_name, test_cases)
        
        # 5. Generate initial modern version
        print("\nGenerating modern Fortran version...")
        modern_code = self.generate_modern_fortran(func_name, f77_source, dependencies)
        
        # 6. Iterative refinement loop
        for iteration in range(max_iterations):
            print(f"\nIteration {iteration + 1}:")
            
            # Save modern version
            modern_path = self.modern_dir / f"{func_name}_modern.f90"
            modern_path.write_text(modern_code)
            
            # Compile modern version
            if not self.compile_modern(func_name):
                print("  ✗ Compilation failed")
                # Ask LLM to fix compilation errors
                modern_code = self.fix_compilation_error(func_name, modern_code, f77_source)
                continue
                
            # Test modern version
            failures = self.test_modern_version(func_name, test_cases, reference_values)
            
            if not failures:
                print(f"  ✓ All {len(test_cases)} tests passed!")
                return {
                    "success": True,
                    "iterations": iteration + 1,
                    "cost": self.total_cost
                }
            
            print(f"  ✗ {len(failures)} tests failed")
            
            # Ask LLM to fix failures
            if iteration < max_iterations - 1:
                print("  Analyzing failures and generating fix...")
                modern_code = self.fix_test_failures(
                    func_name, modern_code, f77_source, failures, iteration
                )
        
        # Failed after max iterations
        self.save_failure_info(func_name, failures, modern_code)
        return {
            "success": False,
            "iterations": max_iterations,
            "remaining_failures": len(failures),
            "cost": self.total_cost
        }
    
    def extract_dependencies(self, f77_source):
        """Extract dependencies from SLATEC comments"""
        lines = f77_source.split('\n')
        for i, line in enumerate(lines):
            if '***ROUTINES CALLED' in line:
                # The dependencies are on the same line after the tag
                if '(NONE)' in line:
                    return []
                # Extract everything after CALLED
                deps_text = line.split('***ROUTINES CALLED')[1].strip()
                if deps_text and deps_text != '(NONE)':
                    return [d.strip() for d in deps_text.split(',') if d.strip()]
        return []
    
    def generate_test_cases(self, func_name, f77_source):
        """Generate test cases using o3-mini"""
        
        prompt = f"""Generate comprehensive test cases for this FORTRAN 77 function.

Include:
1. Normal cases (typical inputs)
2. Edge cases (zeros, negative values, boundary values)
3. Error cases (if applicable)

Return ONLY a JSON array of test cases in this format:
[
  {{"inputs": [values...], "description": "what this tests"}},
  ...
]

Function source:
{f77_source}
"""
        
        response = call_llm(prompt, model=MODELS["test_generation"], 
                          reasoning_effort=REASONING_EFFORT["initial_generation"])
        self.total_cost += estimate_cost(prompt, response, MODELS["test_generation"])
        
        try:
            # Extract JSON from response
            json_start = response.find('[')
            json_end = response.rfind(']') + 1
            if json_start >= 0 and json_end > json_start:
                test_cases = json.loads(response[json_start:json_end])
                return test_cases
        except:
            print("Failed to parse test cases, using defaults")
            
        # Fallback test cases
        return [
            {"inputs": [3.0, 4.0], "description": "simple test"},
            {"inputs": [0.0, 0.0], "description": "zero test"},
            {"inputs": [1.0, 0.0], "description": "mixed test"}
        ]
    
    def get_f77_reference_values(self, func_name, test_cases):
        """Run F77 function to get reference values"""
        reference_values = []
        
        for i, test in enumerate(test_cases):
            # Generate test program
            test_program = self.generate_f77_test_program(func_name, test)
            
            # Run test
            result = self.run_f77_test(func_name, test_program)
            if result is not None:
                reference_values.append(result)
            else:
                reference_values.append("ERROR")
                
        return reference_values
    
    def generate_f77_test_program(self, func_name, test_case):
        """Generate F77 test program using o3-mini"""
        
        # Read function source for context
        f77_source = (self.src_dir / f"{func_name}.f").read_text()
        
        prompt = f"""Generate a complete FORTRAN 77 test program that:
1. Calls the function {func_name} with inputs: {test_case['inputs']}
2. Prints the result in format: RESULT: <value>

Important:
- Declare all variables properly
- Handle arrays correctly
- Include proper IMPLICIT NONE
- Make it compile with gfortran -std=legacy

Function source for reference:
{f77_source[:1000]}...

Return ONLY the complete test program code, no explanations.
"""
        
        response = call_llm(prompt, model=MODELS["code_generation"],
                          reasoning_effort="low")  # Simple task
        self.total_cost += estimate_cost(prompt, response, MODELS["code_generation"])
        
        return response
    
    def run_f77_test(self, func_name, test_program):
        """Compile and run F77 test"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)
            
            # Save test program
            test_file = tmpdir / "test.f"
            test_file.write_text(test_program)
            
            # Compile
            try:
                # First compile the function
                subprocess.run([
                    "gfortran", "-std=legacy", "-c",
                    str(self.src_dir / f"{func_name}.f"),
                    "-o", str(tmpdir / f"{func_name}.o")
                ], check=True, capture_output=True)
                
                # Compile test with function
                subprocess.run([
                    "gfortran", "-std=legacy",
                    str(test_file),
                    str(tmpdir / f"{func_name}.o"),
                    "-o", str(tmpdir / "test")
                ], check=True, capture_output=True)
                
                # Run test
                result = subprocess.run(
                    [str(tmpdir / "test")],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                
                # Parse output
                output = result.stdout
                if "RESULT:" in output:
                    value_str = output.split("RESULT:")[1].strip().split()[0]
                    return float(value_str)
                    
            except Exception as e:
                print(f"  Error running F77 test: {e}")
                
        return None
    
    def generate_modern_fortran(self, func_name, f77_source, dependencies):
        """Generate modern Fortran using o3-mini"""
        
        prompt = f"""Convert this FORTRAN 77 function to modern Fortran (2018 standard).

Requirements:
1. Create a module named {func_name}_module
2. Use modern Fortran features:
   - implicit none
   - intent declarations
   - assumed-shape arrays where appropriate
   - no GOTO statements
3. Preserve EXACT numerical behavior
4. Include F77-compatible wrapper function at the end

Dependencies that need to be available: {dependencies}

Original F77 code:
{f77_source}

Generate the complete modern Fortran code including module and wrapper.
"""
        
        response = call_llm(prompt, model=MODELS["code_generation"],
                          reasoning_effort=REASONING_EFFORT["initial_generation"])
        self.total_cost += estimate_cost(prompt, response, MODELS["code_generation"])
        
        return response
    
    def compile_modern(self, func_name):
        """Compile modern Fortran version"""
        try:
            modern_file = self.modern_dir / f"{func_name}_modern.f90"
            result = subprocess.run([
                "gfortran", "-std=f2018", "-c",
                str(modern_file),
                "-o", str(self.modern_dir / f"{func_name}_modern.o")
            ], capture_output=True, text=True)
            
            if result.returncode != 0:
                self.last_compile_error = result.stderr
                print(f"  Compilation error: {result.stderr}")
                return False
            
            self.last_compile_error = None
            return True
        except Exception as e:
            self.last_compile_error = str(e)
            print(f"  Compilation exception: {e}")
            return False
    
    def test_modern_version(self, func_name, test_cases, reference_values):
        """Test modern version against reference values"""
        failures = []
        
        for i, (test, ref_value) in enumerate(zip(test_cases, reference_values)):
            if ref_value == "ERROR":
                continue
                
            # Generate test program for modern version
            test_program = self.generate_modern_test_program(func_name, test)
            
            # Run test
            result = self.run_modern_test(func_name, test_program)
            
            if result is None:
                failures.append({
                    "test_id": i,
                    "inputs": test["inputs"],
                    "expected": ref_value,
                    "actual": "ERROR",
                    "error": getattr(self, 'last_runtime_error', 'Unknown error'),
                    "description": test.get("description", "")
                })
            elif not self.close_enough(result, ref_value):
                failures.append({
                    "test_id": i,
                    "inputs": test["inputs"],
                    "expected": ref_value,
                    "actual": result,
                    "description": test.get("description", "")
                })
                
        return failures
    
    def generate_modern_test_program(self, func_name, test_case):
        """Generate test program for modern version"""
        # For modern version, we can call the F77 wrapper
        return f"""
program test_modern
    implicit none
    real :: result
    external {func_name}
    real :: {func_name}
    
    result = {func_name}({', '.join(map(str, test_case['inputs']))})
    print *, 'RESULT:', result
end program
"""
    
    def run_modern_test(self, func_name, test_program):
        """Run test with modern version"""
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)
            
            # Save test
            test_file = tmpdir / "test.f90"
            test_file.write_text(test_program)
            
            try:
                # Compile test with modern version
                compile_result = subprocess.run([
                    "gfortran", "-std=f2018",
                    str(test_file),
                    str(self.modern_dir / f"{func_name}_modern.o"),
                    "-o", str(tmpdir / "test")
                ], capture_output=True, text=True)
                
                if compile_result.returncode != 0:
                    self.last_runtime_error = f"Test compilation failed:\n{compile_result.stderr}"
                    return None
                
                # Run
                result = subprocess.run(
                    [str(tmpdir / "test")],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                
                if result.returncode != 0:
                    self.last_runtime_error = f"Runtime error:\n{result.stderr}\nOutput:\n{result.stdout}"
                    return None
                
                # Parse output
                if "RESULT:" in result.stdout:
                    value_str = result.stdout.split("RESULT:")[1].strip().split()[0]
                    return float(value_str)
                else:
                    self.last_runtime_error = f"Could not parse output:\n{result.stdout}"
                    
            except subprocess.TimeoutExpired:
                self.last_runtime_error = "Test timed out (infinite loop?)"
            except Exception as e:
                self.last_runtime_error = f"Exception: {str(e)}"
                print(f"  Error in modern test: {e}")
                
        return None
    
    def close_enough(self, a, b, rel_tol=1e-6):
        """Check if two floating point values are close enough"""
        if abs(b) > 1e-10:
            return abs((a - b) / b) < rel_tol
        else:
            return abs(a - b) < 1e-10
    
    def fix_test_failures(self, func_name, modern_code, f77_source, failures, iteration):
        """Ask o3-mini to fix test failures"""
        
        # Analyze failure patterns
        patterns = self.analyze_failure_patterns(failures)
        
        prompt = f"""The modernized Fortran function is failing {len(failures)} tests. Please fix it.

Original F77 code:
{f77_source}

Current modern code (with failures):
{modern_code}

Test failures:
{self.format_failures(failures[:10])}  # Limit to avoid token limits

Detected patterns:
{patterns}

Common F77 to F90 issues to check:
1. Array indexing (F77 starts at 1)
2. Integer division behavior  
3. Uninitialized variables (F77 might assume 0)
4. GOTO logic translation
5. Precision/rounding differences

This is iteration {iteration + 2} of 5.

Generate the complete CORRECTED modern Fortran code.
"""
        
        response = call_llm(prompt, model=MODELS["code_generation"],
                          reasoning_effort=REASONING_EFFORT["bug_fixing"])  # High effort for debugging
        self.total_cost += estimate_cost(prompt, response, MODELS["code_generation"])
        
        return response
    
    def analyze_failure_patterns(self, failures):
        """Identify patterns in failures"""
        patterns = []
        
        # All zeros?
        if all(f["actual"] == 0.0 for f in failures if isinstance(f["actual"], (int, float))):
            patterns.append("All outputs are zero - likely uninitialized variable")
            
        # Sign errors?
        sign_errors = sum(1 for f in failures 
                         if isinstance(f["actual"], (int, float)) and 
                         isinstance(f["expected"], (int, float)) and
                         f["actual"] * f["expected"] < 0)
        if sign_errors > 0:
            patterns.append(f"{sign_errors} sign errors detected")
            
        # Consistent offset?
        if len(failures) > 2:
            diffs = [f["actual"] - f["expected"] for f in failures
                    if isinstance(f["actual"], (int, float)) and 
                    isinstance(f["expected"], (int, float))]
            if diffs and all(abs(d - diffs[0]) < 0.001 for d in diffs):
                patterns.append(f"Consistent offset of {diffs[0]}")
                
        return "\n".join(patterns) if patterns else "No clear pattern detected"
    
    def format_failures(self, failures):
        """Format failures for LLM consumption"""
        lines = []
        for f in failures:
            lines.append(f"Test {f['test_id']}: {f.get('description', 'N/A')}")
            lines.append(f"Inputs: {f['inputs']}")
            lines.append(f"  Expected (F77): {f['expected']}")
            lines.append(f"  Got (Modern): {f['actual']}")
            
            # Include error details if present
            if 'error' in f and f['error']:
                lines.append(f"  Error details:")
                for error_line in f['error'].split('\n'):
                    lines.append(f"    {error_line}")
            
            lines.append("")
        return "\n".join(lines)
    
    def fix_compilation_error(self, func_name, modern_code, f77_source):
        """Fix compilation errors with specific error details"""
        
        prompt = f"""The modern Fortran code failed to compile. Please fix it.

Compilation error:
{self.last_compile_error}

Current modern code that failed:
{modern_code}

Original F77 code for reference:
{f77_source}

Common compilation issues:
- Markdown code blocks (remove ```fortran and ``` lines)
- Missing end statements
- Incorrect module syntax
- Type mismatches

Generate the complete CORRECTED modern Fortran code.
"""
        
        response = call_llm(prompt, model=MODELS["code_generation"],
                          reasoning_effort=REASONING_EFFORT["bug_fixing"])
        self.total_cost += estimate_cost(prompt, response, MODELS["code_generation"])
        
        # Strip any markdown formatting
        from strip_markdown import strip_markdown_blocks
        return strip_markdown_blocks(response)
    
    def save_failure_info(self, func_name, failures, modern_code):
        """Save debug information for failed migrations"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        debug_file = self.log_dir / f"{func_name}_{timestamp}_debug.json"
        
        debug_info = {
            "function": func_name,
            "timestamp": timestamp,
            "failures": failures,
            "modern_code": modern_code,
            "total_cost": self.total_cost
        }
        
        with open(debug_file, 'w') as f:
            json.dump(debug_info, f, indent=2)
            
        print(f"\nDebug info saved to: {debug_file}")


def main():
    """Run migration on simple functions"""
    
    # Start with functions that have no dependencies
    simple_functions = [
        "denorm",   # Double precision norm, no deps
        "ppsgf",    # 25 lines, no deps
        "pythag",   # Should be simple too
    ]
    
    pipeline = SimpleMigrationPipeline()
    results = []
    
    for func in simple_functions:
        result = pipeline.migrate_function(func)
        results.append((func, result))
        
        if result["success"]:
            print(f"\n✓ Successfully migrated {func} in {result['iterations']} iterations")
        else:
            print(f"\n✗ Failed to migrate {func} after {result['iterations']} iterations")
            print(f"  Remaining failures: {result.get('remaining_failures', 'N/A')}")
    
    # Summary
    print("\n" + "="*60)
    print("MIGRATION SUMMARY")
    print("="*60)
    
    successful = sum(1 for _, r in results if r["success"])
    print(f"Successful: {successful}/{len(results)}")
    print(f"Total API cost: ${pipeline.total_cost:.4f}")
    
    for func, result in results:
        status = "✓" if result["success"] else "✗"
        print(f"{status} {func:15} - {result.get('iterations', 0)} iterations")


if __name__ == "__main__":
    # Check for API key
    if not os.getenv("OPENAI_API_KEY"):
        print("Error: OPENAI_API_KEY environment variable not set")
        print("Set it with: export OPENAI_API_KEY='your-key-here'")
        exit(1)
        
    main()
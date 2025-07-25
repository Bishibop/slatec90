# Phase 0 Detailed Implementation Plan

**Date**: July 24, 2025  
**Scope**: 7 SLATEC functions with zero dependencies  
**Timeline**: 2 weeks to full automation

## Executive Summary

This plan details the step-by-step implementation of Phase 0, leveraging existing infrastructure and filling critical gaps. We already have 5 of 7 validators implemented, making this highly achievable.

## Architectural Decisions for Phase 0

### Module Organization
- **Naming Convention**: `{function_name}_module` (e.g., `pimach_module`, `lsame_module`)
- **Structure**: One function per module for all Phase 0 functions
- **Visibility**: Function PUBLIC, all module internals PRIVATE
- **File Naming**: `{function_name}_module.f90` in modern/phase_0/

### Precision Strategy
- **PIMACH**: Keep REAL (single precision as original)
- **I1MACH**: Keep INTEGER 
- **R1MACH**: Modernize to REAL(REAL32) using ISO_FORTRAN_ENV
- **D1MACH**: Modernize to REAL(REAL64) using ISO_FORTRAN_ENV
- **Others**: Keep original types (LOGICAL for LSAME, CHARACTER for AAAAAA)

### Function Signatures
Keep signatures identical to F77 for validation, except:
- Add INTENT specifications
- Use modern kind parameters for precision
- Keep same parameter order and names

## Current State Analysis

### What's Already Working
1. **Fortran Validator**: Supports 5 of 7 Phase 0 functions (LSAME, I1MACH, R1MACH, D1MACH, FDUMP missing only PIMACH and AAAAAA)
2. **Source Files**: All 7 F77 functions present and analyzed
3. **Test Format**: Well-defined and proven with existing functions
4. **OpenAI Integration**: Basic modernizer exists (needs enhancement)

### What's Missing
1. **Validators**: PIMACH and AAAAAA (trivial to add)
2. **Test Cases**: No test files for Phase 0 functions
3. **Modern Implementations**: None exist yet
4. **Orchestration**: No automated pipeline
5. **Directory Structure**: Need modern/, test_cases/, logs/

## Implementation Steps

### Phase 1: Validator Completion (Day 1-2)

#### 1.1 Extend mega_validator_full.f90

**Add PIMACH validation:**
```fortran
subroutine validate_pimach()
    real :: dum, result_f77, result_modern
    real :: pimach_f77
    external pimach_f77
    
    ! Parse parameters
    if (num_real_params >= 1) then
        dum = real_params(1)
    else
        dum = 1.0  ! Default dummy value
    end if
    
    ! Call both versions
    result_f77 = pimach_f77(dum)
    result_modern = pimach_modern(dum)
    
    ! Compare results
    call compare_real_results('PIMACH', result_f77, result_modern, &
                              description, passed)
end subroutine
```

**Add AAAAAA validation:**
```fortran
subroutine validate_aaaaaa()
    character(len=8) :: ver_f77, ver_modern
    
    ! No parameters for AAAAAA
    
    ! Call both versions
    call aaaaaa_f77(ver_f77)
    call aaaaaa_modern(ver_modern)
    
    ! Compare results
    if (trim(ver_f77) == trim(ver_modern)) then
        call report_success('AAAAAA', description)
        passed = .true.
    else
        write(error_unit, '(A,A,A,A,A)') '  FAILED: ', trim(description), &
            ' F77="', trim(ver_f77), '" Modern="', trim(ver_modern), '"'
        passed = .false.
    end if
end subroutine
```

**Add to select case:**
```fortran
case('PIMACH')
    call validate_pimach()
case('AAAAAA')
    call validate_aaaaaa()
```

#### 1.2 Update Makefile
- Add pimach_f77.o and aaaaaa_f77.o to compilation
- Ensure module dependencies are correct

### Phase 2: Manual Test Case Creation (Day 2-3)

#### 2.1 Create phase_0_tests.txt

```
FUNCTION: PIMACH

TEST_START
Basic PI retrieval with positive dummy
REAL_PARAMS: 1.0
TEST_END

TEST_START
PI retrieval with zero dummy
REAL_PARAMS: 0.0
TEST_END

TEST_START
PI retrieval with negative dummy
REAL_PARAMS: -1.0
TEST_END

FUNCTION: AAAAAA

TEST_START
Get SLATEC version
TEST_END

FUNCTION: LSAME

TEST_START
Exact match uppercase
CHAR_PARAMS: A A
TEST_END

TEST_START
Exact match lowercase
CHAR_PARAMS: a a
TEST_END

TEST_START
Case insensitive match
CHAR_PARAMS: a A
TEST_END

TEST_START
Different characters
CHAR_PARAMS: A B
TEST_END

FUNCTION: FDUMP

TEST_START
Call dummy subroutine
TEST_END

FUNCTION: I1MACH

TEST_START
Standard input unit
INT_PARAMS: 1
TEST_END

TEST_START
Standard output unit
INT_PARAMS: 2
TEST_END

TEST_START
Invalid index (too high)
INT_PARAMS: 17
TEST_END

TEST_START
Invalid index (zero)
INT_PARAMS: 0
TEST_END

FUNCTION: R1MACH

TEST_START
Smallest positive magnitude
INT_PARAMS: 1
TEST_END

TEST_START
Largest magnitude
INT_PARAMS: 2
TEST_END

TEST_START
Invalid index
INT_PARAMS: 6
TEST_END

FUNCTION: D1MACH

TEST_START
Smallest positive magnitude
INT_PARAMS: 1
TEST_END

TEST_START
Largest magnitude
INT_PARAMS: 2
TEST_END

TEST_START
Machine epsilon
INT_PARAMS: 3
TEST_END
```

### Phase 3: Create Directory Structure (Day 3)

```bash
mkdir -p modern/phase_0
mkdir -p test_cases/phase_0
mkdir -p logs/phase_0
mkdir -p work/phase_0
```

### Phase 4: Implement Core Python Infrastructure (Day 4-6)

#### 4.1 phase_0_orchestrator.py

```python
#!/usr/bin/env python3
"""
Phase 0 Orchestrator - Main driver for SLATEC modernization
"""
import os
import json
import subprocess
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple
from concurrent.futures import ThreadPoolExecutor, as_completed

from test_generator import TestGenerator
from llm_modernizer import LLMModernizer
from fortran_validator import FortranValidator

# Phase 0 functions in order of complexity
PHASE_0_FUNCTIONS = [
    'PIMACH',   # Simplest - returns constant
    'AAAAAA',   # Returns version string
    'FDUMP',    # Empty subroutine
    'LSAME',    # Character comparison
    'I1MACH',   # Integer constants
    'R1MACH',   # Real constants
    'D1MACH',   # Double constants
]

class Phase0Orchestrator:
    def __init__(self, config_file='config.json'):
        self.config = self._load_config(config_file)
        self.setup_logging()
        self.test_gen = TestGenerator(self.config)
        self.modernizer = LLMModernizer(self.config)
        self.validator = FortranValidator(self.config)
        self.progress = self._load_progress()
        
    def _load_config(self, config_file):
        """Load configuration from JSON file"""
        default_config = {
            'source_dir': 'src',
            'modern_dir': 'modern/phase_0',
            'test_dir': 'test_cases/phase_0',
            'log_dir': 'logs/phase_0',
            'work_dir': 'work/phase_0',
            'max_iterations': 5,
            'parallel_workers': 4,
            'llm_model': 'gpt-4o-mini',
            'temperature': 0.1,
            'validator_executable': 'fortran_validator/mega_validator_full'
        }
        
        if os.path.exists(config_file):
            with open(config_file) as f:
                user_config = json.load(f)
                default_config.update(user_config)
                
        return default_config
        
    def setup_logging(self):
        """Configure logging"""
        log_file = Path(self.config['log_dir']) / f"phase_0_{datetime.now():%Y%m%d_%H%M%S}.log"
        log_file.parent.mkdir(parents=True, exist_ok=True)
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger('Phase0')
        
    def _load_progress(self):
        """Load progress from previous runs"""
        progress_file = Path(self.config['work_dir']) / 'progress.json'
        if progress_file.exists():
            with open(progress_file) as f:
                return json.load(f)
        return {'completed': [], 'failed': [], 'in_progress': []}
        
    def _save_progress(self):
        """Save current progress"""
        progress_file = Path(self.config['work_dir']) / 'progress.json'
        progress_file.parent.mkdir(parents=True, exist_ok=True)
        with open(progress_file, 'w') as f:
            json.dump(self.progress, f, indent=2)
            
    def read_source(self, func_name):
        """Read F77 source code"""
        source_file = Path(self.config['source_dir']) / f"{func_name.lower()}.f"
        if not source_file.exists():
            raise FileNotFoundError(f"Source file not found: {source_file}")
        return source_file.read_text()
        
    def process_function(self, func_name):
        """Process a single function through the modernization pipeline"""
        self.logger.info(f"Processing {func_name}")
        
        try:
            # Skip if already completed
            if func_name in self.progress['completed']:
                self.logger.info(f"{func_name} already completed, skipping")
                return True
                
            # 1. Read source
            f77_code = self.read_source(func_name)
            
            # 2. Generate or load test cases
            test_file = Path(self.config['test_dir']) / f"{func_name.lower()}_tests.txt"
            if not test_file.exists():
                self.logger.info(f"Generating test cases for {func_name}")
                test_content = self.test_gen.generate(func_name, f77_code)
                test_file.parent.mkdir(parents=True, exist_ok=True)
                test_file.write_text(test_content)
            else:
                self.logger.info(f"Using existing test cases for {func_name}")
                test_content = test_file.read_text()
                
            # 3. Initial modernization
            self.logger.info(f"Modernizing {func_name}")
            modern_result = self.modernizer.modernize(func_name, f77_code, test_content)
            
            # Save initial version
            modern_file = Path(self.config['modern_dir']) / f"{func_name.lower()}_module.f90"
            modern_file.parent.mkdir(parents=True, exist_ok=True)
            modern_file.write_text(modern_result['f90_code'])
            
            # Save analysis
            analysis_file = Path(self.config['log_dir']) / f"{func_name.lower()}_analysis.json"
            analysis_file.parent.mkdir(parents=True, exist_ok=True)
            with open(analysis_file, 'w') as f:
                json.dump({
                    'name': func_name,
                    'description': modern_result.get('description', ''),
                    'algorithm_analysis': modern_result.get('algorithm_analysis', ''),
                    'modernization_notes': modern_result.get('modernization_notes', ''),
                    'timestamp': datetime.now().isoformat()
                }, f, indent=2)
                
            # 4. Iterative validation and refinement
            for iteration in range(self.config['max_iterations']):
                self.logger.info(f"Validation iteration {iteration + 1} for {func_name}")
                
                # Compile modern version
                if not self.validator.compile_modern(func_name, modern_file):
                    self.logger.error(f"Compilation failed for {func_name}")
                    if iteration < self.config['max_iterations'] - 1:
                        # Try to fix compilation errors
                        modern_result = self.modernizer.fix_compilation(
                            func_name, 
                            modern_result['f90_code'],
                            self.validator.get_compilation_errors()
                        )
                        modern_file.write_text(modern_result['f90_code'])
                        continue
                    else:
                        break
                        
                # Run validation
                validation_result = self.validator.validate(func_name, test_file)
                
                if validation_result['pass_rate'] == 1.0:
                    self.logger.info(f"✓ {func_name} validated successfully!")
                    self.progress['completed'].append(func_name)
                    self._save_progress()
                    return True
                    
                # Refine if not last iteration
                if iteration < self.config['max_iterations'] - 1:
                    self.logger.info(
                        f"Refining {func_name} - pass rate: {validation_result['pass_rate']*100:.1f}%"
                    )
                    modern_result = self.modernizer.refine(
                        func_name,
                        modern_result['f90_code'],
                        validation_result['errors']
                    )
                    modern_file.write_text(modern_result['f90_code'])
                    
            # Failed after all iterations
            self.logger.error(f"✗ {func_name} failed validation after {self.config['max_iterations']} iterations")
            self.progress['failed'].append(func_name)
            self._save_progress()
            return False
            
        except Exception as e:
            self.logger.error(f"Error processing {func_name}: {e}", exc_info=True)
            self.progress['failed'].append(func_name)
            self._save_progress()
            return False
            
    def process_all(self, parallel=True):
        """Process all Phase 0 functions"""
        remaining = [f for f in PHASE_0_FUNCTIONS 
                    if f not in self.progress['completed']]
        
        if not remaining:
            self.logger.info("All Phase 0 functions already completed!")
            return
            
        self.logger.info(f"Processing {len(remaining)} functions: {remaining}")
        
        if parallel and len(remaining) > 1:
            # Process in parallel
            with ThreadPoolExecutor(max_workers=self.config['parallel_workers']) as executor:
                futures = {
                    executor.submit(self.process_function, func): func 
                    for func in remaining
                }
                
                for future in as_completed(futures):
                    func = futures[future]
                    try:
                        success = future.result()
                        status = "Success" if success else "Failed"
                        self.logger.info(f"Completed {func}: {status}")
                    except Exception as e:
                        self.logger.error(f"Exception processing {func}: {e}")
        else:
            # Process sequentially
            for func in remaining:
                self.process_function(func)
                
        # Final report
        self.generate_report()
        
    def generate_report(self):
        """Generate final report"""
        report = {
            'phase': 'Phase 0',
            'total_functions': len(PHASE_0_FUNCTIONS),
            'completed': len(self.progress['completed']),
            'failed': len(self.progress['failed']),
            'success_rate': len(self.progress['completed']) / len(PHASE_0_FUNCTIONS) * 100,
            'completed_functions': self.progress['completed'],
            'failed_functions': self.progress['failed'],
            'timestamp': datetime.now().isoformat()
        }
        
        report_file = Path(self.config['log_dir']) / 'phase_0_report.json'
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
            
        self.logger.info(f"Phase 0 Report:")
        self.logger.info(f"  Total functions: {report['total_functions']}")
        self.logger.info(f"  Completed: {report['completed']}")
        self.logger.info(f"  Failed: {report['failed']}")
        self.logger.info(f"  Success rate: {report['success_rate']:.1f}%")
        
        
if __name__ == '__main__':
    import argparse
    
    parser = argparse.ArgumentParser(description='Phase 0 SLATEC Modernization')
    parser.add_argument('--function', help='Process single function')
    parser.add_argument('--sequential', action='store_true', help='Process sequentially')
    parser.add_argument('--config', default='config.json', help='Configuration file')
    
    args = parser.parse_args()
    
    orchestrator = Phase0Orchestrator(args.config)
    
    if args.function:
        # Process single function
        success = orchestrator.process_function(args.function.upper())
        exit(0 if success else 1)
    else:
        # Process all
        orchestrator.process_all(parallel=not args.sequential)
```

#### 4.2 test_generator.py

```python
"""
Test Generator for SLATEC functions
Can use manual patterns or LLM generation
"""
import json
import logging
from typing import Dict, List
import openai

class TestGenerator:
    def __init__(self, config):
        self.config = config
        self.logger = logging.getLogger('TestGenerator')
        
        # Initialize OpenAI if API key available
        if 'openai_api_key' in config:
            openai.api_key = config['openai_api_key']
            self.use_llm = True
        else:
            self.use_llm = False
            self.logger.warning("No OpenAI API key found, using manual patterns only")
            
    def generate(self, func_name, source_code):
        """Generate test cases for a function"""
        # First try manual patterns
        manual_tests = self._get_manual_tests(func_name)
        if manual_tests:
            return manual_tests
            
        # Fall back to LLM if available
        if self.use_llm:
            return self._generate_with_llm(func_name, source_code)
        else:
            raise ValueError(f"No test pattern available for {func_name}")
            
    def _get_manual_tests(self, func_name):
        """Get manually defined test cases"""
        # Define test patterns for Phase 0 functions
        patterns = {
            'PIMACH': """FUNCTION: PIMACH

TEST_START
Basic PI retrieval
REAL_PARAMS: 1.0
TEST_END

TEST_START
PI with zero dummy
REAL_PARAMS: 0.0
TEST_END

TEST_START
PI with negative dummy
REAL_PARAMS: -1.0
TEST_END""",

            'AAAAAA': """FUNCTION: AAAAAA

TEST_START
Get SLATEC version
TEST_END""",

            'LSAME': """FUNCTION: LSAME

TEST_START
Exact match uppercase
CHAR_PARAMS: A A
TEST_END

TEST_START
Exact match lowercase
CHAR_PARAMS: a a
TEST_END

TEST_START
Case insensitive match
CHAR_PARAMS: a A
TEST_END

TEST_START
Different characters
CHAR_PARAMS: A B
TEST_END

TEST_START
Numbers
CHAR_PARAMS: 1 1
TEST_END""",

            'FDUMP': """FUNCTION: FDUMP

TEST_START
Call dummy subroutine
TEST_END""",

            'I1MACH': """FUNCTION: I1MACH

TEST_START
Standard input unit
INT_PARAMS: 1
TEST_END

TEST_START
Standard output unit
INT_PARAMS: 2
TEST_END

TEST_START
Standard error unit
INT_PARAMS: 3
TEST_END

TEST_START
Number of bits per integer
INT_PARAMS: 5
TEST_END

TEST_START
Invalid index too high
INT_PARAMS: 17
TEST_END

TEST_START
Invalid index zero
INT_PARAMS: 0
TEST_END

TEST_START
Invalid index negative
INT_PARAMS: -1
TEST_END""",

            'R1MACH': """FUNCTION: R1MACH

TEST_START
Smallest positive magnitude
INT_PARAMS: 1
TEST_END

TEST_START
Largest magnitude
INT_PARAMS: 2
TEST_END

TEST_START
Smallest relative spacing
INT_PARAMS: 3
TEST_END

TEST_START
Largest relative spacing
INT_PARAMS: 4
TEST_END

TEST_START
Log10 of base
INT_PARAMS: 5
TEST_END

TEST_START
Invalid index
INT_PARAMS: 6
TEST_END

TEST_START
Invalid index zero
INT_PARAMS: 0
TEST_END""",

            'D1MACH': """FUNCTION: D1MACH

TEST_START
Smallest positive magnitude
INT_PARAMS: 1
TEST_END

TEST_START
Largest magnitude
INT_PARAMS: 2
TEST_END

TEST_START
Machine epsilon
INT_PARAMS: 3
TEST_END

TEST_START
Invalid index
INT_PARAMS: 6
TEST_END"""
        }
        
        return patterns.get(func_name.upper())
        
    def _generate_with_llm(self, func_name, source_code):
        """Generate test cases using LLM"""
        prompt = f"""You are a Fortran testing expert. Generate comprehensive test cases for a SLATEC function.

Function to test: {func_name}

Source code:
```fortran
{source_code}
```

Analyze this function and generate test cases that cover:
1. Basic functionality with typical inputs
2. Edge cases (zeros, boundary values)
3. Invalid inputs that should return defaults (not errors)
4. All documented parameter ranges

Please respond with a JSON object containing:
{{
    "function_name": "{func_name}",
    "test_description": "Overview of test coverage strategy",
    "test_categories": ["list of test categories covered"],
    "test_cases": "Complete test cases in Fortran validator format",
    "special_considerations": "Any special testing requirements"
}}

The test_cases field should contain the complete test file content in this format:
FUNCTION: {func_name}

TEST_START
Description of test
PARAMS: value1 value2 ...
TEST_END

Use INT_PARAMS for integers, REAL_PARAMS for reals, CHAR_PARAMS for characters."""

        try:
            response = openai.ChatCompletion.create(
                model=self.config.get('llm_model', 'gpt-4o-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"},
                temperature=self.config.get('temperature', 0.1)
            )
            
            result = json.loads(response.choices[0].message.content)
            return result['test_cases']
            
        except Exception as e:
            self.logger.error(f"LLM test generation failed: {e}")
            raise
```

#### 4.3 llm_modernizer.py

```python
"""
LLM-based Fortran modernizer with iterative refinement
"""
import json
import logging
import openai
from typing import Dict, List

class LLMModernizer:
    def __init__(self, config):
        self.config = config
        self.logger = logging.getLogger('LLMModernizer')
        openai.api_key = config.get('openai_api_key')
        
    def modernize(self, func_name, f77_code, test_cases):
        """Initial modernization of F77 to F90"""
        prompt = self._create_modernization_prompt(func_name, f77_code, test_cases)
        
        try:
            response = openai.ChatCompletion.create(
                model=self.config.get('llm_model', 'gpt-4o-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"},
                temperature=self.config.get('temperature', 0.1)
            )
            
            result = json.loads(response.choices[0].message.content)
            self.logger.info(f"Modernization complete for {func_name}")
            return result
            
        except Exception as e:
            self.logger.error(f"Modernization failed for {func_name}: {e}")
            raise
            
    def refine(self, func_name, current_code, validation_errors):
        """Refine based on validation errors"""
        prompt = f"""You are fixing validation errors in a modernized Fortran function.

Function: {func_name}

Current modernized code that has errors:
```fortran
{current_code}
```

Validation errors to fix:
{json.dumps(validation_errors, indent=2)}

Please analyze the errors and provide a corrected version. Common issues:
- Module name mismatches
- Interface differences between F77 and F90
- Precision mismatches
- Missing PURE/ELEMENTAL attributes

Respond with a JSON object containing:
{{
    "name": "{func_name}",
    "error_analysis": "Your understanding of what went wrong",
    "fixes_applied": "Specific changes made to fix the errors",
    "f90_code": "The complete corrected Fortran 90/95 code"
}}"""

        try:
            response = openai.ChatCompletion.create(
                model=self.config.get('llm_model', 'gpt-4o-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"},
                temperature=self.config.get('temperature', 0.1)
            )
            
            result = json.loads(response.choices[0].message.content)
            self.logger.info(f"Refinement complete for {func_name}")
            return result
            
        except Exception as e:
            self.logger.error(f"Refinement failed for {func_name}: {e}")
            raise
            
    def fix_compilation(self, func_name, current_code, compilation_errors):
        """Fix compilation errors"""
        prompt = f"""Fix compilation errors in this modernized Fortran code.

Function: {func_name}

Code with compilation errors:
```fortran
{current_code}
```

Compilation errors:
{compilation_errors}

Common issues:
- Missing USE statements
- Incorrect module/function names
- Type mismatches
- Missing IMPLICIT NONE
- Incorrect INTENT specifications

Respond with JSON containing the corrected f90_code."""

        try:
            response = openai.ChatCompletion.create(
                model=self.config.get('llm_model', 'gpt-4o-mini'),
                messages=[{"role": "user", "content": prompt}],
                response_format={"type": "json_object"},
                temperature=self.config.get('temperature', 0.1)
            )
            
            result = json.loads(response.choices[0].message.content)
            return result
            
        except Exception as e:
            self.logger.error(f"Compilation fix failed for {func_name}: {e}")
            raise
            
    def _create_modernization_prompt(self, func_name, f77_code, test_cases):
        """Create the modernization prompt"""
        return f"""You are a Fortran expert modernizing a SLATEC function from F77 to modern F90/95.

Function: {func_name}

Original Fortran 77 source:
```fortran
{f77_code}
```

Test cases (for understanding usage):
{test_cases}

Modernize following these rules:
1. Create a module named {func_name.lower()}_module
2. Use 'implicit none' always
3. Add INTENT for all arguments
4. Replace GOTO with structured constructs
5. For machine constants (I1MACH, R1MACH, D1MACH):
   - Use intrinsic functions from ISO_FORTRAN_ENV
   - Return 0 for invalid indices (no error stops)
   - R1MACH: Use REAL(REAL32) for all values
   - D1MACH: Use REAL(REAL64) for all values
6. Remove ALL XERMSG calls - return defaults for errors
7. Use PURE/ELEMENTAL where appropriate
8. Preserve exact mathematical behavior
9. Keep same interface (function vs subroutine)
10. For LSAME: Handle the SAVE statement carefully
11. Module structure:
    - PUBLIC only the main function
    - PRIVATE for any module variables or helper procedures
    - Use modern parameter declarations

Respond with JSON:
{{
    "name": "{func_name}",
    "description": "What this function does",
    "algorithm_analysis": "Understanding of the algorithm",
    "modernization_notes": "Key changes made",
    "f90_code": "Complete module code"
}}"""
```

#### 4.4 fortran_validator.py

```python
"""
Fortran validator wrapper
Handles compilation and validation using the Fortran mega-validator
"""
import subprocess
import json
import logging
from pathlib import Path
import tempfile
import shutil

class FortranValidator:
    def __init__(self, config):
        self.config = config
        self.logger = logging.getLogger('FortranValidator')
        self.validator_exe = Path(config['validator_executable'])
        self.work_dir = Path(config['work_dir'])
        self.compilation_errors = []
        
    def compile_modern(self, func_name, modern_file):
        """Compile the modern F90 implementation"""
        try:
            # Create a temporary directory for compilation
            with tempfile.TemporaryDirectory(dir=self.work_dir) as tmpdir:
                tmpdir = Path(tmpdir)
                
                # Copy modern file
                shutil.copy(modern_file, tmpdir / f"{func_name.lower()}_module.f90")
                
                # Compile to object file
                cmd = [
                    'gfortran',
                    '-c',
                    '-O2',
                    '-std=f2008',
                    '-Wall',
                    '-o', f"{func_name.lower()}_modern.o",
                    f"{func_name.lower()}_module.f90"
                ]
                
                result = subprocess.run(
                    cmd,
                    cwd=tmpdir,
                    capture_output=True,
                    text=True
                )
                
                if result.returncode != 0:
                    self.compilation_errors = result.stderr.splitlines()
                    self.logger.error(f"Compilation failed: {result.stderr}")
                    return False
                    
                # Copy successful object file back
                obj_file = tmpdir / f"{func_name.lower()}_modern.o"
                if obj_file.exists():
                    shutil.copy(obj_file, self.work_dir / f"{func_name.lower()}_modern.o")
                    
                return True
                
        except Exception as e:
            self.logger.error(f"Compilation error: {e}")
            return False
            
    def validate(self, func_name, test_file):
        """Run validation tests"""
        try:
            # Run the validator
            with open(test_file) as f:
                test_content = f.read()
                
            result = subprocess.run(
                [str(self.validator_exe)],
                input=test_content,
                capture_output=True,
                text=True
            )
            
            # Parse results
            return self._parse_validation_output(result.stdout, result.stderr)
            
        except Exception as e:
            self.logger.error(f"Validation error: {e}")
            return {
                'pass_rate': 0.0,
                'errors': [str(e)],
                'passed': 0,
                'failed': 0,
                'total': 0
            }
            
    def _parse_validation_output(self, stdout, stderr):
        """Parse validator output to extract results"""
        lines = stdout.splitlines()
        
        # Look for summary statistics
        passed = 0
        failed = 0
        total = 0
        errors = []
        
        for line in lines:
            if 'PASSED:' in line:
                passed += 1
            elif 'FAILED:' in line:
                failed += 1
                errors.append(line.strip())
            elif 'Total tests:' in line:
                # Parse summary line
                parts = line.split()
                for i, part in enumerate(parts):
                    if part == 'Passed:':
                        passed = int(parts[i+1])
                    elif part == 'Failed:':
                        failed = int(parts[i+1])
                        
        total = passed + failed
        pass_rate = passed / total if total > 0 else 0.0
        
        return {
            'pass_rate': pass_rate,
            'errors': errors,
            'passed': passed,
            'failed': failed,
            'total': total
        }
        
    def get_compilation_errors(self):
        """Get the last compilation errors"""
        return '\n'.join(self.compilation_errors)
```

### Phase 5: Version Control Setup (Day 7)

#### 5.1 Create Phase 0 Branch
```bash
git checkout -b phase-0-implementation
git push -u origin phase-0-implementation
```

#### 5.2 Commit Structure
- One commit per function completion
- Include validation results in commit message
- Tag successful phase completion

### Phase 6: Testing and Validation (Day 8-9)

#### 6.1 Test Validator Extensions
```bash
# Compile and test the extended validator
cd fortran_validator
make clean
make

# Test with manual test cases
./mega_validator_full < ../test_cases/phase_0/phase_0_tests.txt
```

#### 6.2 Test Individual Components
```python
# Test the test generator
python -c "
from test_generator import TestGenerator
gen = TestGenerator({})
print(gen._get_manual_tests('PIMACH'))
"

# Test with a single function
python phase_0_orchestrator.py --function PIMACH
```

### Phase 7: Full Pipeline Execution (Day 10-11)

#### 7.1 Sequential Run (for debugging)
```bash
python phase_0_orchestrator.py --sequential
```

#### 7.2 Parallel Run (for speed)
```bash
python phase_0_orchestrator.py
```

#### 7.3 Monitor Progress
```bash
# Watch logs
tail -f logs/phase_0/phase_0_*.log

# Check progress
cat work/phase_0/progress.json | jq
```

### Phase 8: Refinement and Documentation (Day 12-13)

#### 8.1 Analyze Results
- Review logs for common failure patterns
- Update prompts based on errors
- Document successful patterns

#### 8.2 Create Pattern Library
Document successful modernization patterns for:
- Simple constants (PIMACH)
- Character functions (AAAAAA, LSAME)
- Machine constants (I1MACH, R1MACH, D1MACH)
- Empty stubs (FDUMP)

#### 8.3 Prepare for Phase 0.5
- Identify next batch of functions
- Update complexity analysis
- Plan for dependency handling

## Success Criteria

### Functional Success
- [ ] All 7 Phase 0 functions modernized
- [ ] 100% validation pass rate
- [ ] No XERMSG calls in modern code
- [ ] Clean compilation with -Wall

### Infrastructure Success
- [ ] Automated pipeline operational
- [ ] Parallel processing working
- [ ] Progress tracking functional
- [ ] Comprehensive logging

### Quality Metrics
- [ ] Test coverage > 90%
- [ ] No performance regression
- [ ] Thread-safe implementations
- [ ] Modern Fortran best practices

## Risk Management

### Technical Risks
1. **LLM Hallucinations**
   - Mitigation: Strong validation, manual review
   - Backup: Hand-written implementations

2. **API Rate Limits**
   - Mitigation: Implement backoff, caching
   - Backup: Batch processing, queuing

3. **Validator Limitations**
   - Mitigation: Incremental enhancement
   - Backup: Manual validation scripts

### Schedule Risks
1. **Unexpected Complexity**
   - Buffer: 2 extra days built in
   - Escalation: Focus on subset

2. **Infrastructure Issues**
   - Mitigation: Docker containers
   - Backup: Manual processing

## Conclusion

This implementation plan provides a clear, step-by-step path to achieving Phase 0 automation. By leveraging existing infrastructure (5 of 7 validators already work!) and focusing on incremental progress, we can achieve full automation within 2 weeks.

The key to success is:
1. Start with what works (extend validator first)
2. Build incrementally (test each component)
3. Fail fast and iterate (5 refinement cycles)
4. Document everything (for Phase 0.5 and beyond)

Ready to begin implementation!

## Model Choice: o3-mini

We're using OpenAI's o3-mini model (released January 31, 2025) for this project because:
- **Superior STEM capabilities**: Exceptional at math and coding tasks
- **Production-ready**: Supports structured outputs (JSON) natively  
- **Fast iterations**: 24% faster than o1-mini for our refinement loops
- **High accuracy**: 39% fewer major mistakes than o1-mini
- **Cost-effective**: $0.55/1M input, $4.40/1M output tokens

The medium reasoning effort setting provides the optimal balance between speed and accuracy for our Fortran modernization task.

### API Configuration Note
The o3-mini model may require passing the reasoning effort as an API parameter. Update calls as needed:
```python
# If supported by the API
response = openai.ChatCompletion.create(
    model='o3-mini',
    messages=[...],
    response_format={"type": "json_object"},
    temperature=0.1,
    reasoning_effort='medium'  # Check API docs for exact parameter name
)
```
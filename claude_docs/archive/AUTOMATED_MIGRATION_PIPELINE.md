# SLATEC Automated Migration Pipeline with Feedback Loops

## Overview

This document describes a complete automated system for migrating SLATEC's 750+ FORTRAN 77 functions to modern Fortran using LLM-powered test generation and refactoring with iterative improvement loops.

## System Architecture

```
┌─────────────────────┐
│ Function Selection  │ (Dependency-aware ordering)
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Test Generation    │ (Hybrid: Deterministic + LLM)
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Initial Refactoring │ (LLM generates modern version)
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│   Test Execution    │ (Run tests against refactor)
└──────────┬──────────┘
           │
           ▼
      ┌────┴────┐
      │ Pass?   │──Yes──→ [Success: Save & Continue]
      └────┬────┘
           │ No
           ▼
┌─────────────────────┐
│ Feedback to LLM     │ (Send failures for improvement)
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Improved Refactor   │ (LLM fixes based on failures)
└─────────────────────┘
           │
           └──→ [Loop up to 5 times]
```

## Phase 1: Test Generation

### Hybrid Test Generation System

```python
# generate_tests.py

import json
import numpy as np
from scipy import special
import subprocess

class TestGenerator:
    def __init__(self, llm_client=None):
        self.llm_client = llm_client
        self.test_patterns = self.load_test_patterns()
    
    def generate_test_suite(self, function_name):
        """Generate comprehensive test cases using hybrid approach"""
        
        # 1. Read function source
        f77_source = self.read_function_source(function_name)
        
        # 2. Generate deterministic base cases
        base_tests = self.generate_deterministic_tests(function_name)
        
        # 3. Enhance with LLM analysis (if enabled)
        if self.llm_client:
            llm_tests = self.generate_llm_tests(function_name, f77_source)
            all_tests = self.merge_and_deduplicate(base_tests, llm_tests)
        else:
            all_tests = base_tests
        
        # 4. Generate reference values
        reference_values = self.generate_reference_values(function_name, all_tests)
        
        return {
            'function_name': function_name,
            'test_cases': all_tests,
            'reference_values': reference_values,
            'metadata': {
                'total_tests': len(all_tests),
                'base_tests': len(base_tests),
                'llm_tests': len(all_tests) - len(base_tests)
            }
        }
    
    def generate_deterministic_tests(self, function_name):
        """Generate test cases based on function type patterns"""
        
        func_type = self.classify_function(function_name)
        test_cases = []
        
        if func_type == 'special_function':
            if 'BES' in function_name:  # Bessel functions
                test_cases.extend(self.generate_bessel_tests())
            elif 'GAM' in function_name:  # Gamma functions
                test_cases.extend(self.generate_gamma_tests())
            elif 'ERF' in function_name:  # Error functions
                test_cases.extend(self.generate_error_function_tests())
                
        elif func_type == 'utility_function':
            if 'NORM' in function_name:
                test_cases.extend(self.generate_norm_tests())
            elif 'PYTHAG' in function_name:
                test_cases.extend(self.generate_pythag_tests())
                
        # Add universal edge cases
        test_cases.extend(self.generate_edge_cases())
        
        return test_cases
    
    def generate_llm_tests(self, function_name, source_code):
        """Use LLM to generate implementation-specific test cases"""
        
        prompt = f"""
        Analyze this FORTRAN 77 function and generate test cases that thoroughly 
        exercise its implementation. Focus on:
        
        1. Code paths and branches in the implementation
        2. Numerical stability edge cases
        3. Algorithm transition points
        4. Special handling in the code
        
        Function: {function_name}
        Source:
        {source_code}
        
        Return test cases in JSON format:
        [
            {{
                "inputs": [...],
                "reason": "why this test is important",
                "expected_behavior": "what to verify"
            }},
            ...
        ]
        """
        
        response = self.llm_client.generate(prompt)
        return self.parse_llm_test_cases(response)
    
    def generate_bessel_tests(self):
        """Standard test cases for Bessel functions"""
        test_cases = []
        
        # Small arguments (series expansion)
        for order in [0, 1, 2, 5, 10]:
            for x in [0.01, 0.1, 0.5, 1.0, 2.0]:
                test_cases.append({
                    'inputs': [order, x],
                    'type': 'small_argument',
                    'regime': 'series_expansion'
                })
        
        # Large arguments (asymptotic expansion)
        for order in [0, 1, 2]:
            for x in [10.0, 50.0, 100.0, 500.0]:
                test_cases.append({
                    'inputs': [order, x],
                    'type': 'large_argument',
                    'regime': 'asymptotic_expansion'
                })
        
        # Transition regions
        for x in [3.74, 3.75, 3.76]:  # Common transition point
            test_cases.append({
                'inputs': [0, x],
                'type': 'transition',
                'regime': 'algorithm_switch'
            })
        
        return test_cases
    
    def generate_reference_values(self, function_name, test_cases):
        """Generate reference values using F77 execution or scipy"""
        
        references = []
        
        # First try scipy equivalent
        scipy_func = self.get_scipy_equivalent(function_name)
        if scipy_func:
            for test in test_cases:
                try:
                    result = scipy_func(*test['inputs'])
                    references.append({
                        'test_id': test.get('id', len(references)),
                        'inputs': test['inputs'],
                        'expected': float(result),
                        'source': 'scipy'
                    })
                except:
                    # Fall back to F77 execution
                    result = self.execute_f77_function(function_name, test['inputs'])
                    references.append({
                        'test_id': test.get('id', len(references)),
                        'inputs': test['inputs'],
                        'expected': result,
                        'source': 'f77_original'
                    })
        else:
            # Use F77 execution for all
            for test in test_cases:
                result = self.execute_f77_function(function_name, test['inputs'])
                references.append({
                    'test_id': test.get('id', len(references)),
                    'inputs': test['inputs'],
                    'expected': result,
                    'source': 'f77_original'
                })
        
        return references
```

## Phase 2: Initial Refactoring

```python
# refactor_function.py

class FunctionRefactorer:
    def __init__(self, llm_client):
        self.llm_client = llm_client
        self.templates = self.load_refactoring_templates()
    
    def generate_initial_refactor(self, function_name, f77_source, test_suite):
        """Generate initial modern Fortran refactoring"""
        
        # Analyze function characteristics
        analysis = self.analyze_function(function_name, f77_source)
        
        prompt = f"""
        Refactor this FORTRAN 77 function to modern Fortran (2018 standard).
        
        Original function: {function_name}
        Dependencies: {analysis['dependencies']}
        Function type: {analysis['type']}
        
        Source code:
        {f77_source}
        
        Requirements:
        1. Create a module containing the modernized function
        2. Use modern Fortran features:
           - Modules with explicit interfaces
           - Pure/elemental functions where appropriate
           - Intent declarations for all arguments
           - Assumed-shape arrays instead of explicit dimensions
           - Modern control structures (no GO TO)
        3. Preserve exact numerical behavior
        4. Include comprehensive error handling
        5. Create F77-compatible wrapper function
        
        Example test cases that must pass:
        {self.format_test_examples(test_suite[:5])}
        
        Provide:
        1. Complete module implementation
        2. F77 compatibility wrapper
        3. Brief explanation of major changes
        
        Use this structure:
        
        !FILE: {function_name}_modern.f90
        module {function_name}_module
            ...
        end module
        
        !FILE: {function_name}_compat.f90
        ! F77 compatibility wrapper
        ...
        
        !EXPLANATION:
        ...
        """
        
        response = self.llm_client.generate(prompt, temperature=0.2)
        return self.parse_refactoring_response(response, function_name)
    
    def parse_refactoring_response(self, response, function_name):
        """Parse LLM response into separate files"""
        
        files = {}
        current_file = None
        current_content = []
        explanation = ""
        
        for line in response.split('\n'):
            if line.startswith('!FILE:'):
                if current_file:
                    files[current_file] = '\n'.join(current_content)
                current_file = line.split('!FILE:')[1].strip()
                current_content = []
            elif line.startswith('!EXPLANATION:'):
                if current_file:
                    files[current_file] = '\n'.join(current_content)
                current_file = None
                explanation = ""
            elif current_file:
                current_content.append(line)
            else:
                explanation += line + '\n'
        
        if current_file:
            files[current_file] = '\n'.join(current_content)
        
        return {
            'function_name': function_name,
            'module_file': files.get(f'{function_name}_modern.f90', ''),
            'wrapper_file': files.get(f'{function_name}_compat.f90', ''),
            'explanation': explanation.strip(),
            'files': files
        }
```

## Phase 3: Test Execution

```python
# test_executor.py

import subprocess
import tempfile
import os

class TestExecutor:
    def __init__(self, compiler='gfortran'):
        self.compiler = compiler
        self.compile_flags_f77 = ['-std=legacy', '-O2']
        self.compile_flags_modern = ['-std=f2018', '-O2']
    
    def test_refactored_function(self, refactor, test_suite):
        """Compile and test the refactored function"""
        
        with tempfile.TemporaryDirectory() as tmpdir:
            # Save refactored files
            module_path = os.path.join(tmpdir, f"{refactor['function_name']}_modern.f90")
            wrapper_path = os.path.join(tmpdir, f"{refactor['function_name']}_compat.f90")
            
            with open(module_path, 'w') as f:
                f.write(refactor['module_file'])
            with open(wrapper_path, 'w') as f:
                f.write(refactor['wrapper_file'])
            
            # Compile
            compile_result = self.compile_function(
                refactor['function_name'], 
                module_path, 
                wrapper_path,
                tmpdir
            )
            
            if not compile_result['success']:
                return {
                    'success': False,
                    'phase': 'compilation',
                    'errors': compile_result['errors']
                }
            
            # Run tests
            test_results = self.run_test_suite(
                refactor['function_name'],
                test_suite,
                tmpdir
            )
            
            return test_results
    
    def compile_function(self, function_name, module_path, wrapper_path, tmpdir):
        """Compile modern function with dependencies"""
        
        try:
            # Compile module
            cmd = [self.compiler] + self.compile_flags_modern + [
                '-c', module_path, 
                '-o', os.path.join(tmpdir, f'{function_name}_modern.o')
            ]
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode != 0:
                return {
                    'success': False,
                    'errors': result.stderr
                }
            
            # Compile wrapper
            cmd = [self.compiler] + self.compile_flags_modern + [
                '-c', wrapper_path,
                '-o', os.path.join(tmpdir, f'{function_name}_compat.o')
            ]
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode != 0:
                return {
                    'success': False,
                    'errors': result.stderr
                }
            
            return {'success': True}
            
        except Exception as e:
            return {
                'success': False,
                'errors': str(e)
            }
    
    def run_test_suite(self, function_name, test_suite, tmpdir):
        """Run all test cases and collect results"""
        
        failures = []
        passed = 0
        
        for i, test_case in enumerate(test_suite['test_cases']):
            # Create test program
            test_program = self.create_test_program(
                function_name, 
                test_case,
                test_suite['reference_values'][i]
            )
            
            test_path = os.path.join(tmpdir, f'test_{i}.f90')
            with open(test_path, 'w') as f:
                f.write(test_program)
            
            # Compile and run test
            result = self.run_single_test(test_path, tmpdir, i)
            
            if result['passed']:
                passed += 1
            else:
                failures.append({
                    'test_id': i,
                    'test_case': test_case,
                    'expected': test_suite['reference_values'][i]['expected'],
                    'actual': result.get('actual', 'N/A'),
                    'error': result.get('error', ''),
                    'inputs': test_case['inputs']
                })
        
        return {
            'success': len(failures) == 0,
            'passed': passed,
            'failed': len(failures),
            'total': len(test_suite['test_cases']),
            'failures': failures
        }
```

## Phase 4: Feedback Loop

```python
# improve_refactor.py

class RefactorImprover:
    def __init__(self, llm_client):
        self.llm_client = llm_client
    
    def improve_refactor(self, refactor, test_results, original_f77, iteration):
        """Improve refactoring based on test failures"""
        
        # Analyze failure patterns
        failure_analysis = self.analyze_failures(test_results['failures'])
        
        prompt = f"""
        The refactored function is failing {len(test_results['failures'])} out of 
        {test_results['total']} tests. Please fix the implementation.
        
        Current modern implementation:
        {refactor['module_file']}
        
        Current wrapper:
        {refactor['wrapper_file']}
        
        Failed test cases:
        {self.format_failures(test_results['failures'])}
        
        Failure pattern analysis:
        {failure_analysis}
        
        Original F77 for reference:
        {original_f77}
        
        Common issues to check:
        1. Array indexing differences (F77 starts at 1)
        2. Integer division behavior changes
        3. Uninitialized variables (F77 might initialize to 0)
        4. GO TO logic translation errors
        5. Precision/rounding differences
        6. COMMON block data not properly translated
        
        This is iteration {iteration} of 5.
        
        Provide the corrected implementation that fixes these specific failures.
        Focus on the failing test cases and ensure they pass while maintaining
        correctness for the passing tests.
        
        Use the same file structure as before:
        !FILE: {refactor['function_name']}_modern.f90
        ...
        !FILE: {refactor['function_name']}_compat.f90
        ...
        !EXPLANATION:
        What you changed and why
        """
        
        response = self.llm_client.generate(prompt, temperature=0.1)
        improved = self.parse_refactoring_response(response, refactor['function_name'])
        
        # Preserve successful aspects
        improved['previous_version'] = refactor
        improved['iteration'] = iteration
        
        return improved
    
    def analyze_failures(self, failures):
        """Analyze patterns in test failures"""
        
        patterns = {
            'numerical_precision': 0,
            'boundary_cases': 0,
            'sign_errors': 0,
            'off_by_one': 0,
            'overflow_underflow': 0,
            'uninitialized': 0
        }
        
        for failure in failures:
            expected = failure['expected']
            actual = failure.get('actual', 0)
            inputs = failure['inputs']
            
            # Check for precision issues
            if abs(expected) > 0:
                rel_error = abs(actual - expected) / abs(expected)
                if 1e-10 < rel_error < 1e-5:
                    patterns['numerical_precision'] += 1
            
            # Check for sign errors
            if expected * actual < 0:
                patterns['sign_errors'] += 1
            
            # Check for boundary cases
            if any(x in [0, 1, -1] for x in inputs):
                patterns['boundary_cases'] += 1
            
            # Check for overflow/underflow
            if any(abs(x) > 1e10 or (abs(x) < 1e-10 and x != 0) for x in inputs):
                patterns['overflow_underflow'] += 1
        
        analysis = "Failure patterns detected:\n"
        for pattern, count in patterns.items():
            if count > 0:
                analysis += f"- {pattern.replace('_', ' ').title()}: {count} cases\n"
        
        return analysis
```

## Phase 5: Main Migration Pipeline

```python
# migrate_pipeline.py

import json
from datetime import datetime
import logging

class MigrationPipeline:
    def __init__(self, llm_client, max_iterations=5):
        self.test_generator = TestGenerator(llm_client)
        self.refactorer = FunctionRefactorer(llm_client)
        self.test_executor = TestExecutor()
        self.improver = RefactorImprover(llm_client)
        self.max_iterations = max_iterations
        
        # Setup logging
        logging.basicConfig(
            filename=f'migration_{datetime.now().strftime("%Y%m%d_%H%M%S")}.log',
            level=logging.INFO
        )
    
    def migrate_function(self, function_name):
        """Complete migration pipeline for a single function"""
        
        logging.info(f"Starting migration of {function_name}")
        print(f"\n{'='*60}")
        print(f"Migrating {function_name}")
        print(f"{'='*60}")
        
        # Phase 1: Generate comprehensive test suite
        print("Phase 1: Generating test suite...")
        test_suite = self.test_generator.generate_test_suite(function_name)
        print(f"  Generated {len(test_suite['test_cases'])} test cases")
        
        # Phase 2: Generate initial refactoring
        print("Phase 2: Generating initial refactoring...")
        f77_source = self.read_f77_source(function_name)
        refactor = self.refactorer.generate_initial_refactor(
            function_name, 
            f77_source, 
            test_suite
        )
        
        # Phase 3-5: Test and improve loop
        for iteration in range(1, self.max_iterations + 1):
            print(f"\nIteration {iteration}:")
            print("  Testing refactored function...")
            
            test_results = self.test_executor.test_refactored_function(
                refactor, 
                test_suite
            )
            
            if test_results['success']:
                print(f"  ✓ All {test_results['total']} tests passed!")
                
                # Run performance comparison
                perf_results = self.compare_performance(function_name, refactor)
                
                return {
                    'success': True,
                    'function_name': function_name,
                    'iterations': iteration,
                    'test_results': test_results,
                    'performance': perf_results,
                    'refactor': refactor
                }
            
            print(f"  ✗ {test_results['failed']} tests failed")
            
            if iteration < self.max_iterations:
                print("  Improving refactoring based on failures...")
                refactor = self.improver.improve_refactor(
                    refactor,
                    test_results,
                    f77_source,
                    iteration
                )
            else:
                print("  Maximum iterations reached")
        
        # Migration failed
        return {
            'success': False,
            'function_name': function_name,
            'iterations': self.max_iterations,
            'test_results': test_results,
            'refactor': refactor
        }
    
    def migrate_all_functions(self, function_list=None):
        """Migrate all functions in dependency order"""
        
        if function_list is None:
            function_list = self.get_all_functions_in_order()
        
        results = {
            'successful': [],
            'failed': [],
            'skipped': []
        }
        
        for i, function_name in enumerate(function_list):
            print(f"\n[{i+1}/{len(function_list)}] Processing {function_name}")
            
            # Check dependencies
            if not self.dependencies_available(function_name):
                print(f"  Skipping - dependencies not available")
                results['skipped'].append(function_name)
                continue
            
            # Migrate function
            result = self.migrate_function(function_name)
            
            if result['success']:
                results['successful'].append(result)
                self.save_successful_migration(result)
            else:
                results['failed'].append(result)
                self.save_failed_migration(result)
        
        # Generate final report
        self.generate_migration_report(results)
        
        return results
    
    def save_successful_migration(self, result):
        """Save successfully migrated function"""
        
        function_name = result['function_name']
        refactor = result['refactor']
        
        # Save modern implementation
        with open(f"modern/{function_name}_modern.f90", 'w') as f:
            f.write(refactor['module_file'])
        
        # Save wrapper
        with open(f"wrappers/{function_name}_compat.f90", 'w') as f:
            f.write(refactor['wrapper_file'])
        
        # Save migration metadata
        metadata = {
            'function_name': function_name,
            'migration_date': datetime.now().isoformat(),
            'iterations': result['iterations'],
            'test_count': result['test_results']['total'],
            'performance': result['performance']
        }
        
        with open(f"modern/{function_name}_metadata.json", 'w') as f:
            json.dump(metadata, f, indent=2)
    
    def generate_migration_report(self, results):
        """Generate comprehensive migration report"""
        
        report = f"""
# SLATEC Migration Report
Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## Summary
- Total functions: {len(results['successful']) + len(results['failed']) + len(results['skipped'])}
- Successfully migrated: {len(results['successful'])}
- Failed: {len(results['failed'])}
- Skipped (dependencies): {len(results['skipped'])}

## Successful Migrations
"""
        
        for result in results['successful']:
            report += f"- {result['function_name']} (iterations: {result['iterations']})\n"
        
        report += "\n## Failed Migrations\n"
        for result in results['failed']:
            report += f"- {result['function_name']} "
            report += f"(failed tests: {result['test_results']['failed']}/{result['test_results']['total']})\n"
        
        with open('migration_report.md', 'w') as f:
            f.write(report)
```

## Configuration

```python
# migration_config.py

config = {
    # LLM settings
    'llm': {
        'model': 'gpt-4',
        'temperature': 0.2,  # Lower for more consistent code generation
        'max_tokens': 4000,
        'retry_attempts': 3,
        'rate_limit_delay': 1.0  # Seconds between API calls
    },
    
    # Test generation
    'test_generation': {
        'use_llm_enhancement': True,
        'max_tests_per_function': 100,
        'include_edge_cases': True,
        'include_performance_tests': False
    },
    
    # Migration settings
    'migration': {
        'max_iterations': 5,
        'batch_size': 10,  # Functions to process in parallel
        'save_intermediate': True,  # Save after each iteration
        'continue_on_failure': True
    },
    
    # Test tolerances
    'tolerances': {
        'single_precision': 1e-6,
        'double_precision': 1e-12,
        'complex_magnitude': 1e-10
    },
    
    # Performance thresholds
    'performance': {
        'max_slowdown': 1.2,  # Allow 20% slower
        'min_speedup': 0.8,   # Require at least 80% of original speed
    },
    
    # Priority functions (migrate these first)
    'priority_functions': [
        'i1mach', 'r1mach', 'd1mach',  # Machine constants
        'xermsg', 'xerprn',             # Error handling
        'enorm', 'denorm',              # Basic utilities
        'pythag', 'dpythg'
    ]
}
```

## Usage Example

```python
# run_migration.py

from migrate_pipeline import MigrationPipeline
from migration_config import config
import openai

# Setup LLM client
openai.api_key = "your-api-key"
llm_client = openai.ChatCompletion()

# Create pipeline
pipeline = MigrationPipeline(llm_client, max_iterations=config['migration']['max_iterations'])

# Migrate a single function
result = pipeline.migrate_function('enorm')

# Or migrate all functions
results = pipeline.migrate_all_functions()

# Or migrate priority functions first
priority_results = pipeline.migrate_all_functions(config['priority_functions'])
```

## Error Handling and Recovery

```python
class MigrationErrorHandler:
    def handle_compilation_error(self, error, function_name, iteration):
        """Handle compilation failures"""
        
        error_patterns = {
            'undefined reference': 'Missing dependency or incorrect linking',
            'syntax error': 'Modern Fortran syntax issue',
            'type mismatch': 'Interface incompatibility',
            'unclassifiable statement': 'F77 construct not properly translated'
        }
        
        for pattern, description in error_patterns.items():
            if pattern in error.lower():
                return {
                    'type': 'compilation',
                    'pattern': pattern,
                    'description': description,
                    'suggestion': self.get_fix_suggestion(pattern)
                }
        
        return {
            'type': 'compilation',
            'pattern': 'unknown',
            'description': 'Unrecognized compilation error',
            'raw_error': error
        }
    
    def handle_test_failure(self, failures, function_name):
        """Analyze test failures for patterns"""
        
        if len(failures) == len([f for f in failures if f.get('actual') == 0]):
            return {
                'type': 'all_zeros',
                'description': 'Function returning all zeros - likely uninitialized',
                'suggestion': 'Check variable initialization and SAVE attributes'
            }
        
        # More pattern analysis...
```

## Quality Assurance

```python
class QualityChecker:
    def validate_migration(self, original, modern, test_results):
        """Comprehensive validation of migrated function"""
        
        checks = {
            'numerical_accuracy': self.check_numerical_accuracy(test_results),
            'performance': self.check_performance(original, modern),
            'memory_usage': self.check_memory_usage(modern),
            'thread_safety': self.check_thread_safety(modern),
            'interface_compatibility': self.check_interface(original, modern)
        }
        
        return all(checks.values()), checks
    
    def check_numerical_accuracy(self, test_results):
        """Verify numerical accuracy meets requirements"""
        
        for failure in test_results.get('failures', []):
            if failure.get('relative_error', 1.0) > 1e-12:
                return False
        return True
```

## Batch Processing

```python
def process_batch_with_priority():
    """Process functions in intelligent batches"""
    
    # Get dependency graph
    dep_graph = load_dependency_graph()
    
    # Identify layers (functions with same dependency depth)
    layers = topological_sort_layers(dep_graph)
    
    # Process each layer
    for layer_num, layer_functions in enumerate(layers):
        print(f"\nProcessing dependency layer {layer_num}")
        print(f"Functions in this layer: {', '.join(layer_functions)}")
        
        # Can process functions in same layer in parallel
        with multiprocessing.Pool(processes=4) as pool:
            results = pool.map(migrate_function, layer_functions)
        
        # Check if any critical functions failed
        critical_failures = [r for r in results if not r['success'] and r['function_name'] in CRITICAL_FUNCTIONS]
        if critical_failures:
            print("Critical function failed - stopping batch processing")
            break
```

## Monitoring and Reporting

```python
class MigrationMonitor:
    def __init__(self):
        self.start_time = datetime.now()
        self.stats = {
            'attempted': 0,
            'successful': 0,
            'failed': 0,
            'total_iterations': 0,
            'api_calls': 0
        }
    
    def update_progress(self, function_name, result):
        """Update progress statistics"""
        
        self.stats['attempted'] += 1
        if result['success']:
            self.stats['successful'] += 1
        else:
            self.stats['failed'] += 1
        
        self.stats['total_iterations'] += result.get('iterations', 0)
        
        # Calculate metrics
        success_rate = self.stats['successful'] / self.stats['attempted']
        avg_iterations = self.stats['total_iterations'] / self.stats['attempted']
        
        print(f"\nProgress Update:")
        print(f"  Success rate: {success_rate:.1%}")
        print(f"  Average iterations: {avg_iterations:.1f}")
        print(f"  Elapsed time: {datetime.now() - self.start_time}")
```

## Conclusion

This automated pipeline provides:

1. **Intelligent Test Generation**: Combines deterministic patterns with LLM analysis
2. **Automated Refactoring**: LLM generates modern Fortran with proper structure
3. **Feedback-Driven Improvement**: Failed tests guide specific fixes
4. **Quality Assurance**: Every function must pass all tests
5. **Scalability**: Can process all 750+ SLATEC functions systematically

The key innovation is using test failures as concrete feedback to improve refactoring, creating a self-improving system that can handle the complexity of legacy scientific code migration.
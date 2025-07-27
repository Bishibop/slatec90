#!/usr/bin/env python3
"""
SLATEC Orchestrator - Main driver for SLATEC modernization
Generic orchestrator that processes any set of functions
"""
import os
import json
import re
import subprocess
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv

from test_generator import TestGenerator
from modernizer import UnifiedModernizer as LLMModernizer
from validator_wrapper import FortranValidator
from f77_parser import F77Parser

# Claude modernizer removed - using LLM modernizer only

# Load environment variables
load_dotenv()

# No hard-coded function lists - load from JSON files or command line

class SLATECOrchestrator:
    def __init__(self, config_file='config.json', list_name=None, list_file=None, functions=None, debug=False):
        self.config = self._load_config(config_file)
        self.list_name = list_name
        self.list_file = list_file
        self.functions = functions
        self.debug = debug
        self.setup_logging()
        self.test_gen = TestGenerator(self.config)
        
        # Initialize modernizer and validator
        self.modernizer = LLMModernizer(self.config)
        self.validator = FortranValidator(self.config)
            
        self.parser = F77Parser()
        self.function_data = self._load_function_data()
        self.progress = self._load_progress()
        self._ensure_metadata_updated()
        
        # Initialize run tracking
        self.run_id = datetime.now().strftime('%Y%m%d_%H%M%S')
        self.run_results = {
            'run_id': self.run_id,
            'start_time': datetime.now().isoformat(),
            'functions_attempted': [],
            'results': {},
            'totals': {'succeeded': 0, 'partial': 0, 'failed': 0}
        }
        
    def _load_config(self, config_file):
        """Load configuration from JSON file and environment"""
        default_config = {
            'source_dir': 'src',
            'modern_dir': 'modern',
            'test_dir': 'test_cases',
            'log_dir': 'logs',
            'work_dir': 'work',
            'function_lists_dir': 'function_lists',
            'max_iterations': 5,
            'parallel_workers': 4,
            'llm_model': os.getenv('OPENAI_MODEL', 'o3-mini'),
            'validator_executable': 'fortran_validator/validator',
            'openai_api_key': os.getenv('OPENAI_API_KEY')
        }
        
        if os.path.exists(config_file):
            with open(config_file) as f:
                user_config = json.load(f)
                # Don't override API key from environment
                if 'openai_api_key' in user_config and not user_config['openai_api_key']:
                    user_config.pop('openai_api_key')
                default_config.update(user_config)
                
        # Ensure API key is set
        if not default_config.get('openai_api_key'):
            raise ValueError("OPENAI_API_KEY environment variable not set. Please create a .env file or export the variable.")
                
        return default_config
        
    def setup_logging(self):
        """Configure logging"""
        list_id = self.list_name or Path(self.list_file).stem if self.list_file else 'functions'
        log_file = Path(self.config['log_dir']) / f"{list_id}_{datetime.now():%Y%m%d_%H%M%S}.log"
        log_file.parent.mkdir(parents=True, exist_ok=True)
        
        log_level = logging.DEBUG if self.debug else logging.INFO
        logging.basicConfig(
            level=log_level,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger('SLATECOrchestrator')
        
    def _load_progress(self):
        """Load progress from previous runs"""
        list_id = self.list_name or Path(self.list_file).stem if self.list_file else 'functions'
        progress_file = Path(self.config['work_dir']) / 'progress' / f'{list_id}_progress.json'
        if progress_file.exists():
            with open(progress_file) as f:
                return json.load(f)
        return {'completed': [], 'failed': [], 'in_progress': []}
    
    def _load_function_data(self):
        """Load function list and dependencies"""
        if self.list_name:
            # Load predefined list
            list_file = Path(self.config['function_lists_dir']) / f'{self.list_name}.json'
            if not list_file.exists():
                raise FileNotFoundError(f"Function list not found: {list_file}")
            with open(list_file) as f:
                data = json.load(f)
                return {
                    'functions': data['functions'],
                    'dependencies': data.get('dependencies', {}),
                    'name': data.get('name', self.list_name)
                }
        elif self.list_file:
            # Load custom list file
            with open(self.list_file) as f:
                data = json.load(f)
                return {
                    'functions': data['functions'],
                    'dependencies': data.get('dependencies', {}),
                    'name': data.get('name', Path(self.list_file).stem)
                }
        elif self.functions:
            # Direct function list
            return {
                'functions': self.functions,
                'dependencies': {},
                'name': 'custom'
            }
        else:
            raise ValueError("No function list specified")
        
    def _save_progress(self):
        """Save current progress"""
        list_id = self.list_name or Path(self.list_file).stem if self.list_file else 'functions'
        progress_file = Path(self.config['work_dir']) / 'progress' / f'{list_id}_progress.json'
        progress_file.parent.mkdir(parents=True, exist_ok=True)
        with open(progress_file, 'w') as f:
            json.dump(self.progress, f, indent=2)
    
    def _ensure_metadata_updated(self):
        """Ensure validator metadata is up to date with current functions"""
        # Skip full discovery on startup - we'll do per-function discovery as needed
        pass
            
    def read_source(self, func_name):
        """Read F77 source code"""
        source_file = Path(self.config['source_dir']) / f"{func_name.lower()}.f"
        if not source_file.exists():
            raise FileNotFoundError(f"Source file not found: {source_file}")
        return source_file.read_text()
    
    def ensure_dependencies(self, func_name):
        """Ensure function dependencies are built first"""
        deps = self.function_data['dependencies'].get(func_name, [])
        for dep in deps:
            if dep not in self.progress['completed']:
                self.logger.info(f"{func_name} depends on {dep}, building dependency first")
                if not self.process_function(dep):
                    return False
        return True
        
    def _check_signature_exists(self, func_name):
        """Check if function signature exists in the new signature database"""
        sig_db_file = Path('fortran_validator/signature_database.json')
        if sig_db_file.exists():
            with open(sig_db_file) as f:
                sig_db = json.load(f)
                return func_name.upper() in sig_db.get('functions', {})
        return False
    
    def _discover_function_signature(self, func_name):
        """Discover signature for a specific function"""
        auto_discovery = Path('.') / 'auto_signature_discovery.py'
        if auto_discovery.exists():
            self.logger.info(f"Discovering signature for {func_name}...")
            result = subprocess.run([
                'python3', str(auto_discovery), 
                '--function', func_name,
                '--update'  # Update existing database
            ], capture_output=True, text=True)
            
            if result.returncode == 0:
                self.logger.info(f"Successfully discovered signature for {func_name}")
                # Also regenerate function registrations
                gen_registrations = Path('.') / 'generate_function_registrations.py'
                if gen_registrations.exists():
                    subprocess.run(['python3', str(gen_registrations)])
                return True
            else:
                self.logger.error(f"Failed to discover signature: {result.stderr}")
                return False
        return False
    
    def _preflight_checks(self, func_name):
        """Perform pre-flight checks before processing"""
        self.logger.info(f"Running pre-flight checks for {func_name}")
        
        # Check 1: Source file exists
        source_file = Path(self.config['source_dir']) / f"{func_name.lower()}.f"
        if not source_file.exists():
            self.logger.error(f"Source file not found: {source_file}")
            return False, "Source file not found"
        
        # Check 2: Not already modernized (unless forced)
        modern_file = Path(self.config['modern_dir']) / f"{func_name.lower()}_module.f90"
        if modern_file.exists() and func_name not in self.progress.get('failed', []):
            self.logger.warning(f"Modern version already exists: {modern_file}")
            # Could add --force flag to override
        
        # Check 3: Check for naming conflicts
        reserved_names = {'MODULE', 'FUNCTION', 'SUBROUTINE', 'END', 'IF', 'DO', 'WHILE'}
        if func_name.upper() in reserved_names:
            self.logger.error(f"{func_name} is a reserved Fortran keyword")
            return False, "Reserved keyword"
        
        # Check 4: Validate function name format
        if not re.match(r'^[A-Za-z][A-Za-z0-9_]*$', func_name):
            self.logger.error(f"Invalid function name format: {func_name}")
            return False, "Invalid name format"
        
        return True, "All checks passed"
    
    def process_function(self, func_name):
        """Process a single function through the modernization pipeline"""
        self.logger.info(f"Processing {func_name}")
        
        # Track in run results
        self.run_results['functions_attempted'].append(func_name)
        func_start_time = datetime.now()
        func_result = {
            'status': 'in_progress',
            'start_time': func_start_time.isoformat(),
            'test_count': 0,
            'iterations': 0,
            'pass_rate': 0.0,
            'issues': []
        }
        self.run_results['results'][func_name] = func_result
        
        try:
            # Skip if already completed
            if func_name in self.progress['completed']:
                self.logger.info(f"{func_name} already completed, skipping")
                return True
            
            # Pre-flight checks
            checks_passed, check_msg = self._preflight_checks(func_name)
            if not checks_passed:
                self.logger.error(f"Pre-flight checks failed for {func_name}: {check_msg}")
                self.progress['failed'].append(func_name)
                self._save_progress()
                return False
            
            # Ensure dependencies are built first
            if not self.ensure_dependencies(func_name):
                self.logger.error(f"Failed to build dependencies for {func_name}")
                return False
                
            # 1. Read source
            f77_code = self.read_source(func_name)
            
            # 1.5 Check if signature exists in database
            if not self._check_signature_exists(func_name):
                self.logger.warning(f"Signature not found for {func_name}, running auto-discovery...")
                # Discover just this function's signature
                if self._discover_function_signature(func_name):
                    # Verify it was added
                    if not self._check_signature_exists(func_name):
                        self.logger.error(f"Signature discovery succeeded but {func_name} still not in database")
                        return False
                else:
                    self.logger.error(f"Failed to discover signature for {func_name}")
                    return False
            
            # 2. Generate or load test cases
            test_file = Path(self.config['test_dir']) / f"{func_name.lower()}_tests.txt"
            if not test_file.exists():
                self.logger.info(f"Generating test cases for {func_name}")
                test_content = self.test_gen.generate(func_name, f77_code)
                test_file.parent.mkdir(parents=True, exist_ok=True)
                test_file.write_text(test_content)
                # Count generated tests
                test_count = test_content.count('TEST_START')
                self.logger.info(f"Generated {test_count} test cases for {func_name}")
                func_result['test_count'] = test_count
            else:
                self.logger.info(f"Using existing test cases for {func_name}")
                test_content = test_file.read_text()
                # Count existing tests
                test_count = test_content.count('TEST_START')
                self.logger.info(f"Found {test_count} existing test cases for {func_name}")
                func_result['test_count'] = test_count
                
            # 3. Modernization
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
                func_result['iterations'] = iteration + 1
                
                # Compile modern version
                if not self.validator.compile_modern(func_name, modern_file):
                    self.logger.error(f"Compilation failed for {func_name}")
                    compilation_errors = self.validator.get_compilation_errors()
                    func_result['issues'].append(f"Compilation error (iteration {iteration+1}): {compilation_errors}")
                    
                    if iteration < self.config['max_iterations'] - 1:
                        # Try to fix compilation errors
                        modern_result = self.modernizer.fix_compilation(
                            func_name, 
                            modern_result['f90_code'],
                            compilation_errors
                        )
                        modern_file.write_text(modern_result['f90_code'])
                        continue
                    else:
                        break
                        
                # Run validation
                validation_result = self.validator.validate(func_name, test_file)
                func_result['pass_rate'] = validation_result['pass_rate']
            
                # Save debug info if enabled
                if self.debug:
                    debug_dir = Path(self.config['log_dir']) / 'debug' / func_name.lower()
                    debug_dir.mkdir(parents=True, exist_ok=True)
                    
                    # Save iteration state
                    iter_file = debug_dir / f'iteration_{iteration + 1}.json'
                    with open(iter_file, 'w') as f:
                        json.dump({
                            'iteration': iteration + 1,
                            'pass_rate': validation_result['pass_rate'],
                            'errors': validation_result['errors'],
                            'code': modern_result['f90_code']
                        }, f, indent=2)
                    
                    self.logger.info(f"Saved debug info to {iter_file}")
                
                if validation_result['pass_rate'] == 1.0:
                    self.logger.info(f"✓ {func_name} validated successfully!")
                    self.progress['completed'].append(func_name)
                    self._save_progress()
                    
                    # Update run results
                    func_result['status'] = 'success'
                    func_result['end_time'] = datetime.now().isoformat()
                    func_result['time_taken'] = (datetime.now() - func_start_time).total_seconds()
                    self.run_results['totals']['succeeded'] += 1
                    
                    # Regenerate validator include files after successful modernization
                    self._regenerate_validator_includes()
                    
                    return True
                    
                # Refine if not last iteration
                if iteration < self.config['max_iterations'] - 1:
                    self.logger.info(
                        f"Refining {func_name} - pass rate: {validation_result['pass_rate']*100:.1f}%"
                    )
                    # Log failed tests for tracking
                    if validation_result.get('errors'):
                        func_result['issues'].append(
                            f"Iteration {iteration+1}: {len(validation_result['errors'])} tests failed"
                        )
                    
                    modern_result = self.modernizer.refine(
                        func_name,
                        modern_result['f90_code'],
                        validation_result['errors'],
                        original_f77=f77_code,
                        test_cases=test_content,
                        iteration=iteration + 1
                    )
                    modern_file.write_text(modern_result['f90_code'])
                    
            # Failed after all iterations
            self.logger.error(f"✗ {func_name} failed validation after {self.config['max_iterations']} iterations")
            self.progress['failed'].append(func_name)
            self._save_progress()
        
            # Update run results for partial/failed
            func_result['end_time'] = datetime.now().isoformat()
            func_result['time_taken'] = (datetime.now() - func_start_time).total_seconds()
            
            if func_result['pass_rate'] > 0:
                func_result['status'] = 'partial'
                self.run_results['totals']['partial'] += 1
            else:
                func_result['status'] = 'failed'
                self.run_results['totals']['failed'] += 1
                
            # Save detailed failure log
            self._save_failure_details(func_name, func_result, locals().get('validation_result', None))
            
            return False
            
        except Exception as e:
            self.logger.error(f"Error processing {func_name}: {e}", exc_info=True)
            self.progress['failed'].append(func_name)
            self._save_progress()
            
            # Update run results for exception
            func_result['status'] = 'failed'
            func_result['end_time'] = datetime.now().isoformat()
            func_result['time_taken'] = (datetime.now() - func_start_time).total_seconds()
            func_result['issues'].append(f"Exception: {str(e)}")
            self.run_results['totals']['failed'] += 1
            
            # Save failure details
            self._save_failure_details(func_name, func_result, None)
            
            return False
    
    def _save_failure_details(self, func_name, func_result, validation_result):
        """Save detailed information about test failures"""
        issues_dir = Path(self.config['log_dir']) / 'issues'
        issues_dir.mkdir(parents=True, exist_ok=True)
        
        issues_file = issues_dir / f'issues_{func_name.lower()}.txt'
        
        with open(issues_file, 'w') as f:
            f.write(f"Failure Details for {func_name}\n")
            f.write("=" * 50 + "\n\n")
            f.write(f"Pass Rate: {func_result['pass_rate']*100:.1f}%\n")
            f.write(f"Total Tests: {func_result['test_count']}\n")
            f.write(f"Iterations: {func_result['iterations']}\n")
            f.write(f"Time Taken: {func_result['time_taken']:.1f} seconds\n\n")
            
            f.write("Issues Encountered:\n")
            for issue in func_result['issues']:
                f.write(f"- {issue}\n")
            
            if validation_result and validation_result.get('errors'):
                f.write(f"\nFailed Test Cases ({len(validation_result['errors'])}):\n")
                for i, error in enumerate(validation_result['errors'][:20]):  # First 20 errors
                    f.write(f"\nTest {i+1}:\n")
                    f.write(f"  {error}\n")
                if len(validation_result['errors']) > 20:
                    f.write(f"\n... and {len(validation_result['errors']) - 20} more errors\n")
        
        self.logger.info(f"Saved failure details to {issues_file}")
    
    def _save_run_summary(self):
        """Save summary of the current run"""
        self.run_results['end_time'] = datetime.now().isoformat()
        summary_file = Path(self.config['log_dir']) / f'migration_summary_{self.run_id}.json'
        
        with open(summary_file, 'w') as f:
            json.dump(self.run_results, f, indent=2)
        
        self.logger.info(f"Saved run summary to {summary_file}")
        
        # Also generate markdown report
        self._generate_markdown_report()
        
    def _generate_markdown_report(self):
        """Generate a markdown report for the migration run"""
        report_file = Path(self.config['log_dir']) / f'migration_report_{self.run_id}.md'
        
        total_attempted = len(self.run_results['functions_attempted'])
        totals = self.run_results['totals']
        
        report = f"""# Migration Report - {self.run_id}

## Summary
- **Run Date**: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
- **Total Functions**: {total_attempted}
- **Successful (100%)**: {totals['succeeded']}
- **Partial (>0%)**: {totals['partial']}
- **Failed**: {totals['failed']}

## Results by Function
"""
        
        # Group by status
        success_funcs = []
        partial_funcs = []
        failed_funcs = []
        
        for func, result in self.run_results['results'].items():
            if result['status'] == 'success':
                success_funcs.append((func, result))
            elif result['status'] == 'partial':
                partial_funcs.append((func, result))
            else:
                failed_funcs.append((func, result))
        
        if success_funcs:
            report += "\n### ✅ Successful Migrations\n"
            for func, result in sorted(success_funcs):
                report += f"- **{func}** - 100% ({result['test_count']} tests) - {result['time_taken']:.1f}s\n"
        
        if partial_funcs:
            report += "\n### ⚠️ Partial Success\n"
            for func, result in sorted(partial_funcs):
                report += f"- **{func}** - {result['pass_rate']*100:.1f}% ({int(result['test_count']*result['pass_rate'])}/{result['test_count']} tests)\n"
                if result['issues']:
                    report += f"  - Issues: {result['issues'][-1]}\n"
                report += f"  - See: `logs/issues/issues_{func.lower()}.txt`\n"
        
        if failed_funcs:
            report += "\n### ❌ Failed Migrations\n"
            for func, result in sorted(failed_funcs):
                report += f"- **{func}** - Failed\n"
                if result['issues']:
                    report += f"  - Issue: {result['issues'][0]}\n"
                report += f"  - See: `logs/issues/issues_{func.lower()}.txt`\n"
        
        # Performance stats
        total_time = sum(r['time_taken'] for r in self.run_results['results'].values() if 'time_taken' in r)
        avg_time = total_time / total_attempted if total_attempted > 0 else 0
        
        report += f"""
## Performance Statistics
- **Total Time**: {total_time:.1f} seconds ({total_time/60:.1f} minutes)
- **Average Time per Function**: {avg_time:.1f} seconds
"""
        
        with open(report_file, 'w') as f:
            f.write(report)
        
        self.logger.info(f"Generated markdown report at {report_file}")
    
    def _regenerate_validator_includes(self):
        """Regenerate validator include files after successful modernization"""
        try:
            self.logger.info("Regenerating validator include files...")
            
            # Run the discovery script
            discovery_script = Path(self.config['validator_dir']) / 'discover_functions.py'
            
            result = subprocess.run(
                ['python3', str(discovery_script)],
                cwd=self.config['validator_dir'],
                capture_output=True,
                text=True
            )
            
            if result.returncode == 0:
                self.logger.info("✓ Validator include files regenerated successfully")
                
                # Also rebuild the validator
                self.logger.info("Rebuilding validator...")
                rebuild_result = subprocess.run(
                    ['make', 'clean', '&&', 'make'],
                    cwd=self.config['validator_dir'],
                    shell=True,
                    capture_output=True,
                    text=True
                )
                
                if rebuild_result.returncode == 0:
                    self.logger.info("✓ Validator rebuilt successfully")
                else:
                    self.logger.warning(f"Failed to rebuild validator: {rebuild_result.stderr}")
            else:
                self.logger.warning(f"Failed to regenerate include files: {result.stderr}")
                
        except Exception as e:
            self.logger.warning(f"Error regenerating validator includes: {e}")
            # Don't fail the whole process if regeneration fails
            
    def process_all(self, parallel=True):
        """Process all functions in the list"""
        remaining = [f for f in self.function_data['functions'] 
                    if f not in self.progress['completed']]
        
        if not remaining:
            self.logger.info(f"All functions in {self.function_data['name']} already completed!")
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
            # Process sequentially (respects dependencies)
            for func in remaining:
                self.process_function(func)
                
        # Final report
        self.generate_report()
        
    def generate_report(self):
        """Generate final report"""
        total_functions = len(self.function_data['functions'])
        report = {
            'list_name': self.function_data['name'],
            'total_functions': total_functions,
            'completed': len(self.progress['completed']),
            'failed': len(self.progress['failed']),
            'success_rate': len(self.progress['completed']) / total_functions * 100 if total_functions else 0,
            'completed_functions': self.progress['completed'],
            'failed_functions': self.progress['failed'],
            'timestamp': datetime.now().isoformat()
        }
        
        list_id = self.list_name or Path(self.list_file).stem if self.list_file else 'functions'
        report_file = Path(self.config['log_dir']) / f'{list_id}_report.json'
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
            
        self.logger.info(f"Report for {self.function_data['name']}:")
        self.logger.info(f"  Total functions: {report['total_functions']}")
        self.logger.info(f"  Completed: {report['completed']}")
        self.logger.info(f"  Failed: {report['failed']}")
        self.logger.info(f"  Success rate: {report['success_rate']:.1f}%")
        
        # Also generate markdown report
        self.generate_progress_report()
        
        # Save the detailed run summary
        self._save_run_summary()
    
    def generate_progress_report(self):
        """Generate a markdown progress report"""
        report_file = Path(self.config['work_dir']) / 'modernization_progress.md'
        
        total_functions = len(self.function_data['functions'])
        completed = len(self.progress['completed'])
        failed = len(self.progress['failed'])
        in_progress = len(self.progress['in_progress'])
        pending = total_functions - completed - failed - in_progress
        
        report = f"""# SLATEC Modernization Progress Report

Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## Summary
- **Total Functions**: {total_functions}
- **Completed**: {completed} ({completed/total_functions*100:.1f}%)
- **Failed**: {failed} ({failed/total_functions*100:.1f}%)
- **In Progress**: {in_progress}
- **Pending**: {pending}

## Completed Functions
"""
        for func in sorted(self.progress['completed']):
            report += f"- ✅ {func}\n"
        
        if self.progress['failed']:
            report += "\n## Failed Functions\n"
            for func in sorted(self.progress['failed']):
                report += f"- ❌ {func}\n"
        
        if pending > 0:
            report += "\n## Pending Functions\n"
            pending_funcs = [f for f in self.function_data['functions'] 
                           if f not in self.progress['completed'] 
                           and f not in self.progress['failed']
                           and f not in self.progress['in_progress']]
            for func in sorted(pending_funcs)[:10]:  # Show first 10
                report += f"- ⏳ {func}\n"
            if len(pending_funcs) > 10:
                report += f"- ... and {len(pending_funcs) - 10} more\n"
        
        report_file.parent.mkdir(parents=True, exist_ok=True)
        report_file.write_text(report)
        self.logger.info(f"Progress report written to {report_file}")
        
        
if __name__ == '__main__':
    import argparse
    
    parser = argparse.ArgumentParser(description='SLATEC Modernization Orchestrator')
    parser.add_argument('--list', help='Use predefined function list (e.g., trivial, simple)')
    parser.add_argument('--list-file', help='Use custom function list JSON file')
    parser.add_argument('--functions', help='Comma-separated list of functions to process')
    parser.add_argument('--function', help='Process single function')
    parser.add_argument('--sequential', action='store_true', help='Process sequentially')
    parser.add_argument('--config', default='config.json', help='Configuration file')
    parser.add_argument('--summary', action='store_true', help='Show summary of latest migration reports')
    parser.add_argument('--debug', action='store_true', help='Save debug information for each iteration')
    
    args = parser.parse_args()
    
    # Handle --summary flag
    if args.summary:
        from pathlib import Path
        import json
        
        log_dir = Path('logs')
        
        # Find latest migration summary
        summaries = sorted(log_dir.glob('migration_summary_*.json'), key=lambda x: x.stat().st_mtime, reverse=True)
        
        if summaries:
            latest = summaries[0]
            print(f"\nLatest migration summary: {latest.name}")
            print("=" * 60)
            
            with open(latest) as f:
                data = json.load(f)
                
            print(f"Run ID: {data['run_id']}")
            print(f"Functions attempted: {len(data['functions_attempted'])}")
            print(f"Succeeded: {data['totals']['succeeded']}")
            print(f"Partial: {data['totals']['partial']}")
            print(f"Failed: {data['totals']['failed']}")
            
            # Show latest markdown report
            report_file = log_dir / f"migration_report_{data['run_id']}.md"
            if report_file.exists():
                print(f"\nDetailed report: {report_file}")
                print("-" * 60)
                print(report_file.read_text())
        else:
            print("No migration summaries found.")
        
        exit(0)
    
    # Determine function source
    if args.function:
        # Single function
        functions = [args.function.upper()]
        orchestrator = SLATECOrchestrator(args.config, functions=functions, debug=args.debug)
    elif args.functions:
        # Multiple functions from command line
        functions = [f.strip().upper() for f in args.functions.split(',')]
        orchestrator = SLATECOrchestrator(args.config, functions=functions, debug=args.debug)
    elif args.list:
        # Predefined list
        orchestrator = SLATECOrchestrator(args.config, list_name=args.list, debug=args.debug)
    elif args.list_file:
        # Custom list file
        orchestrator = SLATECOrchestrator(args.config, list_file=args.list_file, debug=args.debug)
    else:
        parser.error('Must specify --list, --list-file, --functions, or --function')
    
    if args.function:
        # Process single function
        success = orchestrator.process_function(args.function.upper())
        exit(0 if success else 1)
    else:
        # Process all
        orchestrator.process_all(parallel=not args.sequential)
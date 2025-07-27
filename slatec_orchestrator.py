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

# Load environment variables
load_dotenv()

# No hard-coded function lists - load from JSON files or command line

class SLATECOrchestrator:
    def __init__(self, config_file='config.json', list_name=None, list_file=None, functions=None):
        self.config = self._load_config(config_file)
        self.list_name = list_name
        self.list_file = list_file
        self.functions = functions
        self.setup_logging()
        self.test_gen = TestGenerator(self.config)
        self.modernizer = LLMModernizer(self.config)
        self.validator = FortranValidator(self.config)
        self.parser = F77Parser()
        self.function_data = self._load_function_data()
        self.progress = self._load_progress()
        self._ensure_metadata_updated()
        
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
        
        logging.basicConfig(
            level=logging.INFO,
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
        if 'validator' in self.config.get('validator_executable', ''):
            validator_dir = Path('fortran_validator')
            metadata_gen = validator_dir / 'generate_fortran_metadata.py'
            if metadata_gen.exists():
                self.logger.info("Updating validator metadata for generic validator...")
                result = subprocess.run(['python3', 'generate_fortran_metadata.py'], 
                                      cwd=str(validator_dir),
                                      capture_output=True,
                                      text=True)
                if result.returncode == 0:
                    self.logger.info("Validator metadata updated successfully")
                else:
                    self.logger.warning(f"Failed to update metadata: {result.stderr}")
            
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
        
    def _auto_add_metadata(self, func_name, f77_code):
        """Automatically add function metadata if not present"""
        # Check if metadata exists
        metadata_file = Path('fortran_validator/slatec_metadata.py')
        metadata_content = metadata_file.read_text()
        
        if f"'{func_name.upper()}':" not in metadata_content:
            self.logger.info(f"Metadata not found for {func_name}, extracting from source...")
            
            # Extract metadata
            metadata = self.parser.extract_metadata(f77_code, func_name)
            if metadata:
                # Also discover dependencies
                deps = self.parser.discover_dependencies(f77_code)
                if deps:
                    self.logger.info(f"Discovered dependencies for {func_name}: {deps}")
                    # Update function_data if needed
                    if 'dependencies' not in self.function_data:
                        self.function_data['dependencies'] = {}
                    self.function_data['dependencies'][func_name] = deps
                
                # Add to metadata file
                self._add_to_metadata_file(func_name, metadata)
                
                # Regenerate Fortran metadata
                self._ensure_metadata_updated()
                
                return True
            else:
                self.logger.warning(f"Could not extract metadata for {func_name}")
                return False
        return True
    
    def _add_to_metadata_file(self, func_name, metadata):
        """Add function metadata to slatec_metadata.py"""
        metadata_file = Path('fortran_validator/slatec_metadata.py')
        content = metadata_file.read_text()
        
        # Find the end of SLATEC_FUNCTIONS dict
        insert_pos = content.rfind('}')
        
        # Format metadata as Python code
        metadata_str = f",\n    \n    '{func_name.upper()}': {{\n"
        metadata_str += f"        'type': '{metadata['type']}',\n"
        metadata_str += f"        'params': [\n"
        
        for param in metadata['params']:
            metadata_str += f"            {{"
            for key, value in param.items():
                if isinstance(value, str):
                    metadata_str += f"'{key}': '{value}', "
                else:
                    metadata_str += f"'{key}': {value}, "
            metadata_str = metadata_str.rstrip(', ') + "},\n"
        
        metadata_str += "        ],\n"
        if metadata.get('returns'):
            metadata_str += f"        'returns': '{metadata['returns']}',\n"
        else:
            metadata_str += f"        'returns': None,\n"
        metadata_str += f"        'description': '{metadata['description']}'\n"
        metadata_str += "    }"
        
        # Insert before the closing brace
        new_content = content[:insert_pos] + metadata_str + content[insert_pos:]
        metadata_file.write_text(new_content)
        
        self.logger.info(f"Added metadata for {func_name} to slatec_metadata.py")
    
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
            
            # 1.5 Auto-add metadata if needed
            if not self._auto_add_metadata(func_name, f77_code):
                self.logger.error(f"Failed to add metadata for {func_name}")
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
            else:
                self.logger.info(f"Using existing test cases for {func_name}")
                test_content = test_file.read_text()
                # Count existing tests
                test_count = test_content.count('TEST_START')
                self.logger.info(f"Found {test_count} existing test cases for {func_name}")
                
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
                    
                    # Regenerate validator include files after successful modernization
                    self._regenerate_validator_includes()
                    
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
    
    args = parser.parse_args()
    
    # Determine function source
    if args.function:
        # Single function
        functions = [args.function.upper()]
        orchestrator = SLATECOrchestrator(args.config, functions=functions)
    elif args.functions:
        # Multiple functions from command line
        functions = [f.strip().upper() for f in args.functions.split(',')]
        orchestrator = SLATECOrchestrator(args.config, functions=functions)
    elif args.list:
        # Predefined list
        orchestrator = SLATECOrchestrator(args.config, list_name=args.list)
    elif args.list_file:
        # Custom list file
        orchestrator = SLATECOrchestrator(args.config, list_file=args.list_file)
    else:
        parser.error('Must specify --list, --list-file, --functions, or --function')
    
    if args.function:
        # Process single function
        success = orchestrator.process_function(args.function.upper())
        exit(0 if success else 1)
    else:
        # Process all
        orchestrator.process_all(parallel=not args.sequential)
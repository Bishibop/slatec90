#!/usr/bin/env python3
"""
SLATEC Orchestrator - Main driver for SLATEC modernization
Supports multiple phases of the iterative modernization process
"""
import os
import json
import subprocess
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Tuple
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv

from test_generator import TestGenerator
from llm_modernizer import LLMModernizer
from fortran_validator import FortranValidator

# Load environment variables
load_dotenv()

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

# Phase 0.1 functions - bridge to complex functions
PHASE_0_1_FUNCTIONS = [
    'CDIV',    # Complex division - zero deps
    'PYTHAG',  # Pythagorean - zero deps  
    'CSROOT',  # Complex sqrt - depends on PYTHAG
    'SVOUT',   # Single vector output - depends on I1MACH
    'DVOUT',   # Double vector output - depends on I1MACH
]

# Function dependencies
DEPENDENCIES = {
    'CSROOT': ['PYTHAG'],
    'SVOUT': ['I1MACH'],
    'DVOUT': ['I1MACH']
}

class SLATECOrchestrator:
    def __init__(self, config_file='config.json', phase='0'):
        self.phase = phase
        self.config = self._load_config(config_file)
        self.setup_logging()
        self.test_gen = TestGenerator(self.config)
        self.modernizer = LLMModernizer(self.config)
        self.validator = FortranValidator(self.config)
        self.progress = self._load_progress()
        self.function_list = self._get_function_list()
        
    def _load_config(self, config_file):
        """Load configuration from JSON file and environment"""
        phase_dir = f'phase_{self.phase.replace(".", "_")}'
        default_config = {
            'source_dir': 'src',
            'modern_dir': f'modern/{phase_dir}',
            'test_dir': f'test_cases/{phase_dir}',
            'log_dir': f'logs/{phase_dir}',
            'work_dir': f'work/{phase_dir}',
            'max_iterations': 5,
            'parallel_workers': 4,
            'llm_model': os.getenv('OPENAI_MODEL', 'o3-mini'),
            'validator_executable': 'fortran_validator/mega_validator_full',
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
        log_file = Path(self.config['log_dir']) / f"phase_{self.phase}_{datetime.now():%Y%m%d_%H%M%S}.log"
        log_file.parent.mkdir(parents=True, exist_ok=True)
        
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(f'Phase{self.phase}')
        
    def _load_progress(self):
        """Load progress from previous runs"""
        progress_file = Path(self.config['work_dir']) / 'progress.json'
        if progress_file.exists():
            with open(progress_file) as f:
                return json.load(f)
        return {'completed': [], 'failed': [], 'in_progress': []}
    
    def _get_function_list(self):
        """Get the function list for the current phase"""
        if self.phase == '0':
            return PHASE_0_FUNCTIONS
        elif self.phase == '0.1':
            return PHASE_0_1_FUNCTIONS
        else:
            raise ValueError(f"Unknown phase: {self.phase}")
        
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
    
    def ensure_dependencies(self, func_name):
        """Ensure function dependencies are built first"""
        deps = DEPENDENCIES.get(func_name, [])
        for dep in deps:
            if dep not in self.progress['completed']:
                self.logger.info(f"{func_name} depends on {dep}, building dependency first")
                if not self.process_function(dep):
                    return False
        return True
        
    def process_function(self, func_name):
        """Process a single function through the modernization pipeline"""
        self.logger.info(f"Processing {func_name}")
        
        try:
            # Skip if already completed
            if func_name in self.progress['completed']:
                self.logger.info(f"{func_name} already completed, skipping")
                return True
            
            # Ensure dependencies are built first
            if not self.ensure_dependencies(func_name):
                self.logger.error(f"Failed to build dependencies for {func_name}")
                return False
                
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
        """Process all functions for the current phase"""
        remaining = [f for f in self.function_list 
                    if f not in self.progress['completed']]
        
        if not remaining:
            self.logger.info(f"All Phase {self.phase} functions already completed!")
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
        report = {
            'phase': f'Phase {self.phase}',
            'total_functions': len(self.function_list),
            'completed': len(self.progress['completed']),
            'failed': len(self.progress['failed']),
            'success_rate': len(self.progress['completed']) / len(self.function_list) * 100 if self.function_list else 0,
            'completed_functions': self.progress['completed'],
            'failed_functions': self.progress['failed'],
            'timestamp': datetime.now().isoformat()
        }
        
        report_file = Path(self.config['log_dir']) / f'phase_{self.phase.replace(".", "_")}_report.json'
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
            
        self.logger.info(f"Phase {self.phase} Report:")
        self.logger.info(f"  Total functions: {report['total_functions']}")
        self.logger.info(f"  Completed: {report['completed']}")
        self.logger.info(f"  Failed: {report['failed']}")
        self.logger.info(f"  Success rate: {report['success_rate']:.1f}%")
        
        
if __name__ == '__main__':
    import argparse
    
    parser = argparse.ArgumentParser(description='SLATEC Modernization Orchestrator')
    parser.add_argument('--phase', default='0', help='Phase to process (0, 0.1, etc)')
    parser.add_argument('--function', help='Process single function')
    parser.add_argument('--sequential', action='store_true', help='Process sequentially')
    parser.add_argument('--config', default='config.json', help='Configuration file')
    
    args = parser.parse_args()
    
    orchestrator = SLATECOrchestrator(args.config, phase=args.phase)
    
    if args.function:
        # Process single function
        success = orchestrator.process_function(args.function.upper())
        exit(0 if success else 1)
    else:
        # Process all
        orchestrator.process_all(parallel=not args.sequential)
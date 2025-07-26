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
                    dest_dir = self.work_dir / 'compiled'
                    dest_dir.mkdir(exist_ok=True)
                    shutil.copy(obj_file, dest_dir / f"{func_name.lower()}_modern.o")
                    
                return True
                
        except Exception as e:
            self.logger.error(f"Compilation error: {e}")
            return False
            
    def validate(self, func_name, test_file):
        """Run validation tests using the mega_validator"""
        try:
            # Build the mega_validator if it doesn't exist
            validator_dir = Path(__file__).parent / 'fortran_validator'
            if not (validator_dir / 'validator').exists():
                self.logger.info("Building validator...")
                # First, regenerate metadata if needed
                metadata_gen = validator_dir / 'generate_fortran_metadata.py'
                if metadata_gen.exists():
                    self.logger.info("Regenerating Fortran metadata...")
                    subprocess.run(['python3', str(metadata_gen)], cwd=validator_dir)
                
                result = subprocess.run(
                    ['make', 'validator'],
                    cwd=validator_dir,
                    capture_output=True,
                    text=True
                )
                if result.returncode != 0:
                    self.logger.error(f"Failed to build validator: {result.stderr}")
                    return {
                        'pass_rate': 0.0,
                        'errors': [f'Build failed: {result.stderr}'],
                        'passed': 0,
                        'failed': 1,
                        'total': 1
                    }
            
            # Run the validator with the test file
            validator_exe = validator_dir / 'validator'
            with open(test_file, 'r') as f:
                result = subprocess.run(
                    [str(validator_exe)],
                    input=f.read(),
                    capture_output=True,
                    text=True,
                    cwd=validator_dir
                )
            
            if result.returncode != 0:
                self.logger.error(f"Validator failed: {result.stderr}")
                return {
                    'pass_rate': 0.0,
                    'errors': [f'Validator error: {result.stderr}'],
                    'passed': 0,
                    'failed': 1,
                    'total': 1
                }
            
            # Parse the output
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
            if 'PASSED:' in line or 'PASS:' in line:
                passed += 1
            elif 'FAILED:' in line or 'FAIL:' in line:
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
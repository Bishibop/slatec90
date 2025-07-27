"""
Test Parameter Validator for SLATEC Functions
Validates test parameters against known mathematical constraints
"""
import logging
from typing import Dict, List, Tuple, Optional, Any
import re
import math

class ParameterValidator:
    """Validates test parameters for mathematical correctness"""
    
    def __init__(self):
        self.logger = logging.getLogger('ParameterValidator')
        
        # Function-specific validators
        self.validators = {
            'BSPLVN': self._validate_bsplvn,
            'BSPVN': self._validate_bspvn,
            'DBSPVN': self._validate_dbspvn,
            'DFSPVN': self._validate_dfspvn,
            'I1MACH': self._validate_i1mach,
            'R1MACH': self._validate_r1mach,
            'D1MACH': self._validate_d1mach,
            'DGELS': self._validate_dgels,
            'ENORM': self._validate_enorm,
            'PYTHAG': self._validate_pythag,
            # Add more as needed
        }
        
        # Common constraint checkers
        self.common_validators = {
            'matrix_dimensions': self._check_matrix_dimensions,
            'array_bounds': self._check_array_bounds,
            'numeric_range': self._check_numeric_range,
        }
    
    def validate_test_case(self, func_name: str, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """
        Validate a single test case
        Returns: (is_valid, error_message, suggested_fixes)
        """
        func_name_upper = func_name.upper()
        
        # Apply function-specific validation if available
        if func_name_upper in self.validators:
            return self.validators[func_name_upper](test_data)
        
        # Default validation - check basic constraints
        return self._default_validation(test_data)
    
    def validate_test_file(self, func_name: str, test_content: str) -> Tuple[str, List[Dict]]:
        """
        Validate an entire test file and fix issues
        Returns: (fixed_content, validation_report)
        """
        lines = test_content.strip().split('\n')
        fixed_lines = []
        current_test = {}
        in_test = False
        validation_report = []
        test_number = 0
        
        for line in lines:
            line = line.strip()
            
            if line.startswith('FUNCTION:'):
                fixed_lines.append(line)
                continue
            
            if line == 'TEST_START':
                in_test = True
                current_test = {}
                test_number += 1
                fixed_lines.append(line)
                continue
            
            if line == 'TEST_END' and in_test:
                # Validate the accumulated test
                is_valid, error_msg, fixes = self.validate_test_case(func_name, current_test)
                
                if not is_valid:
                    self.logger.warning(f"Test {test_number} invalid: {error_msg}")
                    
                    # Apply fixes if available
                    if fixes:
                        current_test.update(fixes)
                        # Don't append TEST_END yet - we'll rebuild the test
                        validation_report.append({
                            'test_number': test_number,
                            'status': 'fixed',
                            'original_error': error_msg,
                            'fixes_applied': fixes
                        })
                    else:
                        validation_report.append({
                            'test_number': test_number,
                            'status': 'invalid',
                            'error': error_msg
                        })
                        fixed_lines.append(line)  # Keep TEST_END for invalid tests
                else:
                    validation_report.append({
                        'test_number': test_number,
                        'status': 'valid'
                    })
                    fixed_lines.append(line)  # Keep TEST_END for valid tests
                
                # If we have fixes, rebuild the test
                if not is_valid and fixes:
                    # Remove lines back to TEST_START
                    while fixed_lines and fixed_lines[-1] != 'TEST_START':
                        fixed_lines.pop()
                    
                    # Rebuild with fixed values
                    if 'description' in current_test:
                        fixed_lines.append(f"Description: {current_test['description']}")
                    
                    if 'int_params' in current_test:
                        fixed_lines.append(f"INT_PARAMS: {' '.join(map(str, current_test['int_params']))}")
                    
                    if 'real_params' in current_test:
                        real_strs = []
                        for val in current_test['real_params']:
                            if str(val) == 'nan':
                                real_strs.append('NaN')
                            elif str(val) == 'inf':
                                real_strs.append('Inf')
                            elif str(val) == '-inf':
                                real_strs.append('-Inf')
                            else:
                                real_strs.append(f"{val:g}")
                        fixed_lines.append(f"REAL_PARAMS: {' '.join(real_strs)}")
                    
                    if 'array_size' in current_test:
                        fixed_lines.append(f"ARRAY_SIZE: {current_test['array_size']}")
                    
                    if 'real_array' in current_test:
                        real_strs = [f"{val:g}" for val in current_test['real_array']]
                        fixed_lines.append(f"REAL_ARRAY: {' '.join(real_strs)}")
                    
                    if 'char_params' in current_test:
                        fixed_lines.append(f"CHAR_PARAMS: {' '.join(current_test['char_params'])}")
                    
                    fixed_lines.append('TEST_END')
                
                in_test = False
                current_test = {}
                continue
            
            if in_test:
                # Parse test data
                if line.startswith('Description:'):
                    current_test['description'] = line[12:].strip()
                elif line.startswith('INT_PARAMS:'):
                    current_test['int_params'] = self._parse_int_params(line[11:])
                elif line.startswith('REAL_PARAMS:'):
                    current_test['real_params'] = self._parse_real_params(line[12:])
                elif line.startswith('ARRAY_SIZE:'):
                    current_test['array_size'] = int(line[11:].strip())
                elif line.startswith('REAL_ARRAY:'):
                    current_test['real_array'] = self._parse_real_params(line[11:])
                elif line.startswith('CHAR_PARAMS:'):
                    current_test['char_params'] = line[12:].strip().split()
                
                fixed_lines.append(line)
            else:
                fixed_lines.append(line)
        
        return '\n'.join(fixed_lines), validation_report
    
    def _parse_int_params(self, param_str: str) -> List[int]:
        """Parse integer parameters from string"""
        try:
            return [int(x) for x in param_str.strip().split()]
        except ValueError:
            self.logger.error(f"Failed to parse int params: {param_str}")
            return []
    
    def _parse_real_params(self, param_str: str) -> List[float]:
        """Parse real parameters from string, handling scientific notation"""
        params = []
        for token in param_str.strip().split():
            try:
                # Fix common issues with scientific notation
                # Remove double exponents like 1e19e0
                if token.count('e') > 1 or token.count('E') > 1:
                    # Keep only first exponent
                    match = re.match(r'([+-]?\d*\.?\d+)[eE]([+-]?\d+)', token)
                    if match:
                        token = match.group(0)
                
                params.append(float(token))
            except ValueError:
                self.logger.error(f"Failed to parse real param: {token}")
                # Try to fix and continue
                if 'inf' in token.lower():
                    params.append(float('inf') if 'inf' in token else float('-inf'))
                else:
                    params.append(0.0)  # Default to 0
        
        return params
    
    def _rewrite_test_in_lines(self, lines: List[str], test_data: Dict):
        """Rewrite the last test in lines with fixed data"""
        # Find the last TEST_START
        test_start_idx = -1
        for i in range(len(lines) - 1, -1, -1):
            if lines[i] == 'TEST_START':
                test_start_idx = i
                break
        
        if test_start_idx == -1:
            return
        
        # Find corresponding TEST_END
        test_end_idx = -1
        for i in range(test_start_idx, len(lines)):
            if lines[i] == 'TEST_END':
                test_end_idx = i
                break
        
        if test_end_idx == -1:
            return
        
        # Rebuild test lines
        new_test_lines = ['TEST_START']
        
        if 'description' in test_data:
            new_test_lines.append(f"Description: {test_data['description']}")
        
        if 'int_params' in test_data:
            new_test_lines.append(f"INT_PARAMS: {' '.join(map(str, test_data['int_params']))}")
        
        if 'real_params' in test_data:
            real_strs = []
            for val in test_data['real_params']:
                if math.isnan(val):
                    real_strs.append('NaN')
                elif math.isinf(val):
                    real_strs.append('Inf' if val > 0 else '-Inf')
                else:
                    real_strs.append(f"{val:g}")
            new_test_lines.append(f"REAL_PARAMS: {' '.join(real_strs)}")
        
        if 'array_size' in test_data:
            new_test_lines.append(f"ARRAY_SIZE: {test_data['array_size']}")
        
        if 'real_array' in test_data:
            real_strs = [f"{val:g}" for val in test_data['real_array']]
            new_test_lines.append(f"REAL_ARRAY: {' '.join(real_strs)}")
        
        if 'char_params' in test_data:
            new_test_lines.append(f"CHAR_PARAMS: {' '.join(test_data['char_params'])}")
        
        new_test_lines.append('TEST_END')
        
        # Replace old test with new
        lines[test_start_idx:test_end_idx+1] = new_test_lines
    
    # Function-specific validators
    
    def _validate_bsplvn(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate BSPLVN parameters"""
        # BSPLVN requires special handling for INDEX parameter and knot constraints
        # For now, just check basic constraints
        if 'int_params' not in test_data:
            return False, "Missing INT_PARAMS", None
        
        # Basic validation - can be enhanced
        return True, None, None
    
    def _validate_bspvn(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate BSPVN parameters (similar to BSPLVN)"""
        return self._validate_bsplvn(test_data)
    
    def _validate_dbspvn(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate DBSPVN parameters (double precision BSPVN)"""
        return self._validate_bsplvn(test_data)
    
    def _validate_dfspvn(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate DFSPVN parameters"""
        return self._validate_bsplvn(test_data)
    
    def _validate_i1mach(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate I1MACH parameters"""
        if 'int_params' not in test_data or len(test_data['int_params']) == 0:
            return False, "Missing INT_PARAMS", None
        
        i = test_data['int_params'][0]
        if i < 1 or i > 16:
            return False, f"I1MACH index {i} out of range [1,16]", {
                'int_params': [min(max(i, 1), 16)]
            }
        
        return True, None, None
    
    def _validate_r1mach(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate R1MACH parameters"""
        if 'int_params' not in test_data or len(test_data['int_params']) == 0:
            return False, "Missing INT_PARAMS", None
        
        i = test_data['int_params'][0]
        if i < 1 or i > 5:
            return False, f"R1MACH index {i} out of range [1,5]", {
                'int_params': [min(max(i, 1), 5)]
            }
        
        return True, None, None
    
    def _validate_d1mach(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate D1MACH parameters"""
        return self._validate_r1mach(test_data)  # Same constraints as R1MACH
    
    def _validate_dgels(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate DGELS (least squares) parameters"""
        # Would need to check matrix dimension constraints
        # For now, basic validation
        return True, None, None
    
    def _validate_enorm(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate ENORM parameters"""
        if 'int_params' not in test_data or len(test_data['int_params']) == 0:
            return False, "Missing INT_PARAMS (N)", None
        
        n = test_data['int_params'][0]
        if n < 1:
            return False, f"ENORM N={n} must be positive", {
                'int_params': [max(n, 1)]
            }
        
        if 'array_size' in test_data:
            if test_data['array_size'] != n:
                return False, f"Array size {test_data['array_size']} doesn't match N={n}", {
                    'array_size': n
                }
        
        if 'real_array' in test_data:
            if len(test_data['real_array']) != n:
                # Truncate or pad array
                fixed_array = test_data['real_array'][:n] if len(test_data['real_array']) > n else \
                              test_data['real_array'] + [0.0] * (n - len(test_data['real_array']))
                return False, f"Array length doesn't match N={n}", {
                    'real_array': fixed_array
                }
        
        return True, None, None
    
    def _validate_pythag(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Validate PYTHAG parameters"""
        if 'real_params' not in test_data or len(test_data['real_params']) < 2:
            return False, "PYTHAG requires 2 REAL_PARAMS", None
        
        a, b = test_data['real_params'][0], test_data['real_params'][1]
        
        # Check for both NaN (causes infinite loop in some implementations)
        if math.isnan(a) and math.isnan(b):
            return False, "Both inputs NaN would cause infinite loop", {
                'real_params': [0.0, 1.0]  # Safe default values
            }
        
        return True, None, None
    
    def _default_validation(self, test_data: Dict) -> Tuple[bool, Optional[str], Optional[Dict]]:
        """Default validation for functions without specific validators"""
        # Basic sanity checks
        
        # Check for malformed numbers in real params/arrays
        if 'real_params' in test_data:
            for i, val in enumerate(test_data['real_params']):
                if not isinstance(val, (int, float)):
                    return False, f"Invalid real parameter at position {i}", None
        
        if 'real_array' in test_data:
            for i, val in enumerate(test_data['real_array']):
                if not isinstance(val, (int, float)):
                    return False, f"Invalid real array value at position {i}", None
        
        return True, None, None
    
    # Common constraint checkers
    
    def _check_matrix_dimensions(self, m: int, n: int, lda: int, ldb: Optional[int] = None) -> Tuple[bool, str]:
        """Check matrix dimension constraints"""
        if lda < max(1, m):
            return False, f"LDA={lda} must be >= max(1, M={m})"
        
        if ldb is not None and ldb < max(1, m, n):
            return False, f"LDB={ldb} must be >= max(1, M={m}, N={n})"
        
        return True, ""
    
    def _check_array_bounds(self, index: int, array_size: int) -> Tuple[bool, str]:
        """Check array index bounds"""
        if index < 1 or index > array_size:
            return False, f"Index {index} out of bounds [1, {array_size}]"
        return True, ""
    
    def _check_numeric_range(self, value: float, min_val: float, max_val: float, name: str) -> Tuple[bool, str]:
        """Check if value is in valid range"""
        if value < min_val or value > max_val:
            return False, f"{name}={value} out of range [{min_val}, {max_val}]"
        return True, ""
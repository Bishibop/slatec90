#!/usr/bin/env python3
"""
Comprehensive Validation Framework for SLATEC Functions

This script provides additional validation beyond the blind testing and 
official SLATEC test suite by cross-checking against multiple reference
implementations and mathematical properties.
"""

import math
import numpy as np
import scipy.special
from typing import List, Tuple, Callable, Any
import subprocess
import json

class MathValidator:
    """Cross-validation against multiple mathematical libraries"""
    
    def __init__(self, function_name: str):
        self.function_name = function_name
        self.tolerance = 1e-6  # Single precision expectation
        
    def compare_implementations(self, inputs: List[Any]) -> dict:
        """Compare our implementation against reference libraries"""
        results = {}
        
        # Our SLATEC F90 implementation
        results['slatec_f90'] = self._call_slatec_f90(inputs)
        
        # Original SLATEC F77 (if available)
        try:
            results['slatec_f77'] = self._call_slatec_f77(inputs)
        except:
            results['slatec_f77'] = None
            
        # NumPy/SciPy reference
        results['scipy'] = self._call_scipy_reference(inputs)
        
        # GSL via SciPy
        results['gsl'] = self._call_gsl_reference(inputs)
        
        return results
    
    def validate_mathematical_properties(self, inputs: List[Any]) -> List[str]:
        """Test mathematical properties specific to each function"""
        failures = []
        
        if self.function_name.upper() == 'PYTHAG':
            failures.extend(self._validate_pythag_properties(inputs))
        elif self.function_name.upper() == 'ENORM':
            failures.extend(self._validate_norm_properties(inputs))
        elif self.function_name.upper() in ['GAMMA', 'DGAMMA']:
            failures.extend(self._validate_gamma_properties(inputs))
            
        return failures
    
    def _validate_pythag_properties(self, inputs: List[float]) -> List[str]:
        """Validate PYTHAG mathematical properties"""
        failures = []
        a, b = inputs[0], inputs[1]
        result = self._call_slatec_f90([a, b])
        
        # Property 1: result² = a² + b²
        expected_square = a*a + b*b
        if abs(result*result - expected_square) > self.tolerance * expected_square:
            failures.append(f"Pythagorean theorem violated: {result}² ≠ {a}² + {b}²")
            
        # Property 2: result ≥ max(|a|, |b|)
        max_input = max(abs(a), abs(b))
        if result < max_input * (1 - self.tolerance):
            failures.append(f"Result {result} < max input {max_input}")
            
        # Property 3: Symmetry
        result_swapped = self._call_slatec_f90([b, a])
        if abs(result - result_swapped) > self.tolerance:
            failures.append(f"Not symmetric: pythag({a},{b}) ≠ pythag({b},{a})")
            
        return failures
    
    def _validate_norm_properties(self, inputs: List[Any]) -> List[str]:
        """Validate ENORM/DENORM properties"""
        failures = []
        n, x = inputs[0], inputs[1]
        result = self._call_slatec_f90([n, x])
        
        # Property 1: Non-negative
        if result < 0:
            failures.append(f"Norm is negative: {result}")
            
        # Property 2: Zero vector has zero norm
        if all(abs(xi) < 1e-15 for xi in x) and abs(result) > self.tolerance:
            failures.append(f"Zero vector should have zero norm, got {result}")
            
        # Property 3: Scaling property ||kx|| = |k| * ||x||
        if len(x) > 0 and max(abs(xi) for xi in x) > 0:
            k = 2.5
            scaled_x = [k * xi for xi in x]
            scaled_result = self._call_slatec_f90([n, scaled_x])
            expected = abs(k) * result
            if abs(scaled_result - expected) > self.tolerance * expected:
                failures.append(f"Scaling property violated: ||{k}x|| ≠ {k}||x||")
                
        return failures
    
    def _call_slatec_f90(self, inputs: List[Any]) -> float:
        """Call our modern F90 implementation"""
        # This would compile and run the F90 version
        # Implementation depends on the specific function
        # For now, placeholder
        return 0.0
    
    def _call_scipy_reference(self, inputs: List[Any]) -> float:
        """Call SciPy/NumPy reference implementation"""
        func_map = {
            'PYTHAG': lambda a, b: np.hypot(a, b),
            'ENORM': lambda n, x: np.linalg.norm(x[:n]),
            'GAMMA': lambda x: scipy.special.gamma(x),
            'ZABS': lambda ar, ai: abs(complex(ar, ai)),
        }
        
        func = func_map.get(self.function_name.upper())
        if func:
            return func(*inputs)
        return None
    
    def _call_gsl_reference(self, inputs: List[Any]) -> float:
        """Call GSL reference (via SciPy wrappers)"""
        # Most GSL functions are available through SciPy
        return self._call_scipy_reference(inputs)

class NISTValidator:
    """Validate against NIST reference data"""
    
    def __init__(self):
        self.nist_data = self._load_nist_data()
    
    def _load_nist_data(self) -> dict:
        """Load NIST reference values"""
        # This would load from downloaded NIST datasets
        # For now, some known exact values
        return {
            'GAMMA': [
                (0.5, 1.7724538509055160273),  # sqrt(π)
                (1.0, 1.0),                    # 0!
                (2.0, 1.0),                    # 1!
                (3.0, 2.0),                    # 2!
            ],
            'ERF': [
                (0.0, 0.0),
                (float('inf'), 1.0),
            ]
        }
    
    def validate_function(self, function_name: str, test_function: Callable) -> List[str]:
        """Validate against NIST reference values"""
        failures = []
        reference_data = self.nist_data.get(function_name.upper(), [])
        
        for input_val, expected in reference_data:
            try:
                result = test_function(input_val)
                relative_error = abs(result - expected) / abs(expected) if expected != 0 else abs(result)
                
                if relative_error > 1e-10:  # High precision expectation for exact values
                    failures.append(f"NIST mismatch: {function_name}({input_val}) = {result}, expected {expected}")
            except Exception as e:
                failures.append(f"Error computing {function_name}({input_val}): {e}")
                
        return failures

def run_comprehensive_validation(function_name: str, test_cases: List[List[Any]]) -> dict:
    """Run all validation methods on a function"""
    results = {
        'cross_validation': [],
        'property_validation': [],
        'nist_validation': [],
        'summary': {'total_tests': 0, 'failures': 0}
    }
    
    validator = MathValidator(function_name)
    nist_validator = NISTValidator()
    
    for test_case in test_cases:
        # Cross-validation against multiple implementations
        implementations = validator.compare_implementations(test_case)
        
        # Check if all implementations agree
        values = [v for v in implementations.values() if v is not None]
        if len(values) > 1:
            max_diff = max(values) - min(values)
            if max_diff > validator.tolerance * max(abs(v) for v in values):
                results['cross_validation'].append({
                    'inputs': test_case,
                    'implementations': implementations,
                    'max_difference': max_diff
                })
        
        # Mathematical property validation
        property_failures = validator.validate_mathematical_properties(test_case)
        results['property_validation'].extend(property_failures)
    
    # NIST validation (if applicable)
    def test_func(x):
        return validator._call_slatec_f90([x])
    
    nist_failures = nist_validator.validate_function(function_name, test_func)
    results['nist_validation'].extend(nist_failures)
    
    # Summary
    total_failures = (len(results['cross_validation']) + 
                     len(results['property_validation']) + 
                     len(results['nist_validation']))
    
    results['summary'] = {
        'total_tests': len(test_cases),
        'cross_validation_failures': len(results['cross_validation']),
        'property_failures': len(results['property_validation']),
        'nist_failures': len(results['nist_validation']),
        'total_failures': total_failures,
        'success_rate': 1 - (total_failures / max(len(test_cases), 1))
    }
    
    return results

if __name__ == "__main__":
    # Example usage
    test_cases = [
        [3.0, 4.0],      # PYTHAG test case
        [0.0, 1.0],      # Edge case
        [1e-10, 1e-10],  # Underflow test
    ]
    
    results = run_comprehensive_validation("PYTHAG", test_cases)
    print(json.dumps(results, indent=2))
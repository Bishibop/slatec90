#!/usr/bin/env python3
"""
Analyze validation errors to provide better feedback to the LLM
"""
import json
import re
from pathlib import Path
from typing import Dict, List

class ValidationErrorAnalyzer:
    def __init__(self):
        self.error_patterns = {
            'ierr_missing': r'error flag|ierr|IERR',
            'numerical_mismatch': r'Expected:.*Actual:|relative error|tolerance',
            'type_mismatch': r'type mismatch|incompatible types',
            'algorithm_flow': r'lookup table|integer handling|wrong branch',
            'overflow': r'overflow|HUGE|infinity',
            'underflow': r'underflow|denormal|tiny',
            'special_cases': r'zero|negative|NaN|special case'
        }
    
    def analyze_function_errors(self, func_name: str, errors: List[str]) -> Dict:
        """Analyze errors for a specific function"""
        analysis = {
            'function': func_name,
            'total_errors': len(errors),
            'error_categories': {},
            'specific_issues': [],
            'recommendations': []
        }
        
        # Categorize errors
        for error in errors:
            for category, pattern in self.error_patterns.items():
                if re.search(pattern, error, re.IGNORECASE):
                    if category not in analysis['error_categories']:
                        analysis['error_categories'][category] = []
                    analysis['error_categories'][category].append(error)
        
        # Function-specific analysis
        if func_name.upper() == 'GAMLN':
            analysis['specific_issues'] = self._analyze_gamln_errors(errors)
            analysis['recommendations'] = [
                "Check lookup table logic - should be checked FIRST for exact integers",
                "Verify zinc calculation: should be ZMIN - INT(Z), not ZMIN - Z",
                "Ensure error handling matches F77 (IERR parameter required)",
                "Algorithm flow: lookup table → asymptotic expansion → recurrence"
            ]
        
        return analysis
    
    def _analyze_gamln_errors(self, errors: List[str]) -> List[str]:
        """GAMLN-specific error analysis"""
        issues = []
        
        # Check for specific test patterns
        failing_tests = []
        for error in errors:
            if 'Test' in error and 'FAIL' in error:
                test_match = re.search(r'Test (\d+)', error)
                if test_match:
                    failing_tests.append(int(test_match.group(1)))
        
        if 1 in failing_tests and 2 in failing_tests:
            issues.append("Error handling tests failing - IERR parameter missing or PURE conflict")
        
        if any(3 <= t <= 13 for t in failing_tests):
            issues.append("Non-integer value tests failing - algorithm flow issue")
        
        return issues
    
    def generate_llm_guidance(self, analysis: Dict) -> str:
        """Generate specific guidance for LLM refinement"""
        guidance = []
        
        if 'ierr_missing' in analysis['error_categories']:
            guidance.append("""
IERR Parameter Issue:
- The F77 function has an IERR output parameter for error status
- Tests expect this parameter to be set (1 for error, 0 for success)
- If using PURE, remove it since PURE functions cannot modify output parameters
- Alternative: Keep PURE but return special values (like -HUGE) for errors
""")
        
        if 'numerical_mismatch' in analysis['error_categories']:
            guidance.append("""
Numerical Accuracy Issue:
- Results differ between F77 and F90 implementations
- Check algorithm flow matches F77 exactly
- Verify all mathematical operations use same precision
- Look for differences in:
  * Order of operations
  * Intermediate variable calculations
  * Lookup table vs computation logic
""")
        
        if analysis['function'] == 'GAMLN' and analysis['specific_issues']:
            guidance.append("""
GAMLN-Specific Issues:
1. Lookup Table Logic:
   - F77 checks for exact integers FIRST (lines 142-148)
   - Only if not an exact integer in range 1-100, proceed to computation
   
2. ZINC Calculation (line 163):
   - Should be: ZINC = ZMIN - NZ (where NZ = INT(Z))
   - NOT: ZINC = ZMIN - Z
   
3. Algorithm Flow:
   a) Check if Z is exact integer 1-100 → use lookup table
   b) Calculate ZMIN based on machine precision
   c) If Z < ZMIN, shift up by ZINC and use recurrence
   d) Apply asymptotic expansion
   e) If shifted, correct using recurrence relation
""")
        
        return '\n'.join(guidance)

if __name__ == '__main__':
    # Example usage
    analyzer = ValidationErrorAnalyzer()
    
    # Read errors from file
    error_file = Path('logs/issues/issues_gamln.txt')
    if error_file.exists():
        with open(error_file) as f:
            content = f.read()
        
        # Extract error list (simple parsing)
        errors = []
        in_errors = False
        for line in content.splitlines():
            if 'Failed Test Cases' in line:
                in_errors = True
            elif in_errors and 'FAIL:' in line:
                errors.append(line.strip())
        
        analysis = analyzer.analyze_function_errors('GAMLN', errors)
        print(json.dumps(analysis, indent=2))
        
        print("\n" + "="*60)
        print("LLM Guidance:")
        print("="*60)
        print(analyzer.generate_llm_guidance(analysis))
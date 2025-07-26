"""
F77 Parser - Extracts metadata from Fortran 77 source files
Automates function signature extraction and dependency discovery
"""
import re
from typing import Dict, List, Optional, Tuple
import logging

class F77Parser:
    def __init__(self):
        self.logger = logging.getLogger('F77Parser')
        
    def extract_metadata(self, f77_code: str, func_name: str) -> Optional[Dict]:
        """Extract function metadata from F77 source code"""
        try:
            # Normalize function name
            func_name = func_name.upper()
            
            # Find function/subroutine declaration
            func_pattern = rf'^\s*(FUNCTION|SUBROUTINE)\s+{func_name}\s*\((.*?)\)'
            func_match = re.search(func_pattern, f77_code, re.MULTILINE | re.IGNORECASE)
            
            if not func_match:
                self.logger.error(f"Could not find declaration for {func_name}")
                return None
                
            func_type = func_match.group(1).upper()
            param_string = func_match.group(2).strip()
            
            # Parse parameters
            param_names = []
            if param_string:
                # Split by comma, handling nested parentheses
                param_names = [p.strip() for p in self._split_params(param_string)]
            
            # Extract parameter types and intents
            params = []
            for param in param_names:
                param_info = self._extract_param_info(f77_code, param)
                if param_info:
                    params.append(param_info)
            
            # Build metadata structure
            metadata = {
                'type': 'function' if func_type == 'FUNCTION' else 'subroutine',
                'params': params,
                'description': self._extract_description(f77_code, func_name)
            }
            
            # For functions, determine return type
            if func_type == 'FUNCTION':
                metadata['returns'] = self._extract_return_type(f77_code, func_name)
            else:
                metadata['returns'] = None
                
            return metadata
            
        except Exception as e:
            self.logger.error(f"Error parsing {func_name}: {e}")
            return None
    
    def discover_dependencies(self, f77_code: str) -> List[str]:
        """Extract function dependencies from CALL statements"""
        dependencies = []
        
        # Remove comment lines to avoid false matches
        code_lines = []
        for line in f77_code.split('\n'):
            # Skip comment lines and prologue
            if line.strip() and not line.strip().startswith(('C', 'c', '*', '!')):
                code_lines.append(line)
        clean_code = '\n'.join(code_lines)
        
        # Find CALL statements
        call_pattern = r'CALL\s+(\w+)\s*\('
        calls = re.findall(call_pattern, clean_code, re.IGNORECASE)
        
        # Find function calls (harder to detect reliably)
        # Look for known patterns like PYTHAG(x,y) in expressions
        func_pattern = r'(?:=|[+\-*/])\s*(\w+)\s*\([^)]+\)'
        funcs = re.findall(func_pattern, clean_code)
        
        # Combine and deduplicate
        all_deps = set(calls + funcs)
        
        # Filter out intrinsic functions
        intrinsics = {
            'ABS', 'SQRT', 'EXP', 'LOG', 'SIN', 'COS', 'TAN', 'ASIN', 'ACOS', 'ATAN',
            'SINH', 'COSH', 'TANH', 'MIN', 'MAX', 'MOD', 'SIGN', 'DIM', 'DPROD',
            'LEN', 'INDEX', 'AIMAG', 'CONJG', 'REAL', 'CMPLX', 'INT', 'NINT',
            'AINT', 'ANINT', 'FLOAT', 'SNGL', 'DBLE', 'FLOOR', 'CEILING',
            'IABS', 'IDIM', 'ISIGN', 'IDINT', 'IFIX', 'REAL', 'AIMAG', 'DABS'
        }
        
        dependencies = [dep.upper() for dep in all_deps if dep.upper() not in intrinsics]
        return sorted(list(set(dependencies)))
    
    def _split_params(self, param_string: str) -> List[str]:
        """Split parameters handling nested parentheses"""
        params = []
        current = ''
        paren_depth = 0
        
        for char in param_string:
            if char == ',' and paren_depth == 0:
                params.append(current.strip())
                current = ''
            else:
                current += char
                if char == '(':
                    paren_depth += 1
                elif char == ')':
                    paren_depth -= 1
        
        if current.strip():
            params.append(current.strip())
        
        return params
    
    def _extract_param_info(self, f77_code: str, param_name: str) -> Optional[Dict]:
        """Extract type and intent information for a parameter"""
        param_info = {
            'name': param_name.upper(),
            'type': 'real',  # Default assumption
            'intent': 'in'   # Default assumption
        }
        
        # Look for type declarations
        type_patterns = [
            (r'^\s*INTEGER.*\b' + param_name + r'\b', 'integer'),
            (r'^\s*REAL.*\b' + param_name + r'\b', 'real'),
            (r'^\s*DOUBLE\s+PRECISION.*\b' + param_name + r'\b', 'double'),
            (r'^\s*COMPLEX.*\b' + param_name + r'\b', 'complex'),
            (r'^\s*LOGICAL.*\b' + param_name + r'\b', 'logical'),
            (r'^\s*CHARACTER.*\b' + param_name + r'\b', 'character'),
        ]
        
        for pattern, ptype in type_patterns:
            if re.search(pattern, f77_code, re.MULTILINE | re.IGNORECASE):
                param_info['type'] = ptype
                break
        
        # Check for array dimensions
        dim_pattern = rf'{param_name}\s*\(([^)]+)\)'
        dim_match = re.search(dim_pattern, f77_code, re.IGNORECASE)
        if dim_match:
            param_info['dimension'] = dim_match.group(1).strip()
        
        # Check for character length
        if param_info['type'] == 'character':
            char_pattern = rf'CHARACTER\s*\*\s*\(?\s*(\d+|\*)\s*\)?.*\b{param_name}\b'
            char_match = re.search(char_pattern, f77_code, re.IGNORECASE)
            if char_match:
                size = char_match.group(1)
                param_info['size'] = int(size) if size.isdigit() else size
        
        # Determine intent by usage (simplified heuristic)
        # If parameter appears on left side of assignment, it's OUT or INOUT
        assign_pattern = rf'^\s*{param_name}\s*(?:\([^)]*\))?\s*='
        if re.search(assign_pattern, f77_code, re.MULTILINE | re.IGNORECASE):
            # Check if also used on right side (INOUT)
            use_pattern = rf'=.*\b{param_name}\b'
            if re.search(use_pattern, f77_code, re.IGNORECASE):
                param_info['intent'] = 'inout'
            else:
                param_info['intent'] = 'out'
        
        return param_info
    
    def _extract_return_type(self, f77_code: str, func_name: str) -> str:
        """Extract return type for a function"""
        # Check for typed function declaration
        typed_pattern = rf'^\s*(INTEGER|REAL|DOUBLE\s+PRECISION|COMPLEX|LOGICAL)\s+FUNCTION\s+{func_name}'
        typed_match = re.search(typed_pattern, f77_code, re.MULTILINE | re.IGNORECASE)
        
        if typed_match:
            rtype = typed_match.group(1).upper()
            return 'double' if 'DOUBLE' in rtype else rtype.lower()
        
        # Check for separate type declaration
        type_patterns = [
            (rf'^\s*INTEGER\s+{func_name}\b', 'integer'),
            (rf'^\s*REAL\s+{func_name}\b', 'real'),
            (rf'^\s*DOUBLE\s+PRECISION\s+{func_name}\b', 'double'),
            (rf'^\s*COMPLEX\s+{func_name}\b', 'complex'),
            (rf'^\s*LOGICAL\s+{func_name}\b', 'logical'),
        ]
        
        for pattern, rtype in type_patterns:
            if re.search(pattern, f77_code, re.MULTILINE | re.IGNORECASE):
                return rtype
        
        # Default to REAL for functions
        return 'real'
    
    def _extract_description(self, f77_code: str, func_name: str) -> str:
        """Extract function description from prologue"""
        # Look for PURPOSE in prologue
        purpose_pattern = r'C\*\*\*PURPOSE\s+(.+?)(?=C\*\*\*|$)'
        purpose_match = re.search(purpose_pattern, f77_code, re.MULTILINE)
        
        if purpose_match:
            return purpose_match.group(1).strip()
        
        # Fallback to DESCRIPTION
        desc_pattern = r'C\*\*\*DESCRIPTION\s*\n((?:C.*\n)+)'
        desc_match = re.search(desc_pattern, f77_code, re.MULTILINE)
        
        if desc_match:
            # Extract first meaningful line
            lines = desc_match.group(1).split('\n')
            for line in lines:
                cleaned = line.strip().lstrip('C').strip()
                if cleaned and not cleaned.startswith('*'):
                    return cleaned
        
        return f"SLATEC function {func_name}"
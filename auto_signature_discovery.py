#!/usr/bin/env python3
"""
Automatic F77 Signature Discovery Script

Uses f2py to automatically extract function signatures from F77 source files
and generates a comprehensive signature database for the validator system.
"""

import os
import re
import json
import subprocess
import tempfile
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass

@dataclass
class Parameter:
    name: str
    type: str
    intent: str  # 'in', 'out', 'inout'
    is_array: bool
    array_spec: str = ""

@dataclass
class FunctionSignature:
    name: str
    is_function: bool
    return_type: Optional[str]
    parameters: List[Parameter]
    signature_pattern: str
    signature_type: int

class F77SignatureDiscovery:
    def __init__(self, base_dir=None):
        if base_dir is None:
            self.base_dir = Path(__file__).parent
        else:
            self.base_dir = Path(base_dir)
            
        self.src_dir = self.base_dir / "src"
        self.modern_dir = self.base_dir / "modern"
        self.validator_dir = self.base_dir / "fortran_validator"
        
        # Type mappings
        self.type_map = {
            'real': 'TYPE_REAL',
            'integer': 'TYPE_INTEGER', 
            'character': 'TYPE_CHARACTER',
            'double precision': 'TYPE_DOUBLE',
            'logical': 'TYPE_LOGICAL'
        }
        
        # Signature pattern cache
        self.signature_patterns = {}
        self.next_signature_type = 1
        
    def extract_signature_with_f2py(self, f77_file: Path) -> Optional[FunctionSignature]:
        """Extract function signature using f2py."""
        try:
            # Use a simple filename in current directory
            temp_pyf = Path(f"{f77_file.stem}_signature.pyf")
            
            # Run f2py to extract signature
            result = subprocess.run([
                'f2py', '-h', str(temp_pyf), str(f77_file)
            ], capture_output=True, text=True)
            
            if result.returncode != 0:
                print(f"  f2py failed: {result.stderr.strip()}")
                return None
                
            # Check if pyf file was created and has content
            if not temp_pyf.exists() or temp_pyf.stat().st_size == 0:
                print(f"  f2py produced no output")
                return None
                
            # Parse the .pyf file
            signature = self.parse_pyf_file(temp_pyf, f77_file)
            
            # Clean up
            temp_pyf.unlink()
            
            return signature
            
        except Exception as e:
            print(f"  Exception: {e}")
            return None
    
    def parse_pyf_file(self, pyf_file: Path, f77_file: Path = None) -> Optional[FunctionSignature]:
        """Parse f2py-generated .pyf file to extract signature."""
        with open(pyf_file, 'r') as f:
            content = f.read()
        
        # Extract function/subroutine declaration
        func_match = re.search(r'^(function|subroutine)\s+(\w+)\((.*?)\)', content, re.MULTILINE | re.IGNORECASE)
        if not func_match:
            return None
            
        is_function = func_match.group(1).lower() == 'function'
        func_name = func_match.group(2).upper()
        param_list = func_match.group(3)
        
        # Extract parameter declarations
        parameters = []
        param_names = [p.strip() for p in param_list.split(',') if p.strip()]
        
        for param_name in param_names:
            param = self.extract_parameter_info(content, param_name, f77_file)
            if param:
                parameters.append(param)
        
        # Extract return type for functions
        return_type = None
        if is_function:
            return_match = re.search(rf'^\s*(\w+(?:\s+\w+)?)\s*::\s*{func_name.lower()}', content, re.MULTILINE)
            if return_match:
                return_type = return_match.group(1).strip()
        
        # Generate signature pattern and type
        pattern = self.generate_signature_pattern(is_function, return_type, parameters)
        sig_type = self.get_signature_type(pattern)
        
        return FunctionSignature(
            name=func_name,
            is_function=is_function,
            return_type=return_type,
            parameters=parameters,
            signature_pattern=pattern,
            signature_type=sig_type
        )
    
    def extract_parameter_info(self, content: str, param_name: str, f77_file: Path = None) -> Optional[Parameter]:
        """Extract parameter type and array information."""
        # Find parameter declaration
        param_match = re.search(rf'^\s*(\w+(?:\s+\w+)?)\s+(?:dimension\(([^)]+)\)\s+::\s*|::\s*){param_name}', 
                               content, re.MULTILINE | re.IGNORECASE)
        
        if not param_match:
            return None
            
        param_type = param_match.group(1).strip().lower()
        array_spec = param_match.group(2) if param_match.group(2) else ""
        is_array = bool(array_spec) or 'dimension' in content.lower()
        
        # Infer intent (F77 doesn't have explicit intent, so we make educated guesses)
        intent = self.infer_parameter_intent(param_name, param_type, is_array, f77_file)
        
        return Parameter(
            name=param_name,
            type=param_type,
            intent=intent,
            is_array=is_array,
            array_spec=array_spec
        )
    
    def infer_parameter_intent(self, param_name: str, param_type: str, is_array: bool, f77_file: Path = None) -> str:
        """Infer parameter intent from name patterns, type, and F77 source analysis."""
        param_lower = param_name.lower()
        
        # Definite output parameters
        if any(suffix in param_lower for suffix in ['out', 'result', 'flag', 'err', 'info', 'left']):
            return 'out'
            
        # Definite input parameters 
        if any(prefix in param_lower for prefix in ['n', 'm', 'l', 'len', 'lx', 'ly', 'lz']):
            return 'in'
        if param_lower in ['x', 'y', 'z', 'a', 'b', 'c', 'tol', 'eps']:
            return 'in'
            
        # Try to analyze F77 source if available
        if f77_file and f77_file.exists():
            intent = self.analyze_f77_parameter_usage(f77_file, param_name)
            if intent:
                return intent
        
        # Arrays often input/output, scalars usually input
        if is_array:
            return 'inout'
        else:
            return 'in'
            
    def analyze_f77_parameter_usage(self, f77_file: Path, param_name: str) -> Optional[str]:
        """Analyze F77 source to determine parameter intent."""
        try:
            with open(f77_file, 'r') as f:
                content = f.read().upper()
            
            param_upper = param_name.upper()
            
            # Look for assignment patterns
            # If parameter is assigned to: likely output
            assign_patterns = [
                rf'^[ ]*{param_upper}[ ]*=',  # Direct assignment
                rf'^[ ]*{param_upper}\([^)]*\)[ ]*=',  # Array assignment
            ]
            
            for pattern in assign_patterns:
                if re.search(pattern, content, re.MULTILINE):
                    return 'out'
            
            # If parameter is only used in expressions: likely input
            # This is a simple heuristic - could be improved
            return 'in'
            
        except Exception:
            return None
    
    def generate_signature_pattern(self, is_function: bool, return_type: Optional[str], 
                                 parameters: List[Parameter]) -> str:
        """Generate a signature pattern string for classification."""
        if is_function:
            # Clean return type - remove spaces, normalize
            clean_return = self.clean_type_name(return_type or 'UNKNOWN')
            pattern = f"FUNC_{clean_return}"
        else:
            pattern = "SUB"
            
        # Add parameter pattern
        param_pattern = []
        for param in parameters:
            clean_type = self.clean_type_name(param.type)
            if param.is_array:
                param_pattern.append(f"{clean_type}_ARR")
            else:
                param_pattern.append(clean_type)
                
        if param_pattern:
            pattern += "_" + "_".join(param_pattern)
        else:
            pattern += "_0_PARAMS"
            
        # Ensure pattern is not too long for Fortran (max 63 chars)
        if len(pattern) > 50:  # Conservative limit
            # Use a hash-based approach for very long patterns
            import hashlib
            pattern_hash = hashlib.md5(pattern.encode()).hexdigest()[:8].upper()
            pattern = pattern[:40] + "_" + pattern_hash
            
        return pattern
    
    def clean_type_name(self, type_name: str) -> str:
        """Clean type name to be valid Fortran identifier."""
        if not type_name:
            return "UNKNOWN"
            
        # Replace problematic characters
        clean = type_name.upper()
        clean = clean.replace(" ", "_")
        clean = clean.replace("DOUBLE_PRECISION", "DOUBLE")
        clean = clean.replace("PRECISION", "PREC")
        
        # Truncate if too long
        if len(clean) > 15:
            clean = clean[:15]
            
        return clean
    
    def get_signature_type(self, pattern: str) -> int:
        """Get or create signature type number for pattern."""
        if pattern in self.signature_patterns:
            return self.signature_patterns[pattern]
            
        # Assign new signature type
        sig_type = self.next_signature_type
        self.signature_patterns[pattern] = sig_type
        self.next_signature_type += 1
        
        return sig_type
    
    def discover_all_signatures(self) -> Dict[str, FunctionSignature]:
        """Discover signatures for all F77 files."""
        signatures = {}
        
        # Get all .f files in src directory
        f77_files = list(self.src_dir.glob("*.f"))
        
        print(f"Discovering signatures from {len(f77_files)} F77 files...")
        
        for f77_file in sorted(f77_files):
            print(f"Processing {f77_file.name}...")
            signature = self.extract_signature_with_f2py(f77_file)
            
            if signature:
                signatures[signature.name.lower()] = signature
                print(f"  ✓ {signature.name}: {signature.signature_pattern}")
            else:
                print(f"  ✗ Failed to extract signature")
                
        return signatures
    
    def generate_signature_constants(self) -> str:
        """Generate Fortran constants for all signature types."""
        lines = []
        
        # Add unknown type
        lines.append("    integer, parameter :: SIG_UNKNOWN = 0")
        
        # Add discovered signature types
        for pattern, sig_type in sorted(self.signature_patterns.items(), key=lambda x: x[1]):
            const_name = f"SIG_{pattern}"
            lines.append(f"    integer, parameter :: {const_name} = {sig_type}")
            
        return '\n'.join(lines)
    
    def generate_function_registry(self, signatures: Dict[str, FunctionSignature]) -> str:
        """Generate Fortran function registry array."""
        lines = [
            f"    integer, parameter :: NUM_FUNCTIONS = {len(signatures)}",
            "",
            "    type(function_info), parameter :: FUNCTION_REGISTRY(NUM_FUNCTIONS) = [ &"
        ]
        
        entries = []
        for name, sig in sorted(signatures.items()):
            # Build parameter arrays
            param_types = []
            param_intents = []
            
            for i in range(10):  # Fixed array size
                if i < len(sig.parameters):
                    param = sig.parameters[i]
                    param_types.append(self.type_map.get(param.type, '1'))  # Default to integer
                    intent_map = {'in': '1', 'out': '2', 'inout': '3'}
                    param_intents.append(intent_map.get(param.intent, '1'))
                else:
                    param_types.append('0')
                    param_intents.append('0')
            
            return_type_num = '0'
            if sig.return_type:
                return_type_num = self.type_map.get(sig.return_type, '2')  # Default to real
            
            entry = f"""        function_info( &
            name='{sig.name:<20}', &
            signature_type={sig.signature_type}, &
            is_function={'.true.' if sig.is_function else '.false.'}, &
            num_params={len(sig.parameters)}, &
            param_types=[{','.join(param_types)}], &
            param_intents=[{','.join(param_intents)}], &
            return_type={return_type_num} &
        )"""
            entries.append(entry)
        
        # Join entries with proper Fortran array syntax
        if len(entries) == 1:
            lines.append("    " + entries[0] + " &")
        else:
            lines.append("    " + entries[0] + ", &")
            for entry in entries[1:-1]:
                lines.append("    " + entry + ", &")
            lines.append("    " + entries[-1] + " &")
        lines.append("    ]")
        
        return '\n'.join(lines)
    
    def generate_signature_module(self, signatures: Dict[str, FunctionSignature]) -> str:
        """Generate complete slatec_signatures_module.f90 file."""
        constants = self.generate_signature_constants()
        registry = self.generate_function_registry(signatures)
        
        module_template = f"""module slatec_signatures_module
    implicit none
    
    ! Public parameters
    public :: SIG_UNKNOWN, function_info, get_function_info, get_signature_type
    public :: TYPE_INTEGER, TYPE_REAL, TYPE_CHARACTER, TYPE_DOUBLE, TYPE_LOGICAL
    public :: INTENT_IN, INTENT_OUT, INTENT_INOUT
    
    ! Signature type constants - AUTO-GENERATED
{constants}
    
    ! Function information type
    type :: function_info
        character(len=20) :: name
        integer :: signature_type
        logical :: is_function  ! true for function, false for subroutine
        integer :: num_params
        integer :: param_types(10)  ! 1=int, 2=real, 3=char, 4=double, 5=logical
        integer :: param_intents(10) ! 1=in, 2=out, 3=inout
        integer :: return_type      ! for functions only
    end type function_info
    
    ! Parameter type constants
    integer, parameter :: TYPE_INTEGER = 1
    integer, parameter :: TYPE_REAL = 2
    integer, parameter :: TYPE_CHARACTER = 3
    integer, parameter :: TYPE_DOUBLE = 4
    integer, parameter :: TYPE_LOGICAL = 5
    
    ! Intent constants
    integer, parameter :: INTENT_IN = 1
    integer, parameter :: INTENT_OUT = 2
    integer, parameter :: INTENT_INOUT = 3
    
    ! Function registry - AUTO-GENERATED
{registry}
    
contains

    function get_function_info(func_name) result(info)
        character(len=*), intent(in) :: func_name
        type(function_info) :: info
        integer :: i
        character(len=20) :: upper_name
        
        ! Convert to uppercase for comparison
        upper_name = func_name
        call to_upper(upper_name)
        
        ! Search registry
        do i = 1, NUM_FUNCTIONS
            if (trim(FUNCTION_REGISTRY(i)%name) == trim(upper_name)) then
                info = FUNCTION_REGISTRY(i)
                return
            end if
        end do
        
        ! Not found - return unknown
        info%name = upper_name
        info%signature_type = SIG_UNKNOWN
        info%is_function = .false.
        info%num_params = 0
        info%param_types = 0
        info%param_intents = 0
        info%return_type = 0
    end function
    
    function get_signature_type(func_name) result(sig_type)
        character(len=*), intent(in) :: func_name
        integer :: sig_type
        type(function_info) :: info
        
        info = get_function_info(func_name)
        sig_type = info%signature_type
    end function
    
    subroutine to_upper(str)
        character(len=*), intent(inout) :: str
        integer :: i
        
        do i = 1, len_trim(str)
            if (str(i:i) >= 'a' .and. str(i:i) <= 'z') then
                str(i:i) = char(iachar(str(i:i)) - 32)
            end if
        end do
    end subroutine

end module slatec_signatures_module"""
        
        return module_template
    
    def save_signature_database(self, signatures: Dict[str, FunctionSignature]):
        """Save signature database to JSON."""
        db = {
            'metadata': {
                'generated_by': 'auto_signature_discovery.py',
                'num_functions': len(signatures),
                'signature_patterns': self.signature_patterns
            },
            'functions': {}
        }
        
        for name, sig in signatures.items():
            db['functions'][name] = {
                'name': sig.name,
                'is_function': sig.is_function,
                'return_type': sig.return_type,
                'signature_pattern': sig.signature_pattern,
                'signature_type': sig.signature_type,
                'parameters': [
                    {
                        'name': p.name,
                        'type': p.type,
                        'intent': p.intent,
                        'is_array': p.is_array,
                        'array_spec': p.array_spec
                    }
                    for p in sig.parameters
                ]
            }
        
        output_file = self.validator_dir / "signature_database.json"
        with open(output_file, 'w') as f:
            json.dump(db, f, indent=2, sort_keys=True)
            
        print(f"✓ Saved signature database: {output_file}")
        return output_file
    
    def run(self):
        """Run the complete signature discovery process."""
        print("F77 Automatic Signature Discovery")
        print("=" * 50)
        
        # Discover all signatures
        signatures = self.discover_all_signatures()
        
        if not signatures:
            print("No signatures discovered!")
            return False
            
        print(f"\nDiscovered {len(signatures)} function signatures")
        print(f"Generated {len(self.signature_patterns)} signature patterns")
        
        # Generate and save signature module
        module_content = self.generate_signature_module(signatures)
        module_file = self.validator_dir / "slatec_signatures_module.f90"
        
        # Backup existing file
        if module_file.exists():
            backup_file = module_file.with_suffix('.f90.backup')
            module_file.rename(backup_file)
            print(f"✓ Backed up existing module to {backup_file}")
        
        module_file.write_text(module_content)
        print(f"✓ Generated new signature module: {module_file}")
        
        # Save signature database
        self.save_signature_database(signatures)
        
        # Clean up temporary files
        for temp_file in Path('.').glob('*_signature.pyf'):
            temp_file.unlink()
        
        print("\n🎉 Signature discovery complete!")
        print(f"All {len(signatures)} functions now have proper signature types.")
        
        return True

if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='F77 Signature Discovery Tool')
    parser.add_argument('--function', help='Discover signature for a specific function')
    parser.add_argument('--update', action='store_true', help='Update existing database (default: replace)')
    args = parser.parse_args()
    
    discovery = F77SignatureDiscovery()
    
    if args.function:
        # Single function discovery
        f77_file = discovery.src_dir / f"{args.function.lower()}.f"
        if not f77_file.exists():
            print(f"Error: F77 file not found: {f77_file}")
            exit(1)
            
        print(f"Discovering signature for {args.function}...")
        signature = discovery.extract_signature_with_f2py(f77_file)
        
        if signature:
            # Load existing database
            db_file = discovery.validator_dir / "signature_database.json"
            if db_file.exists() and args.update:
                with open(db_file, 'r') as f:
                    db = json.load(f)
                    # Ensure signature_patterns exists
                    if 'signature_patterns' not in db:
                        db['signature_patterns'] = {}
            else:
                db = {'functions': {}, 'signature_patterns': {}}
                
            # Update database with single function
            func_name = signature.name.upper()
            db['functions'][func_name] = {
                'name': func_name,
                'is_function': signature.is_function,
                'signature_type': signature.signature_type,
                'signature_pattern': signature.signature_pattern,
                'params': [{
                    'name': p.name,
                    'type': p.type,
                    'intent': p.intent,
                    'dimension': [p.array_spec] if p.is_array and p.array_spec else None
                } for p in signature.parameters],
                'return_type': signature.return_type
            }
            
            # Update pattern registry
            if signature.signature_pattern not in db['signature_patterns']:
                db['signature_patterns'][signature.signature_pattern] = signature.signature_type
                
            # Save updated database
            with open(db_file, 'w') as f:
                json.dump(db, f, indent=2)
                
            print(f"✓ Added {func_name} to signature database")
            print(f"  Pattern: {signature.signature_pattern}")
            print(f"  Type: {signature.signature_type}")
            
            # Regenerate Fortran module
            discovery.generate_fortran_signatures(db)
        else:
            print(f"Failed to extract signature for {args.function}")
            exit(1)
    else:
        # Full discovery
        discovery.run()
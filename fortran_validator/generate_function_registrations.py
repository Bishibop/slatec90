#!/usr/bin/env python3
"""
Generate function_registrations.inc from signature database
Connects the signature database to the function registry system
"""

import json
from pathlib import Path

def generate_function_registrations():
    """Generate function registrations from signature database"""
    
    # Read signature database
    db_path = Path("fortran_validator/signature_database.json")
    if not db_path.exists():
        print(f"Error: Signature database not found at {db_path}")
        return False
    
    with open(db_path, 'r') as f:
        db = json.load(f)
    
    functions = db['functions']
    
    # Generate registration calls
    registrations = []
    registrations.append("! Auto-generated function registrations from signature database")
    registrations.append("! DO NOT EDIT MANUALLY")
    registrations.append("")
    
    for func_name, func_info in functions.items():
        name = func_info['name']
        is_function = func_info['is_function']
        signature_pattern = func_info['signature_pattern']
        
        # Map signature patterns to registration calls
        if is_function:
            if signature_pattern == "FUNC_REAL_INTEGER":
                registrations.append(f"call register_real_func_1_int('{name}', {name.lower()}, {name.lower()}_modern)")
            elif signature_pattern == "FUNC_REAL_REAL":
                registrations.append(f"call register_real_func_1_real('{name}', {name.lower()}, {name.lower()}_modern)")
            elif signature_pattern == "FUNC_REAL_REAL_REAL":
                registrations.append(f"call register_real_func_2_real('{name}', {name.lower()}, {name.lower()}_modern)")
            elif signature_pattern == "FUNC_INTEGER_INTEGER":
                registrations.append(f"call register_int_func_1_int('{name}', {name.lower()}, {name.lower()}_modern)")
            elif signature_pattern.startswith("FUNC_LOGICAL"):
                registrations.append(f"call register_logical_func_2_char('{name}', {name.lower()}, {name.lower()}_modern)")
            else:
                registrations.append(f"! TODO: Add registration for {name} ({signature_pattern})")
        else:  # subroutine
            if signature_pattern == "SUB_0_PARAMS":
                registrations.append(f"call register_sub_0_params('{name}', {name.lower()}, {name.lower()}_modern)")
            elif "SUB_" in signature_pattern and "CHAR" in signature_pattern:
                registrations.append(f"call register_sub_1_char_out('{name}', {name.lower()}, {name.lower()}_modern)")
            else:
                registrations.append(f"! TODO: Add registration for {name} ({signature_pattern})")
    
    # Write to file
    output_file = Path("fortran_validator/function_registrations.inc")
    with open(output_file, 'w') as f:
        f.write('\n'.join(registrations))
        f.write('\n')
    
    print(f"Generated {output_file} with {len(functions)} function registrations")
    return True

if __name__ == "__main__":
    generate_function_registrations()
#!/usr/bin/env python3
"""Generate function registration include file"""

# Define functions and their signatures
# Format: (name, signature_type, has_modern)
functions = [
    ("R1MACH", "real_func_1_int", True),
    ("I1MACH", "int_func_1_int", True),
    ("D1MACH", "double_func_1_int", True),
    ("PIMACH", "real_func_0", True),
    ("ZABS", "real_func_2_real", True),
    ("PYTHAG", "real_func_2_real", True),
    ("CSROOT", "real_func_2_real", True),
    ("QWGTC", "real_func_2_real", True),
    ("LSAME", "logical_func_2_char", True),
    ("FDUMP", "sub_0_params", True),
    ("AAAAAA", "sub_1_char_out", True),
    ("CDIV", "sub_6_real", True),
    # Add more as needed
]

# Generate the include file
with open('function_registrations.inc', 'w') as f:
    f.write("! Automatically generated function registrations\n")
    f.write("! DO NOT EDIT MANUALLY\n\n")
    
    # First, declare all the external F77 functions
    f.write("! External F77 function declarations\n")
    for name, sig_type, _ in functions:
        if sig_type == "real_func_1_int":
            f.write(f"real, external :: {name.lower()}\n")
        elif sig_type == "int_func_1_int":
            f.write(f"integer, external :: {name.lower()}\n")
        elif sig_type == "double_func_1_int":
            f.write(f"real(8), external :: {name.lower()}\n")
        elif sig_type == "real_func_0":
            f.write(f"real, external :: {name.lower()}\n")
        elif sig_type == "real_func_2_real":
            f.write(f"real, external :: {name.lower()}\n")
        elif sig_type == "logical_func_2_char":
            f.write(f"logical, external :: {name.lower()}\n")
        elif sig_type == "sub_0_params":
            f.write(f"external :: {name.lower()}\n")
        elif sig_type == "sub_1_char_out":
            f.write(f"external :: {name.lower()}\n")
        elif sig_type == "sub_6_real":
            f.write(f"external :: {name.lower()}\n")
    
    f.write("\n! Modern function imports\n")
    f.write("include 'functions.inc'\n")
    
    f.write("\n! Register all functions\n")
    for name, sig_type, has_modern in functions:
        if sig_type == "real_func_1_int":
            f.write(f"call register_real_func_1_int('{name}', {name.lower()}")
            if has_modern:
                f.write(f", {name.lower()}_modern")
            f.write(")\n")
        elif sig_type == "int_func_1_int":
            f.write(f"call register_int_func_1_int('{name}', {name.lower()}")
            if has_modern:
                f.write(f", {name.lower()}_modern")
            f.write(")\n")
        elif sig_type == "real_func_2_real":
            f.write(f"call register_real_func_2_real('{name}', {name.lower()}")
            if has_modern:
                f.write(f", {name.lower()}_modern")
            f.write(")\n")
        elif sig_type == "logical_func_2_char":
            f.write(f"call register_logical_func_2_char('{name}', {name.lower()}")
            if has_modern:
                f.write(f", {name.lower()}_modern")
            f.write(")\n")
        elif sig_type == "sub_0_params":
            f.write(f"call register_sub_0_params('{name}', {name.lower()}")
            if has_modern:
                f.write(f", {name.lower()}_modern")
            f.write(")\n")
        elif sig_type == "sub_1_char_out":
            f.write(f"call register_sub_1_char_out('{name}', {name.lower()}")
            if has_modern:
                f.write(f", {name.lower()}_modern")
            f.write(")\n")

print("Generated function_registrations.inc")
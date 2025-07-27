#!/usr/bin/env python3
"""
Generate Fortran metadata modules from Python SLATEC metadata registry
This creates a bridge between Python metadata and Fortran validation
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from slatec_metadata import SLATEC_FUNCTIONS

def generate_signature_module():
    """Generate Fortran module with function signatures"""
    code = """module slatec_signatures_module
    implicit none
    
    ! Public parameters
    public :: SIG_UNKNOWN, SIG_REAL_FUNC_1_INT, SIG_REAL_FUNC_1_REAL
    public :: SIG_REAL_FUNC_2_REAL, SIG_INT_FUNC_1_INT, SIG_DOUBLE_FUNC_1_INT
    public :: SIG_LOGICAL_FUNC_2_CHAR, SIG_SUB_0_PARAMS, SIG_SUB_1_CHAR_OUT
    public :: SIG_SUB_6_REAL, SIG_REAL_FUNC_INT_REAL_ARRAY
    public :: function_info, get_function_info, get_signature_type
    public :: TYPE_INTEGER, TYPE_REAL, TYPE_CHARACTER, TYPE_DOUBLE, TYPE_LOGICAL
    public :: INTENT_IN, INTENT_OUT, INTENT_INOUT
    
    ! Signature type constants
    integer, parameter :: SIG_UNKNOWN = 0
    integer, parameter :: SIG_REAL_FUNC_1_INT = 1
    integer, parameter :: SIG_REAL_FUNC_1_REAL = 2
    integer, parameter :: SIG_REAL_FUNC_2_REAL = 3
    integer, parameter :: SIG_INT_FUNC_1_INT = 4
    integer, parameter :: SIG_DOUBLE_FUNC_1_INT = 5
    integer, parameter :: SIG_LOGICAL_FUNC_2_CHAR = 6
    integer, parameter :: SIG_SUB_0_PARAMS = 7
    integer, parameter :: SIG_SUB_1_CHAR_OUT = 8
    integer, parameter :: SIG_SUB_6_REAL = 9
    integer, parameter :: SIG_REAL_FUNC_INT_REAL_ARRAY = 10
    
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
    
    ! Number of registered functions
    integer, parameter :: NUM_FUNCTIONS = """ + str(len(SLATEC_FUNCTIONS)) + """
    
    ! Function registry
    type(function_info), parameter :: FUNCTION_REGISTRY(NUM_FUNCTIONS) = [ &
"""
    
    # Generate function entries
    entries = []
    for i, (name, info) in enumerate(SLATEC_FUNCTIONS.items()):
        # Determine signature type
        sig_type = determine_signature_type(name, info)
        
        # Build parameter arrays
        param_types = [0] * 10
        param_intents = [0] * 10
        for j, param in enumerate(info['params'][:10]):
            param_types[j] = get_type_constant(param['type'])
            param_intents[j] = get_intent_constant(param.get('intent', 'in'))
        
        # Return type
        return_type = 0
        if info['type'] == 'function':
            return_type = get_type_constant(info['returns'])
        
        entry = f"""        function_info( &
            name='{name.upper():<20}', &
            signature_type={sig_type}, &
            is_function={'.true.' if info['type'] == 'function' else '.false.'}, &
            num_params={len(info['params'])}, &
            param_types=[{','.join(str(x) for x in param_types)}], &
            param_intents=[{','.join(str(x) for x in param_intents)}], &
            return_type={return_type} &
        )"""
        entries.append(entry)
    
    code += ', &\n'.join(entries)
    code += """ &
    ]
    
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

end module slatec_signatures_module
"""
    
    return code

def determine_signature_type(name, info):
    """Determine the signature type constant for a function"""
    if info['type'] == 'function':
        params = info['params']
        returns = info['returns']
        
        # Check specific patterns
        if len(params) == 1:
            if params[0]['type'] == 'integer' and returns == 'real':
                return 1  # SIG_REAL_FUNC_1_INT
            elif params[0]['type'] == 'real' and returns == 'real':
                return 2  # SIG_REAL_FUNC_1_REAL
            elif params[0]['type'] == 'integer' and returns == 'integer':
                return 4  # SIG_INT_FUNC_1_INT
            elif params[0]['type'] == 'integer' and returns == 'double':
                return 5  # SIG_DOUBLE_FUNC_1_INT
        elif len(params) == 2:
            if all(p['type'] == 'real' for p in params) and returns == 'real':
                return 3  # SIG_REAL_FUNC_2_REAL
            elif all(p['type'] == 'character' for p in params) and returns == 'logical':
                return 6  # SIG_LOGICAL_FUNC_2_CHAR
            elif params[0]['type'] == 'integer' and params[1]['type'] == 'real' and returns == 'real':
                # Check if second param is array (for ENORM-like functions)
                if 'dimension' in params[1] or name.upper() == 'ENORM':
                    return 10  # SIG_REAL_FUNC_INT_REAL_ARRAY
    else:  # subroutine
        params = info['params']
        if len(params) == 0:
            return 7  # SIG_SUB_0_PARAMS
        elif len(params) == 1 and params[0]['type'] == 'character' and params[0]['intent'] == 'out':
            return 8  # SIG_SUB_1_CHAR_OUT
        elif len(params) == 6 and all(p['type'] == 'real' for p in params):
            return 9  # SIG_SUB_6_REAL
    
    return 0  # SIG_UNKNOWN

def get_type_constant(type_str):
    """Convert type string to constant"""
    mapping = {
        'integer': 1,
        'real': 2,
        'character': 3,
        'double': 4,
        'logical': 5
    }
    return mapping.get(type_str, 0)

def get_intent_constant(intent_str):
    """Convert intent string to constant"""
    mapping = {
        'in': 1,
        'out': 2,
        'inout': 3
    }
    return mapping.get(intent_str, 1)

def generate_dispatcher_module():
    """Generate module for dynamic dispatch"""
    code = """module function_dispatcher_module
    use slatec_signatures_module
    use validation_reporting_module
    use validator_module
    implicit none
    
contains

    subroutine dispatch_validation(func_name, int_params, num_int_params, &
                                  real_params, num_real_params, &
                                  char_params, num_char_params, &
                                  real_array, array_size)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: int_params(:), num_int_params
        real, intent(in) :: real_params(:)
        integer, intent(in) :: num_real_params
        character(len=*), intent(in) :: char_params(:)
        integer, intent(in) :: num_char_params
        real, allocatable, intent(in) :: real_array(:)
        integer, intent(in) :: array_size
        
        type(function_info) :: info
        
        ! Get function signature
        info = get_function_info(func_name)
        
        ! Dispatch based on signature type
        select case(info%signature_type)
            case(SIG_REAL_FUNC_1_INT)
                if (num_int_params >= 1) then
                    call validate_real_function_1_int(func_name, int_params(1))
                else
                    call report_failed_test_character("Missing integer parameter", "")
                end if
                
            case(SIG_REAL_FUNC_1_REAL)
                if (num_real_params >= 1) then
                    call validate_real_function_1_real(func_name, real_params(1))
                else
                    call report_failed_test_character("Missing real parameter", "")
                end if
                
            case(SIG_REAL_FUNC_2_REAL)
                if (num_real_params >= 2) then
                    call validate_real_function_2_real(func_name, real_params(1), real_params(2))
                else
                    call report_failed_test_character("Missing real parameters", "")
                end if
                
            case(SIG_INT_FUNC_1_INT)
                if (num_int_params >= 1) then
                    call validate_integer_function_1_int(func_name, int_params(1))
                else
                    call report_failed_test_character("Missing integer parameter", "")
                end if
                
            case(SIG_DOUBLE_FUNC_1_INT)
                if (num_int_params >= 1) then
                    call validate_double_function_1_int(func_name, int_params(1))
                else
                    call report_failed_test_character("Missing integer parameter", "")
                end if
                
            case(SIG_LOGICAL_FUNC_2_CHAR)
                if (num_char_params >= 2) then
                    call validate_logical_function_2_char(func_name, &
                        char_params(1)(1:1), char_params(2)(1:1))
                else
                    call report_failed_test_character("Missing character parameters", "")
                end if
                
            case(SIG_SUB_0_PARAMS)
                call validate_subroutine_0_params(func_name)
                
            case(SIG_SUB_1_CHAR_OUT)
                call validate_subroutine_1_char_out(func_name, 16)  ! Assume 16 chars
                
            case(SIG_SUB_6_REAL)
                if (num_real_params >= 6) then
                    call validate_subroutine_6_real(func_name, &
                        real_params(1), real_params(2), real_params(3), &
                        real_params(4), real_params(5), real_params(6))
                else
                    call report_failed_test_character("Missing real parameters", "")
                end if
                
            case(SIG_REAL_FUNC_INT_REAL_ARRAY)
                if (num_int_params >= 1 .and. array_size > 0) then
                    call validate_real_function_int_real_array(func_name, &
                        int_params(1), real_array)
                else
                    call report_failed_test_character("Missing integer parameter or array", "")
                end if
                
            case default
                call report_failed_test_character( &
                    "Unknown function signature for " // trim(func_name), "")
        end select
        
    end subroutine dispatch_validation

end module function_dispatcher_module
"""
    return code

def main():
    """Generate all metadata files"""
    # Generate signature module
    sig_module = generate_signature_module()
    with open('slatec_signatures_module.f90', 'w') as f:
        f.write(sig_module)
    print("Generated slatec_signatures_module.f90")
    
    # Generate dispatcher module
    disp_module = generate_dispatcher_module()
    with open('function_dispatcher_module.f90', 'w') as f:
        f.write(disp_module)
    print("Generated function_dispatcher_module.f90")

if __name__ == '__main__':
    main()
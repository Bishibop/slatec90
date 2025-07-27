"""
SLATEC Function Metadata Registry
Contains function signatures for all SLATEC functions to enable generic validation
"""

SLATEC_FUNCTIONS = {
    'AAAAAA': {
        'type': 'subroutine',
        'params': [
            {'name': 'VER', 'type': 'character', 'intent': 'out', 'size': 16}
        ],
        'returns': None,
        'description': 'Returns SLATEC version string'
    },
    
    'CDIV': {
        'type': 'subroutine',
        'params': [
            {'name': 'AR', 'type': 'real', 'intent': 'in'},
            {'name': 'AI', 'type': 'real', 'intent': 'in'},
            {'name': 'BR', 'type': 'real', 'intent': 'in'},
            {'name': 'BI', 'type': 'real', 'intent': 'in'},
            {'name': 'CR', 'type': 'real', 'intent': 'out'},
            {'name': 'CI', 'type': 'real', 'intent': 'out'}
        ],
        'returns': None,
        'description': 'Complex division (AR+i*AI)/(BR+i*BI) = CR+i*CI'
    },
    
    'D1MACH': {
        'type': 'function',
        'params': [
            {'name': 'I', 'type': 'integer', 'intent': 'in'}
        ],
        'returns': 'double',
        'description': 'Double precision machine constants'
    },
    
    'FDUMP': {
        'type': 'subroutine',
        'params': [],
        'returns': None,
        'description': 'Dumps error message buffer'
    },
    
    'I1MACH': {
        'type': 'function',
        'params': [
            {'name': 'I', 'type': 'integer', 'intent': 'in'}
        ],
        'returns': 'integer',
        'description': 'Integer machine constants'
    },
    
    'LSAME': {
        'type': 'function',
        'params': [
            {'name': 'CA', 'type': 'character', 'intent': 'in', 'size': 1},
            {'name': 'CB', 'type': 'character', 'intent': 'in', 'size': 1}
        ],
        'returns': 'logical',
        'description': 'Case-insensitive character comparison'
    },
    
    'PIMACH': {
        'type': 'function',
        'params': [
            {'name': 'DUM', 'type': 'real', 'intent': 'in'}
        ],
        'returns': 'real',
        'description': 'Returns the value of pi'
    },
    
    'PYTHAG': {
        'type': 'function',
        'params': [
            {'name': 'A', 'type': 'real', 'intent': 'in'},
            {'name': 'B', 'type': 'real', 'intent': 'in'}
        ],
        'returns': 'real',
        'description': 'Computes sqrt(A**2 + B**2) without overflow/underflow'
    },
    
    'R1MACH': {
        'type': 'function',
        'params': [
            {'name': 'I', 'type': 'integer', 'intent': 'in'}
        ],
        'returns': 'real',
        'description': 'Single precision machine constants'
    },
    
    # Example: Adding a new function requires only updating metadata
    'HYPOT': {
        'type': 'function',
        'params': [
            {'name': 'X', 'type': 'real', 'intent': 'in'},
            {'name': 'Y', 'type': 'real', 'intent': 'in'}
        ],
        'returns': 'real',
        'description': 'Computes sqrt(X**2 + Y**2) (example function)'
    }
,
    
    'CSROOT': {
        'type': 'subroutine',
        'params': [
            {'name': 'XR', 'type': 'real', 'intent': 'in'},
            {'name': 'XI', 'type': 'real', 'intent': 'in'},
            {'name': 'YR', 'type': 'real', 'intent': 'out'},
            {'name': 'YI', 'type': 'real', 'intent': 'out'}
        ],
        'returns': None,
        'description': 'Compute the complex square root of a complex number.'
    },
    
    'SVOUT': {
        'type': 'subroutine',
        'params': [
            {'name': 'N', 'type': 'real', 'intent': 'in', 'dimension': 'SVOUT-S, DVOUT-D'},
            {'name': 'SX', 'type': 'real', 'intent': 'in', 'dimension': '*'},
            {'name': 'IFMT', 'type': 'character', 'intent': 'in', 'dimension': '*'},
            {'name': 'IDIGIT', 'type': 'real', 'intent': 'in'},
        ],
        'returns': None,
        'description': 'Subsidiary to SPLP'
    },
    
    'ZEXP': {
        'type': 'subroutine',
        'params': [
            {'name': 'AR', 'type': 'double', 'intent': 'in'},
            {'name': 'AI', 'type': 'double', 'intent': 'in'},
            {'name': 'BR', 'type': 'double', 'intent': 'out'},
            {'name': 'BI', 'type': 'double', 'intent': 'out'},
        ],
        'returns': None,
        'description': 'Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and'
    },
    
    'QWGTC': {
        'type': 'function',
        'params': [
            {'name': 'X', 'type': 'real', 'intent': 'in'},
            {'name': 'C', 'type': 'real', 'intent': 'in'},
            {'name': 'P2', 'type': 'real', 'intent': 'in'},
            {'name': 'P3', 'type': 'real', 'intent': 'in'},
            {'name': 'P4', 'type': 'real', 'intent': 'in'},
            {'name': 'KP', 'type': 'integer', 'intent': 'in'},
        ],
        'returns': 'real',
        'description': 'Cauchy weight function for quadrature'
    },
    
    'ZABS': {
        'type': 'function',
        'params': [
            {'name': 'ZR', 'type': 'double', 'intent': 'in'},
            {'name': 'ZI', 'type': 'double', 'intent': 'in'},
        ],
        'returns': 'double',
        'description': 'Computes absolute value of complex number without overflow'
    },
    
    'ENORM': {
        'type': 'function',
        'params': [
            {'name': 'N', 'type': 'integer', 'intent': 'in', 'dimension': 'ENORM-S, DENORM-D'},
            {'name': 'X', 'type': 'real', 'intent': 'in', 'dimension': '*'},
        ],
        'returns': 'real',
        'description': 'Subsidiary to SNLS1, SNLS1E, SNSQ and SNSQE'
    },
    
    'POLCOF': {
        'type': 'subroutine',
        'params': [
            {'name': 'XX', 'type': 'real', 'intent': 'in'},
            {'name': 'N', 'type': 'real', 'intent': 'in', 'dimension': 'POLCOF-S, DPOLCF-D'},
            {'name': 'X', 'type': 'real', 'intent': 'in', 'dimension': '*'},
            {'name': 'C', 'type': 'real', 'intent': 'in', 'dimension': '*'},
            {'name': 'D', 'type': 'real', 'intent': 'inout', 'dimension': '1'},
            {'name': 'WORK', 'type': 'real', 'intent': 'inout', 'dimension': '*'},
        ],
        'returns': None,
        'description': 'Compute the coefficients of the polynomial fit (including'
    },
    
    'XRED': {
        'type': 'subroutine',
        'params': [
            {'name': 'X', 'type': 'real', 'intent': 'in'},
            {'name': 'IX', 'type': 'integer', 'intent': 'in'},
            {'name': 'IERROR', 'type': 'real', 'intent': 'out'},
        ],
        'returns': None,
        'description': 'To provide single-precision floating-point arithmetic'
    },
    
    'QFORM': {
        'type': 'subroutine',
        'params': [
            {'name': 'M', 'type': 'integer', 'intent': 'in'},
            {'name': 'N', 'type': 'integer', 'intent': 'in'},
            {'name': 'Q', 'type': 'real', 'intent': 'inout', 'dimension': 'LDQ,*'},
            {'name': 'LDQ', 'type': 'integer', 'intent': 'in'},
            {'name': 'WA', 'type': 'real', 'intent': 'inout', 'dimension': 'M'}
        ],
        'returns': None,
        'description': 'Accumulate Q from QR factorization'
    }}

def get_function_signature(func_name):
    """Get function signature metadata"""
    return SLATEC_FUNCTIONS.get(func_name.upper())

def get_param_count(func_name):
    """Get number of parameters for a function"""
    sig = get_function_signature(func_name)
    return len(sig['params']) if sig else 0

def get_param_types(func_name):
    """Get parameter types as a list"""
    sig = get_function_signature(func_name)
    if not sig:
        return []
    return [p['type'] for p in sig['params']]

def is_function(func_name):
    """Check if this is a function (vs subroutine)"""
    sig = get_function_signature(func_name)
    return sig and sig['type'] == 'function'

def get_return_type(func_name):
    """Get return type for functions"""
    sig = get_function_signature(func_name)
    return sig.get('returns') if sig else None
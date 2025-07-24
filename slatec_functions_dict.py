#!/usr/bin/env python3
"""
SLATEC functions dictionary - extracted function names and descriptions.
This file can be imported as a Python module to access the function data.
"""

import json
from pathlib import Path

# Load the JSON data
json_file = Path(__file__).parent / 'slatec_functions.json'
with open(json_file, 'r') as f:
    SLATEC_FUNCTIONS = json.load(f)

# Create a simplified dictionary with just names and purposes
SLATEC_PURPOSES = {name: info['purpose'] for name, info in SLATEC_FUNCTIONS.items()}

def get_function_info(function_name):
    """Get information about a specific SLATEC function."""
    name = function_name.upper()
    if name in SLATEC_FUNCTIONS:
        return SLATEC_FUNCTIONS[name]
    return None

def search_functions(search_term):
    """Search for functions by name or purpose description."""
    search_term = search_term.lower()
    results = {}
    
    for name, info in SLATEC_FUNCTIONS.items():
        if search_term in name.lower() or search_term in info['purpose'].lower():
            results[name] = info
    
    return results

def get_functions_by_category(category):
    """Get functions that match a specific category keyword."""
    category = category.lower()
    results = {}
    
    category_keywords = {
        'bessel': ['bessel', 'besi', 'besj', 'besk', 'besy'],
        'integration': ['integrat', 'quadrature', 'integral'],
        'interpolation': ['interpolat', 'spline'],
        'differential': ['differential', 'ode', 'equation'],
        'linear': ['linear', 'matrix', 'solve'],
        'special': ['special function', 'gamma', 'error'],
    }
    
    keywords = category_keywords.get(category, [category])
    
    for name, info in SLATEC_FUNCTIONS.items():
        purpose_lower = info['purpose'].lower()
        if any(keyword in purpose_lower for keyword in keywords):
            results[name] = info
    
    return results

if __name__ == '__main__':
    # Example usage
    print(f"Total SLATEC functions: {len(SLATEC_FUNCTIONS)}")
    print("\nExample - Bessel functions:")
    bessel_funcs = get_functions_by_category('bessel')
    for name, info in list(bessel_funcs.items())[:5]:
        print(f"  {name}: {info['purpose']}")
    
    print("\nExample - Integration functions:")
    integration_funcs = get_functions_by_category('integration')
    for name, info in list(integration_funcs.items())[:5]:
        print(f"  {name}: {info['purpose']}")
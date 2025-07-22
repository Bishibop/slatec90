#!/usr/bin/env python3
"""
Test o3-mini directly
"""

from llm_config import call_llm

prompt = """Write a simple F77-compatible wrapper function for a modern Fortran function.

The modern function is:
function denorm(n, x) result(norm)
  integer, intent(in) :: n
  real(dp), intent(in) :: x(:)
  real(dp) :: norm

The F77 wrapper should:
1. Have signature: double precision function denorm(n, x)
2. Take x(*) as assumed-size array
3. Call the modern function with x(1:n)

Write ONLY the wrapper function code, no module or explanations."""

print("Testing o3-mini...")
response = call_llm(prompt, model="o3-mini", reasoning_effort="low")

print("\nResponse:")
print(response)
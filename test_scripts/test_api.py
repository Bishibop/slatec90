#!/usr/bin/env python3
"""
Test OpenAI API connection
"""

from llm_config import call_llm

try:
    # Simple test using our config
    response = call_llm("Say hello", model="gpt-3.5-turbo", max_tokens=10)
    
    print("API Response:", response)
    print("✓ API connection works!")
    
except Exception as e:
    print(f"✗ API Error: {e}")
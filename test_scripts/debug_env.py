#!/usr/bin/env python3
"""
Debug environment loading
"""

import os

print("1. Checking raw environment:")
print(f"   OPENAI_API_KEY from env: {os.getenv('OPENAI_API_KEY', 'NOT SET')}")

print("\n2. Trying to load .env file:")
try:
    from dotenv import load_dotenv
    print("   dotenv imported successfully")
    
    # Load .env
    result = load_dotenv()
    print(f"   load_dotenv() returned: {result}")
    
    # Check if file exists
    if os.path.exists('.env'):
        print("   .env file exists")
        with open('.env', 'r') as f:
            lines = f.readlines()
            print(f"   .env has {len(lines)} lines")
            for i, line in enumerate(lines):
                if 'OPENAI_API_KEY' in line:
                    print(f"   Line {i+1} contains OPENAI_API_KEY")
    else:
        print("   .env file NOT found")
        
except ImportError as e:
    print(f"   Failed to import dotenv: {e}")

print("\n3. After loading .env:")
print(f"   OPENAI_API_KEY: {os.getenv('OPENAI_API_KEY', 'NOT SET')[:20] if os.getenv('OPENAI_API_KEY') else 'NOT SET'}...")

print("\n4. Current working directory:")
print(f"   {os.getcwd()}")

print("\n5. Files in current directory:")
for f in ['.env', '.env.example', 'llm_config.py']:
    print(f"   {f}: {'exists' if os.path.exists(f) else 'NOT FOUND'}")
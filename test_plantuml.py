#!/usr/bin/env python3
"""
Test PlantUML encoding and web service manually
"""
import base64
import zlib
import requests

def encode_plantuml_web(plantuml_code: str) -> str:
    """Encode PlantUML code for web service using proper deflate encoding"""
    # PlantUML uses deflate compression (not zlib which adds headers)
    compress_obj = zlib.compressobj(wbits=-15)  # -15 = raw deflate, no headers
    compressed = compress_obj.compress(plantuml_code.encode('utf-8'))
    compressed += compress_obj.flush()
    
    # PlantUML uses a custom base64-like encoding
    encoded = base64.b64encode(compressed).decode('ascii')
    # Replace characters for URL safety
    encoded = encoded.replace('+', '-').replace('/', '_')
    # Remove padding
    encoded = encoded.rstrip('=')
    return encoded

# Test with a simple PlantUML diagram
test_code = """
@startuml
title Test Diagram

component "Source" as src #4CAF50
component "Process" as proc #FF9800
storage "Output" as out #9C27B0

src --> proc : data
proc --> out : result

@enduml
"""

print("PlantUML code:")
print(test_code)
print("\n" + "="*50)

encoded = encode_plantuml_web(test_code)
print(f"Encoded: {encoded}")
print(f"Encoded length: {len(encoded)}")

url = f"http://www.plantuml.com/plantuml/png/{encoded}"
print(f"\nURL: {url}")

print("\n" + "="*50)
print("Making request...")

try:
    response = requests.get(url, timeout=30)
    print(f"Status code: {response.status_code}")
    print(f"Headers: {dict(response.headers)}")
    print(f"Content length: {len(response.content)}")
    
    if response.content.startswith(b'\x89PNG'):
        print("✓ Valid PNG received!")
        # Save for inspection
        with open('/tmp/test_plantuml.png', 'wb') as f:
            f.write(response.content)
        print("Saved to /tmp/test_plantuml.png")
    else:
        print("✗ Not a PNG - got error response")
        print("Content (first 500 chars):")
        try:
            print(response.content.decode('utf-8')[:500])
        except:
            print(response.content[:500])
            
except Exception as e:
    print(f"Request failed: {e}")
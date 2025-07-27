#!/usr/bin/env python3
"""
Test Mermaid rendering manually
"""
import base64
import requests
from visualization.mermaid_renderer import MermaidTemplateGenerator

# Test Mermaid encoding and rendering
generator = MermaidTemplateGenerator()

# Test queue diagram
print("Testing Mermaid Queue Diagram:")
print("="*50)
data = {'function_name': 'PYTHAG'}
mermaid_code = generator.generate_stage_diagram('queue', data)
print("Mermaid code:")
print(mermaid_code)

# Simple base64 encoding for Mermaid.ink
encoded = base64.b64encode(mermaid_code.encode('utf-8')).decode('ascii')
url = f"https://mermaid.ink/img/{encoded}"

print(f"\nURL: {url}")

try:
    print("Making request...")
    response = requests.get(url, timeout=30)
    print(f"Status: {response.status_code}")
    print(f"Content-Type: {response.headers.get('content-type')}")
    print(f"Content length: {len(response.content)}")
    
    if response.status_code == 200:
        if response.content.startswith(b'\x89PNG'):
            print("✓ Valid PNG received!")
            with open('/tmp/mermaid_queue.png', 'wb') as f:
                f.write(response.content)
            print("Saved to /tmp/mermaid_queue.png")
        elif response.content.startswith(b'\xff\xd8\xff'):
            print("✓ Valid JPEG received!")
            with open('/tmp/mermaid_queue.jpg', 'wb') as f:
                f.write(response.content)
            print("Saved to /tmp/mermaid_queue.jpg")
        elif response.content.startswith(b'<svg'):
            print("✓ Valid SVG received!")
            with open('/tmp/mermaid_queue.svg', 'w') as f:
                f.write(response.content.decode('utf-8'))
            print("Saved to /tmp/mermaid_queue.svg")
        else:
            print("✗ Unknown format")
            print(f"First 100 bytes: {response.content[:100]}")
    else:
        print(f"✗ Request failed: {response.status_code}")
        print(response.text[:200])
        
except Exception as e:
    print(f"Request failed: {e}")

print("\n" + "="*50)
print("Testing Test Generation diagram...")

# Test test generation
data2 = {
    'function_name': 'PYTHAG', 
    'progress': 50, 
    'completed': False, 
    'test_count': '130'
}
mermaid_code2 = generator.generate_stage_diagram('test generation', data2)
encoded2 = base64.b64encode(mermaid_code2.encode('utf-8')).decode('ascii')
url2 = f"https://mermaid.ink/img/{encoded2}"

print(f"Test Gen URL: {url2}")

try:
    response2 = requests.get(url2, timeout=30)
    if response2.status_code == 200:
        if response2.content.startswith(b'\x89PNG'):
            print("✓ Test Gen PNG is valid!")
            with open('/tmp/mermaid_testgen.png', 'wb') as f:
                f.write(response2.content)
            print("Saved to /tmp/mermaid_testgen.png")
        elif response2.content.startswith(b'\xff\xd8\xff'):
            print("✓ Test Gen JPEG is valid!")
            with open('/tmp/mermaid_testgen.jpg', 'wb') as f:
                f.write(response2.content)
            print("Saved to /tmp/mermaid_testgen.jpg")
        elif response2.content.startswith(b'<svg'):
            print("✓ Test Gen SVG is valid!")
            with open('/tmp/mermaid_testgen.svg', 'w') as f:
                f.write(response2.content.decode('utf-8'))
            print("Saved to /tmp/mermaid_testgen.svg")
        else:
            print("✗ Test Gen failed")
            print(f"Content: {response2.content[:200]}")
    else:
        print(f"✗ Test Gen failed: {response2.status_code}")
except Exception as e:
    print(f"Test Gen request failed: {e}")
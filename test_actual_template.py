#!/usr/bin/env python3
"""
Test the actual PlantUML templates we're using in the demo
"""
import base64
import zlib
import requests
from visualization.plantuml_renderer import PlantUMLTemplateGenerator

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

# Test our actual templates
generator = PlantUMLTemplateGenerator()

# Test queue diagram
print("Testing Queue Diagram:")
print("="*50)
data = {'function_name': 'PYTHAG'}
queue_code = generator.generate_stage_diagram('queue', data)
print("PlantUML code:")
print(queue_code)

encoded = encode_plantuml_web(queue_code)
url = f"http://www.plantuml.com/plantuml/png/{encoded}"
print(f"\nURL: {url}")

try:
    response = requests.get(url, timeout=30)
    print(f"Status: {response.status_code}")
    
    if response.content.startswith(b'\x89PNG'):
        print("✓ Queue diagram: Valid PNG!")
        with open('/tmp/queue_diagram.png', 'wb') as f:
            f.write(response.content)
    else:
        print("✗ Queue diagram failed")
        try:
            print("Error:", response.content.decode('utf-8')[:200])
        except:
            print("Error:", response.content[:200])
            
except Exception as e:
    print(f"Request failed: {e}")

print("\n" + "="*50)
print("Testing Test Generation Diagram:")

# Test test generation diagram
data = {
    'function_name': 'PYTHAG', 
    'progress': 50, 
    'completed': False, 
    'test_count': '130'
}
test_gen_code = generator.generate_stage_diagram('test generation', data)
print("PlantUML code:")
print(test_gen_code)

encoded = encode_plantuml_web(test_gen_code)
url = f"http://www.plantuml.com/plantuml/png/{encoded}"

try:
    response = requests.get(url, timeout=30)
    print(f"Status: {response.status_code}")
    
    if response.content.startswith(b'\x89PNG'):
        print("✓ Test generation diagram: Valid PNG!")
        with open('/tmp/testgen_diagram.png', 'wb') as f:
            f.write(response.content)
    else:
        print("✗ Test generation diagram failed")
        try:
            print("Error:", response.content.decode('utf-8')[:500])
        except:
            print("Error:", response.content[:500])
            
except Exception as e:
    print(f"Request failed: {e}")
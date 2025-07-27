#!/usr/bin/env python3
"""
Test the exact diagram that's failing in the demo
"""
import base64
import zlib
import requests
from visualization.plantuml_renderer import PlantUMLTemplateGenerator

def encode_plantuml_web(plantuml_code: str) -> str:
    """Encode PlantUML code for web service using proper PlantUML encoding"""
    # Use the plantuml library's encoding if available
    try:
        import plantuml
        # Use the plantuml library's built-in encoding
        plantuml_obj = plantuml.PlantUML(url='http://www.plantuml.com/plantuml/img/')
        # Extract the encoding method
        return plantuml_obj.get_url(plantuml_code).split('/')[-1]
    except (ImportError, AttributeError):
        # Fallback: Add ~1 header as suggested by the error message
        # PlantUML expects a specific format with ~1 header for Huffman encoding
        data_with_header = "~1" + plantuml_code
        
        # Use zlib compression
        compressed = zlib.compress(data_with_header.encode('utf-8'))
        
        # PlantUML uses a custom base64-like encoding
        encoded = base64.b64encode(compressed).decode('ascii')
        # Replace characters for URL safety
        encoded = encoded.replace('+', '-').replace('/', '_')
        # Remove padding
        encoded = encoded.rstrip('=')
        return encoded

# Generate the exact same diagram that's showing in the demo
generator = PlantUMLTemplateGenerator()

# This is the exact data from the demo's first stage
data = {
    'function_name': 'PYTHAG', 
    'completed': False, 
    'pass_rate': 0.0, 
    'iteration': 0, 
    'progress': 0, 
    'test_count': '130'
}

# Generate queue diagram (first stage)
print("Generating EXACT queue diagram from demo...")
print("="*60)
plantuml_code = generator.generate_stage_diagram('queue', data)

print("PlantUML Code:")
print(plantuml_code)
print("\n" + "="*60)

# Encode and get URL
encoded = encode_plantuml_web(plantuml_code)
url = f"http://www.plantuml.com/plantuml/png/{encoded}"

print(f"Encoded: {encoded}")
print(f"URL: {url}")
print("\n" + "="*60)

# Test the web service
try:
    print("Testing web service...")
    response = requests.get(url, timeout=30)
    print(f"Status: {response.status_code}")
    print(f"Content-Type: {response.headers.get('content-type')}")
    print(f"Content length: {len(response.content)}")
    
    if response.content.startswith(b'\x89PNG'):
        print("✓ Valid PNG received!")
        
        # Save the exact PNG that should be displaying
        with open('/tmp/demo_queue_diagram.png', 'wb') as f:
            f.write(response.content)
        print("Saved to /tmp/demo_queue_diagram.png")
        
        # Also test if we can load it into a QPixmap
        try:
            from PyQt6.QtWidgets import QApplication
            from PyQt6.QtGui import QPixmap
            import sys
            
            app = QApplication(sys.argv)
            pixmap = QPixmap()
            if pixmap.loadFromData(response.content):
                print(f"✓ QPixmap loaded successfully: {pixmap.size()}")
                print(f"QPixmap isNull: {pixmap.isNull()}")
            else:
                print("✗ Failed to load into QPixmap")
        except Exception as e:
            print(f"QPixmap test failed: {e}")
            
    else:
        print("✗ Not a valid PNG - error response:")
        try:
            error_text = response.content.decode('utf-8')
            print(error_text)
        except:
            print(f"Binary response: {response.content[:200]}")
            
except Exception as e:
    print(f"Request failed: {e}")

print("\n" + "="*60)
print("Now testing Test Generation diagram...")

# Test the second diagram that appears
data2 = {
    'function_name': 'PYTHAG', 
    'completed': False, 
    'pass_rate': 0.0, 
    'iteration': 0, 
    'progress': 25, 
    'test_count': '130'
}

plantuml_code2 = generator.generate_stage_diagram('test generation', data2)
encoded2 = encode_plantuml_web(plantuml_code2)
url2 = f"http://www.plantuml.com/plantuml/png/{encoded2}"

print(f"Test Gen URL: {url2}")

try:
    response2 = requests.get(url2, timeout=30)
    if response2.content.startswith(b'\x89PNG'):
        print("✓ Test Generation PNG is valid!")
        with open('/tmp/demo_testgen_diagram.png', 'wb') as f:
            f.write(response2.content)
        print("Saved to /tmp/demo_testgen_diagram.png")
    else:
        print("✗ Test Generation PNG failed")
        try:
            print("Error:", response2.content.decode('utf-8')[:200])
        except:
            print("Binary error:", response2.content[:200])
except Exception as e:
    print(f"Test Gen request failed: {e}")
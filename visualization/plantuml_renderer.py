"""
PlantUML Renderer for SLATEC System Architecture Diagrams
Provides professional diagram generation with automatic layout
"""
import base64
import hashlib
import os
import subprocess
import tempfile
import zlib
from pathlib import Path
from typing import Dict, Any, Optional
import requests
from PyQt6.QtCore import QThread, pyqtSignal, QObject
from PyQt6.QtGui import QPixmap


class PlantUMLRenderer(QObject):
    """PlantUML diagram renderer with caching and fallback support"""
    
    def __init__(self):
        super().__init__()
        self.cache_dir = Path(tempfile.gettempdir()) / "slatec_plantuml_cache"
        self.cache_dir.mkdir(exist_ok=True)
        self.plantuml_jar_path = self._find_plantuml_jar()
        self.use_web_service = True  # Fallback to web service if JAR not found
        
    def _find_plantuml_jar(self) -> Optional[Path]:
        """Try to find local PlantUML JAR file"""
        possible_paths = [
            Path.home() / "plantuml.jar",
            Path("/usr/local/bin/plantuml.jar"),
            Path("plantuml.jar"),
        ]
        
        for path in possible_paths:
            if path.exists():
                return path
        return None
    
    def _get_cache_path(self, plantuml_code: str) -> Path:
        """Generate cache file path for given PlantUML code"""
        code_hash = hashlib.md5(plantuml_code.encode()).hexdigest()
        return self.cache_dir / f"{code_hash}.png"
    
    def _encode_plantuml_web(self, plantuml_code: str) -> str:
        """Encode PlantUML code for web service using proper PlantUML encoding"""
        import zlib
        
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
    
    def render_diagram(self, plantuml_code: str) -> Optional[QPixmap]:
        """Render PlantUML diagram and return QPixmap"""
        try:
            # Check cache first
            cache_path = self._get_cache_path(plantuml_code)
            if cache_path.exists():
                pixmap = QPixmap(str(cache_path))
                if not pixmap.isNull():
                    return pixmap
            
            # Try local JAR first, then web service
            png_data = None
            if self.plantuml_jar_path and self.plantuml_jar_path.exists():
                png_data = self._render_with_jar(plantuml_code)
            
            if png_data is None and self.use_web_service:
                png_data = self._render_with_web_service(plantuml_code)
            
            if png_data:
                # Save to cache
                with open(cache_path, 'wb') as f:
                    f.write(png_data)
                
                # Return as QPixmap
                pixmap = QPixmap()
                if pixmap.loadFromData(png_data):
                    return pixmap
                
        except Exception as e:
            print(f"PlantUML rendering error: {e}")
            import traceback
            traceback.print_exc()
        
        return None
    
    def _render_with_jar(self, plantuml_code: str) -> Optional[bytes]:
        """Render using local PlantUML JAR"""
        try:
            process = subprocess.run([
                'java', '-jar', str(self.plantuml_jar_path),
                '-tpng', '-pipe'
            ], input=plantuml_code.encode(), capture_output=True, timeout=30)
            
            if process.returncode == 0:
                return process.stdout
        except (subprocess.TimeoutExpired, subprocess.SubprocessError, FileNotFoundError):
            pass
        return None
    
    def _render_with_web_service(self, plantuml_code: str) -> Optional[bytes]:
        """Render using PlantUML web service"""
        try:
            encoded = self._encode_plantuml_web(plantuml_code)
            url = f"http://www.plantuml.com/plantuml/png/{encoded}"
            
            response = requests.get(url, timeout=30)
            
            if response.status_code == 200:
                # Check if it's actually a PNG (starts with PNG header)
                if response.content.startswith(b'\x89PNG'):
                    return response.content
                
        except requests.RequestException:
            pass
        return None


class PlantUMLTemplateGenerator:
    """Generates PlantUML code for different SLATEC system stages"""
    
    def __init__(self):
        # Dark theme styling for PlantUML
        self.dark_theme_header = """
!theme blueprint
skinparam backgroundColor #1E1E1E
skinparam componentStyle rectangle
skinparam componentBackgroundColor #2D2D2D
skinparam componentBorderColor #555555
skinparam componentFontColor #E0E0E0
skinparam arrowColor #0D7EC4
skinparam arrowFontColor #E0E0E0
skinparam titleFontColor #E0E0E0
skinparam titleBackgroundColor #1E1E1E
"""
    
    def generate_stage_diagram(self, stage: str, data: Dict[str, Any] = None) -> str:
        """Generate PlantUML code for a specific stage"""
        data = data or {}
        
        if stage.lower() in ['test generation']:
            return self._generate_test_generation_diagram(data)
        elif stage.lower() in ['modernize']:
            return self._generate_modernization_diagram(data)
        elif stage.lower() in ['validate']:
            return self._generate_validation_diagram(data)
        else:
            return self._generate_queue_diagram(data)
    
    def _generate_test_generation_diagram(self, data: Dict[str, Any]) -> str:
        """Generate test generation architecture diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        progress = data.get('progress', 0)
        completed = data.get('completed', False)
        test_count = data.get('test_count', '130')
        newline = '\\n'  # Extract backslash to avoid f-string limitation
        
        status = "Complete" if completed else f"{progress}% complete"
        
        return f"""
@startuml
{self.dark_theme_header}
title Test Generation Architecture - {status}

component "F77 Source{newline}{function_name}.f" as f77 #4CAF50
component "LLM Generator{newline}(o3-mini)" as llm #FF9800
component "Parameter{newline}Validator" as validator #2196F3
storage "Test Cases{newline}{test_count} generated" as tests #9C27B0

f77 --> llm : Function code
llm --> validator : Generated tests
validator --> tests : Valid tests
validator --> llm : Constraint violations

note right of tests
  {"✓ Generated" if completed else "⏳ Generating..."}
  Coverage: Edge cases,
  boundary conditions,
  numerical stability
end note

@enduml
"""
    
    def _generate_modernization_diagram(self, data: Dict[str, Any]) -> str:
        """Generate modernization architecture diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        progress = data.get('progress', 0)
        completed = data.get('completed', False)
        iteration = data.get('iteration', 0)
        newline = '\\n'  # Extract backslash to avoid f-string limitation
        
        if iteration > 0:
            status = f"Refinement (Iteration {iteration})"
        else:
            status = "Initial Modernization"
            
        if completed:
            status += " - Complete"
        else:
            status += f" - {progress}%"
        
        return f"""
@startuml
{self.dark_theme_header}
title Modernization Architecture - {status}

component "F77 Code{newline}{function_name}.f" as f77 #4CAF50
component "Test Cases{newline}(Generated)" as tests #4CAF50
component "Error History{newline}(Previous iterations)" as errors #4CAF50
component "LLM Converter{newline}(o3-mini)" as llm #FF9800
storage "F90 Module{newline}{function_name}_module.f90" as f90 #9C27B0

f77 --> llm : Original code
tests --> llm : Test constraints  
errors --> llm : Fix guidance
llm --> f90 : Modern code

note right of f90
  {"✓ Generated" if completed else "⏳ Converting..."}
  Features: Module structure,
  pure functions, type safety,
  modern Fortran syntax
end note

@enduml
"""
    
    def _generate_validation_diagram(self, data: Dict[str, Any]) -> str:
        """Generate validation architecture diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        progress = data.get('progress', 0)
        completed = data.get('completed', False)
        pass_rate = data.get('pass_rate', 0.0)
        iteration = data.get('iteration', 0)
        newline = '\\n'  # Extract backslash to avoid f-string limitation
        
        status = f"{int(pass_rate * 100)}% pass rate"
        if completed and pass_rate >= 1.0:
            status = "✓ PASSED - 100%"
        elif completed:
            status = f"⚠ {status} - Needs refinement"
        else:
            status = f"⏳ Testing... {status}"
        
        return f"""
@startuml
{self.dark_theme_header}
title Validation Architecture - {status}

component "F77 Binary{newline}(Reference)" as f77_bin #4CAF50
component "F90 Binary{newline}(Modern)" as f90_bin #4CAF50
component "Test Runner{newline}(130 test cases)" as runner #2196F3
component "Result Comparator{newline}(Numerical accuracy)" as comp #2196F3
storage "Pass/Fail Results{newline}{int(pass_rate * 100)}% pass rate" as results #9C27B0

f77_bin --> runner : Execute tests
f90_bin --> runner : Execute tests
runner --> comp : F77 results
runner --> comp : F90 results
comp --> results : Comparison

note right of results
  {"✓ Validation complete" if completed else "⏳ Running tests..."}
  Tolerance: ±1.0e-15
  Categories: Boundary,
  edge cases, precision
end note

@enduml
"""
    
    def _generate_queue_diagram(self, data: Dict[str, Any]) -> str:
        """Generate queue system diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        newline = '\\n'  # Extract backslash to avoid f-string limitation
        
        return f"""
@startuml
{self.dark_theme_header}
title Function Processing Queue

queue "Processing Queue" as queue {{
  component "{function_name}{newline}Ready for processing" as func #2196F3
}}

note right of func
  Function: {function_name}
  Type: Mathematical utility
  Dependencies: None
  Complexity: Level 0
  
  Ready to begin modernization...
end note

@enduml
"""


class AsyncPlantUMLRenderer(QThread):
    """Async PlantUML renderer to prevent UI blocking"""
    
    diagram_ready = pyqtSignal(QPixmap)
    error_occurred = pyqtSignal(str)
    
    def __init__(self, renderer: PlantUMLRenderer, plantuml_code: str):
        super().__init__()
        self.renderer = renderer
        self.plantuml_code = plantuml_code
    
    def run(self):
        """Render diagram in background thread"""
        try:
            pixmap = self.renderer.render_diagram(self.plantuml_code)
            if pixmap and not pixmap.isNull():
                self.diagram_ready.emit(pixmap)
            else:
                self.error_occurred.emit("Failed to render PlantUML diagram")
        except Exception as e:
            self.error_occurred.emit(f"PlantUML rendering error: {str(e)}")
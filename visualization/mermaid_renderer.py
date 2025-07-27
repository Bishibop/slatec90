"""
Mermaid Renderer for SLATEC System Architecture Diagrams
Much simpler than PlantUML with better web integration
"""
import base64
import hashlib
import tempfile
from pathlib import Path
from typing import Dict, Any, Optional
import requests
from PyQt6.QtCore import QObject
from PyQt6.QtGui import QPixmap


class MermaidRenderer(QObject):
    """Mermaid diagram renderer using web service"""
    
    def __init__(self):
        super().__init__()
        self.cache_dir = Path(tempfile.gettempdir()) / "slatec_mermaid_cache"
        self.cache_dir.mkdir(exist_ok=True)
        
    def _get_cache_path(self, mermaid_code: str) -> Path:
        """Generate cache file path for given Mermaid code"""
        code_hash = hashlib.md5(mermaid_code.encode()).hexdigest()
        return self.cache_dir / f"{code_hash}.png"
    
    def render_diagram(self, mermaid_code: str) -> Optional[QPixmap]:
        """Render Mermaid diagram and return QPixmap"""
        try:
            # Check cache first
            cache_path = self._get_cache_path(mermaid_code)
            if cache_path.exists():
                pixmap = QPixmap(str(cache_path))
                if not pixmap.isNull():
                    return pixmap
            
            # Render using Mermaid web service
            png_data = self._render_with_web_service(mermaid_code)
            
            if png_data:
                # Save to cache
                with open(cache_path, 'wb') as f:
                    f.write(png_data)
                
                # Return as QPixmap
                pixmap = QPixmap()
                if pixmap.loadFromData(png_data):
                    return pixmap
                
        except Exception as e:
            print(f"Mermaid rendering error: {e}")
        
        return None
    
    def _render_with_web_service(self, mermaid_code: str) -> Optional[bytes]:
        """Render using Mermaid.ink web service"""
        try:
            # Mermaid.ink uses simple base64 encoding
            encoded = base64.b64encode(mermaid_code.encode('utf-8')).decode('ascii')
            url = f"https://mermaid.ink/img/{encoded}"
            
            response = requests.get(url, timeout=30)
            
            if response.status_code == 200:
                # Check if it's a valid image format (PNG, JPEG, or SVG)
                if (response.content.startswith(b'\x89PNG') or 
                    response.content.startswith(b'\xff\xd8\xff') or  # JPEG
                    response.content.startswith(b'<svg')):
                    return response.content
                
        except requests.RequestException:
            pass
        return None


class MermaidTemplateGenerator:
    """Generates Mermaid code for different SLATEC system stages"""
    
    def __init__(self):
        # Dark theme configuration for Mermaid
        self.dark_theme_config = """
%%{init: {
  'theme': 'dark',
  'themeVariables': {
    'primaryColor': '#2D2D2D',
    'primaryTextColor': '#E0E0E0',
    'primaryBorderColor': '#555555',
    'lineColor': '#0D7EC4',
    'secondaryColor': '#3A3A3A',
    'tertiaryColor': '#1E1E1E',
    'background': '#1E1E1E',
    'mainBkg': '#2D2D2D',
    'secondBkg': '#3A3A3A',
    'tertiaryBkg': '#4A4A4A'
  }
}}%%
"""
    
    def generate_stage_diagram(self, stage: str, data: Dict[str, Any] = None) -> str:
        """Generate Mermaid code for a specific stage"""
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
        
        status = "Complete" if completed else f"{progress}% complete"
        
        return f"""{self.dark_theme_config}
graph TD
    F77["{function_name}.f<br/>F77 Source"] --> LLM["LLM Generator<br/>(o3-mini)"]
    LLM --> VAL["Parameter<br/>Validator"]
    VAL --> TESTS["Test Cases<br/>{test_count} generated"]
    VAL -.-> LLM
    
    style F77 fill:#4CAF50
    style LLM fill:#FF9800
    style VAL fill:#2196F3
    style TESTS fill:#9C27B0
    
    classDef default color:#E0E0E0
"""
    
    def _generate_modernization_diagram(self, data: Dict[str, Any]) -> str:
        """Generate modernization architecture diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        progress = data.get('progress', 0)
        completed = data.get('completed', False)
        iteration = data.get('iteration', 0)
        
        if iteration > 0:
            status = f"Refinement (Iteration {iteration})"
        else:
            status = "Initial Modernization"
            
        if completed:
            status += " - Complete"
        else:
            status += f" - {progress}%"
        
        return f"""{self.dark_theme_config}
graph TD
    F77["{function_name}.f<br/>Original F77"] --> LLM["LLM Converter<br/>(o3-mini)"]
    TESTS["Test Cases<br/>(Generated)"] --> LLM
    ERRORS["Error History<br/>(Previous iterations)"] --> LLM
    LLM --> F90["{function_name}_module.f90<br/>Modern F90"]
    
    style F77 fill:#4CAF50
    style TESTS fill:#4CAF50
    style ERRORS fill:#4CAF50
    style LLM fill:#FF9800
    style F90 fill:#9C27B0
    
    classDef default color:#E0E0E0
"""
    
    def _generate_validation_diagram(self, data: Dict[str, Any]) -> str:
        """Generate validation architecture diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        progress = data.get('progress', 0)
        completed = data.get('completed', False)
        pass_rate = data.get('pass_rate', 0.0)
        
        status = f"{int(pass_rate * 100)}% pass rate"
        if completed and pass_rate >= 1.0:
            status = "✓ PASSED - 100%"
        elif completed:
            status = f"⚠ {status} - Needs refinement"
        else:
            status = f"⏳ Testing... {status}"
        
        return f"""{self.dark_theme_config}
graph TD
    F77BIN["F77 Binary<br/>(Reference)"] --> RUNNER["Test Runner<br/>(130 test cases)"]
    F90BIN["F90 Binary<br/>(Modern)"] --> RUNNER
    RUNNER --> COMP["Result Comparator<br/>(Numerical accuracy)"]
    COMP --> RESULTS["Pass/Fail Results<br/>{int(pass_rate * 100)}% pass rate"]
    
    style F77BIN fill:#4CAF50
    style F90BIN fill:#4CAF50
    style RUNNER fill:#2196F3
    style COMP fill:#2196F3
    style RESULTS fill:#9C27B0
    
    classDef default color:#E0E0E0
"""
    
    def _generate_queue_diagram(self, data: Dict[str, Any]) -> str:
        """Generate queue system diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        
        return f"""{self.dark_theme_config}
graph TD
    QUEUE["Processing Queue"] --> FUNC["{function_name}<br/>Ready for processing"]
    
    style QUEUE fill:#2196F3
    style FUNC fill:#2196F3
    
    classDef default color:#E0E0E0
"""
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
        if not mermaid_code:
            return None
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
            # Request PNG with higher scale for better resolution
            # Use fit parameter to remove whitespace
            url = f"https://mermaid.ink/img/{encoded}?bgColor=!1E1E1E"
            
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
    'darkMode': true,
    'background': '#1E1E1E',
    'primaryColor': '#2D2D2D',
    'primaryTextColor': '#E0E0E0',
    'primaryBorderColor': '#555555',
    'lineColor': '#0D7EC4',
    'secondaryColor': '#3A3A3A',
    'tertiaryColor': '#1E1E1E',
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
        elif stage.lower() in ['modernize', 'modernization']:
            return self._generate_modernization_diagram(data)
        elif stage.lower() in ['validate', 'validation']:
            return self._generate_validation_diagram(data)
        elif stage.lower() in ['output']:
            return self._generate_output_diagram(data)
        elif stage.lower() in ['queue']:
            return self._generate_queue_diagram(data)
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
    F77["{function_name}.f"] --> LLM["LLM Generator"]
    LLM --> FORMAT["Numeric Format Fixer"]
    FORMAT --> VAL["Parameter Validator"]
    VAL --> TESTS["Test Cases"]
    VAL --> STATS["Validation Report"]
    
    style F77 fill:#505050
    style LLM fill:#606060
    style FORMAT fill:#707070
    style VAL fill:#808080
    style TESTS fill:#909090
    style STATS fill:#A0A0A0
    
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
    F77["{function_name}.f"] --> LLM["LLM Modernizer"]
    TESTS["Test Cases"] --> LLM
    ERRORS["Error History"] --> LLM
    LLM --> F90["{function_name}_module.f90"]
    
    style F77 fill:#505050
    style TESTS fill:#606060
    style ERRORS fill:#707070
    style LLM fill:#808080
    style F90 fill:#909090
    
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
    F77BIN["F77 Binary"] --> VALIDATOR["Validator"]
    F90BIN["F90 Binary"] --> VALIDATOR
    TESTS["Test Cases"] --> VALIDATOR
    VALIDATOR --> RESULTS["Validation Results"]
    
    style F77BIN fill:#606060
    style F90BIN fill:#707070
    style TESTS fill:#808080
    style VALIDATOR fill:#909090
    style RESULTS fill:#A0A0A0
    
    classDef default color:#E0E0E0
"""
    
    def _generate_queue_diagram(self, data: Dict[str, Any]) -> str:
        """Generate queue system diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        
        return f"""{self.dark_theme_config}
graph TD
    FUNC["{function_name}.f"] --> COMP["Complexity Analysis"]
    FUNC --> DEP["Dependency Analysis"]
    
    COMP --> QUEUE["Prioritized Queue"]
    DEP --> QUEUE
    
    style FUNC fill:#606060
    style COMP fill:#707070
    style DEP fill:#808080
    style QUEUE fill:#909090
    
    classDef default color:#E0E0E0
"""
    
    def _generate_output_diagram(self, data: Dict[str, Any]) -> str:
        """Generate output/delivery architecture diagram"""
        function_name = data.get('function_name', 'PYTHAG')
        
        return f"""{self.dark_theme_config}
graph TD
    REPORT["Report Generator"] --> FINAL["{function_name} Modernization Complete"]
    FUNCTION["{function_name}_module.f90"] --> FINAL
    
    style REPORT fill:#707070
    style FUNCTION fill:#808080
    style FINAL fill:#909090
    
    classDef default color:#E0E0E0
"""
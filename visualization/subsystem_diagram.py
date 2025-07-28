"""
Subsystem Architecture Diagram Widget
Displays professional PlantUML-generated system architecture diagrams
"""
from PyQt6.QtWidgets import QWidget, QLabel, QVBoxLayout
from PyQt6.QtCore import Qt, QSize
from PyQt6.QtGui import QPixmap, QPainter, QColor, QFont, QPen, QBrush
from typing import Dict, Any, Optional

from .mermaid_renderer import MermaidRenderer, MermaidTemplateGenerator


class SubsystemDiagramWidget(QWidget):
    """Widget that displays Mermaid-generated architecture diagrams for different subsystems"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.current_stage = "queue"
        self.current_data = {}
        self.setMinimumSize(400, 300)
        
        # Mermaid components
        self.renderer = MermaidRenderer()
        self.template_generator = MermaidTemplateGenerator()
        
        
        # UI setup
        self.init_ui()
        
        # Current diagram state
        self.current_pixmap = None
        
    def init_ui(self):
        """Initialize the UI layout"""
        layout = QVBoxLayout()
        layout.setContentsMargins(0, 0, 0, 0)
        self.setLayout(layout)
        
    def set_stage(self, stage: str, data: Dict[str, Any] = None):
        """Set the current stage and data to display"""
        self.current_stage = stage.lower()
        self.current_data = data or {}
        self._render_diagram()
        
    def _render_diagram(self):
        """Render the Mermaid diagram for current stage"""
        try:
            # Generate Mermaid code
            mermaid_code = self.template_generator.generate_stage_diagram(
                self.current_stage, self.current_data
            )
            
            if not mermaid_code:
                # No diagram for this stage
                self.current_pixmap = None
                self.update()
                return
            
            # Render with Mermaid
            pixmap = self.renderer.render_diagram(mermaid_code)
            self.current_pixmap = pixmap
            self.update()
                
        except Exception as e:
            self.current_pixmap = None
            self.update()
    
    def paintEvent(self, event):
        """Paint the diagram"""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        if self.current_pixmap:
            # Draw Mermaid diagram
            self._draw_mermaid_diagram(painter)
        else:
            # No diagram to show - clear background
            painter.fillRect(self.rect(), QColor("#1E1E1E"))
            
    def _draw_mermaid_diagram(self, painter: QPainter):
        """Draw the Mermaid-generated diagram"""
        if not self.current_pixmap:
            return
            
        # Calculate scaling to fit widget while maintaining aspect ratio
        widget_size = self.size()
        pixmap_size = self.current_pixmap.size()
        
        scale_x = widget_size.width() / pixmap_size.width()
        scale_y = widget_size.height() / pixmap_size.height()
        scale = min(scale_x, scale_y) * 0.95  # Use 95% of available space
        
        # Calculate centered position
        scaled_width = int(pixmap_size.width() * scale)
        scaled_height = int(pixmap_size.height() * scale)
        x = (widget_size.width() - scaled_width) // 2
        y = (widget_size.height() - scaled_height) // 2
        
        # Draw scaled pixmap
        scaled_pixmap = self.current_pixmap.scaled(
            QSize(scaled_width, scaled_height),
            Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation
        )
        
        painter.drawPixmap(x, y, scaled_pixmap)

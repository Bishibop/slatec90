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
        
        # Fallback colors for custom drawing (if Mermaid fails)
        self.fallback_colors = {
            'input': QColor('#4CAF50'),      # Green
            'process': QColor('#2196F3'),    # Blue  
            'llm': QColor('#FF9800'),        # Orange
            'output': QColor('#9C27B0'),     # Purple
            'arrow': QColor('#666666'),      # Gray
            'text': QColor('#FFFFFF'),       # White
            'background': QColor('#2C2C2C')  # Dark gray
        }
        
        # UI setup
        self.init_ui()
        
        # Current diagram state
        self.current_pixmap = None
        self.use_fallback = False
        
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
            
            # Try to render with Mermaid
            pixmap = self.renderer.render_diagram(mermaid_code)
            
            if pixmap and not pixmap.isNull():
                self.current_pixmap = pixmap
                self.use_fallback = False
                self.update()
            else:
                # Fall back to custom drawing
                self.use_fallback = True
                self.current_pixmap = None
                self.update()
                
        except Exception as e:
            # Fall back to custom drawing
            self.use_fallback = True
            self.current_pixmap = None
            self.update()
    
    def paintEvent(self, event):
        """Paint the diagram"""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        if self.current_pixmap and not self.use_fallback:
            # Draw Mermaid diagram
            self._draw_mermaid_diagram(painter)
        else:
            # Fall back to custom drawing
            self._draw_fallback_diagram(painter)
            
    def _draw_mermaid_diagram(self, painter: QPainter):
        """Draw the Mermaid-generated diagram"""
        if not self.current_pixmap:
            return
            
        # Calculate scaling to fit widget while maintaining aspect ratio
        widget_size = self.size()
        pixmap_size = self.current_pixmap.size()
        
        scale_x = widget_size.width() / pixmap_size.width()
        scale_y = widget_size.height() / pixmap_size.height()
        scale = min(scale_x, scale_y)
        
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
        
    def _draw_fallback_diagram(self, painter: QPainter):
        """Draw fallback diagram using custom painting"""
        # Clear background
        painter.fillRect(self.rect(), self.fallback_colors['background'])
        
        # Draw the appropriate diagram based on current stage
        if self.current_stage in ['test generation']:
            self._draw_test_generation_fallback(painter)
        elif self.current_stage in ['modernize']:
            self._draw_modernization_fallback(painter)
        elif self.current_stage in ['validate']:
            self._draw_validation_fallback(painter)
        else:
            self._draw_queue_fallback(painter)
            
    def _draw_test_generation_fallback(self, painter: QPainter):
        """Draw test generation system architecture (fallback)"""
        from PyQt6.QtCore import QRect
        width = self.width()
        height = self.height()
        
        # Component positions and sizes
        box_width = 100
        box_height = 60
        y_center = height // 2
        
        # F77 Source (input)
        f77_rect = QRect(20, y_center - box_height//2, box_width, box_height)
        self._draw_component(painter, f77_rect, "F77 Source", self.fallback_colors['input'])
        
        # LLM (process)
        llm_rect = QRect(width//2 - box_width//2, y_center - box_height//2, box_width, box_height)
        self._draw_component(painter, llm_rect, "LLM\n(o3-mini)", self.fallback_colors['llm'])
        
        # Test Cases (output)
        tests_rect = QRect(width - box_width - 20, y_center - box_height//2, box_width, box_height)
        self._draw_component(painter, tests_rect, "Test Cases\n(50-100)", self.fallback_colors['output'])
        
        # Arrows
        self._draw_arrow(painter, f77_rect.right(), y_center, llm_rect.left(), y_center)
        self._draw_arrow(painter, llm_rect.right(), y_center, tests_rect.left(), y_center)
        
        # Parameter Validator (below LLM)
        validator_rect = QRect(width//2 - box_width//2, y_center + box_height//2 + 20, box_width, 40)
        self._draw_component(painter, validator_rect, "Validator", self.fallback_colors['process'])
        
        # Curved arrow from LLM to Validator and back
        self._draw_curved_arrow(painter, llm_rect.center().x(), llm_rect.bottom(), 
                               validator_rect.center().x(), validator_rect.top())
        
        # Title
        self._draw_title(painter, "Test Generation Architecture")
        
    def _draw_modernization_fallback(self, painter: QPainter):
        """Draw modernization system architecture (fallback)"""
        from PyQt6.QtCore import QRect
        width = self.width()
        height = self.height()
        
        box_width = 80
        box_height = 50
        y_top = height // 3
        y_bottom = 2 * height // 3
        
        # Inputs (top row)
        f77_rect = QRect(20, y_top - box_height//2, box_width, box_height)
        self._draw_component(painter, f77_rect, "F77 Code", self.fallback_colors['input'])
        
        tests_rect = QRect(120, y_top - box_height//2, box_width, box_height)
        self._draw_component(painter, tests_rect, "Test Cases", self.fallback_colors['input'])
        
        errors_rect = QRect(220, y_top - box_height//2, box_width, box_height)
        self._draw_component(painter, errors_rect, "Error History", self.fallback_colors['input'])
        
        # LLM (center)
        llm_rect = QRect(width//2 - box_width//2, height//2 - box_height//2, box_width, box_height)
        self._draw_component(painter, llm_rect, "LLM\nConverter", self.fallback_colors['llm'])
        
        # Output (bottom)
        f90_rect = QRect(width//2 - box_width//2, y_bottom - box_height//2, box_width, box_height)
        self._draw_component(painter, f90_rect, "F90 Module", self.fallback_colors['output'])
        
        # Arrows to LLM
        self._draw_arrow(painter, f77_rect.center().x(), f77_rect.bottom(), 
                        llm_rect.center().x() - 30, llm_rect.top())
        self._draw_arrow(painter, tests_rect.center().x(), tests_rect.bottom(), 
                        llm_rect.center().x(), llm_rect.top())
        self._draw_arrow(painter, errors_rect.center().x(), errors_rect.bottom(), 
                        llm_rect.center().x() + 30, llm_rect.top())
        
        # Arrow from LLM to output
        self._draw_arrow(painter, llm_rect.center().x(), llm_rect.bottom(), 
                        f90_rect.center().x(), f90_rect.top())
        
        self._draw_title(painter, "Modernization Architecture")
        
    def _draw_validation_fallback(self, painter: QPainter):
        """Draw validation system architecture (fallback)"""
        from PyQt6.QtCore import QRect
        width = self.width()
        height = self.height()
        
        box_width = 80
        box_height = 50
        y_center = height // 2
        
        # F77 Binary
        f77_rect = QRect(20, y_center - box_height - 10, box_width, box_height)
        self._draw_component(painter, f77_rect, "F77 Binary", self.fallback_colors['input'])
        
        # F90 Binary  
        f90_rect = QRect(20, y_center + 10, box_width, box_height)
        self._draw_component(painter, f90_rect, "F90 Binary", self.fallback_colors['input'])
        
        # Test Runner
        runner_rect = QRect(width//3, y_center - box_height//2, box_width, box_height)
        self._draw_component(painter, runner_rect, "Test Runner", self.fallback_colors['process'])
        
        # Comparator
        comp_rect = QRect(2*width//3, y_center - box_height//2, box_width, box_height)
        self._draw_component(painter, comp_rect, "Comparator", self.fallback_colors['process'])
        
        # Results
        results_rect = QRect(width - box_width - 20, y_center - box_height//2, box_width, box_height)
        self._draw_component(painter, results_rect, "Pass/Fail\nResults", self.fallback_colors['output'])
        
        # Arrows
        self._draw_arrow(painter, f77_rect.right(), f77_rect.center().y(), 
                        runner_rect.left(), runner_rect.center().y() - 15)
        self._draw_arrow(painter, f90_rect.right(), f90_rect.center().y(), 
                        runner_rect.left(), runner_rect.center().y() + 15)
        self._draw_arrow(painter, runner_rect.right(), runner_rect.center().y(), 
                        comp_rect.left(), comp_rect.center().y())
        self._draw_arrow(painter, comp_rect.right(), comp_rect.center().y(), 
                        results_rect.left(), results_rect.center().y())
        
        self._draw_title(painter, "Validation Architecture")
        
    def _draw_queue_fallback(self, painter: QPainter):
        """Draw simple queue system (fallback)"""
        from PyQt6.QtCore import QRect
        width = self.width()
        height = self.height()
        
        # Simple queue representation
        queue_rect = QRect(width//4, height//2 - 40, width//2, 80)
        self._draw_component(painter, queue_rect, "Processing Queue\n\nFunctions waiting\nfor modernization", self.fallback_colors['process'])
        
        self._draw_title(painter, "Function Queue")
        
    def _draw_component(self, painter: QPainter, rect, text: str, color: QColor):
        """Draw a component box with text"""
        # Draw rounded rectangle
        painter.setPen(QPen(color.darker(), 2))
        painter.setBrush(QBrush(color.lighter(120)))
        painter.drawRoundedRect(rect, 8, 8)
        
        # Draw text
        painter.setPen(QPen(self.fallback_colors['text']))
        font = QFont()
        font.setPixelSize(10)
        font.setBold(True)
        painter.setFont(font)
        painter.drawText(rect, Qt.AlignmentFlag.AlignCenter, text)
        
    def _draw_arrow(self, painter: QPainter, x1: int, y1: int, x2: int, y2: int):
        """Draw an arrow from (x1,y1) to (x2,y2)"""
        painter.setPen(QPen(self.fallback_colors['arrow'], 2))
        painter.drawLine(x1, y1, x2, y2)
        
        # Draw arrowhead
        import math
        angle = math.atan2(y2 - y1, x2 - x1)
        arrowhead_length = 10
        arrowhead_angle = math.pi / 6
        
        # Calculate arrowhead points
        x3 = x2 - arrowhead_length * math.cos(angle - arrowhead_angle)
        y3 = y2 - arrowhead_length * math.sin(angle - arrowhead_angle)
        x4 = x2 - arrowhead_length * math.cos(angle + arrowhead_angle)
        y4 = y2 - arrowhead_length * math.sin(angle + arrowhead_angle)
        
        # Draw arrowhead
        painter.drawLine(x2, y2, int(x3), int(y3))
        painter.drawLine(x2, y2, int(x4), int(y4))
        
    def _draw_curved_arrow(self, painter: QPainter, x1: int, y1: int, x2: int, y2: int):
        """Draw a curved arrow (for feedback loops)"""
        from PyQt6.QtGui import QPainterPath
        painter.setPen(QPen(self.fallback_colors['arrow'], 2))
        
        # Create curved path
        path = QPainterPath()
        path.moveTo(x1, y1)
        
        # Control points for curve
        ctrl1_x = x1 + 20
        ctrl1_y = y1 + 10
        ctrl2_x = x2 - 20
        ctrl2_y = y2 - 10
        
        path.cubicTo(ctrl1_x, ctrl1_y, ctrl2_x, ctrl2_y, x2, y2)
        painter.drawPath(path)
        
    def _draw_title(self, painter: QPainter, title: str):
        """Draw the diagram title"""
        painter.setPen(QPen(self.fallback_colors['text']))
        font = QFont()
        font.setPixelSize(14)
        font.setBold(True)
        painter.setFont(font)
        painter.drawText(10, 20, title)
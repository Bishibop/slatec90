"""
Subsystem Detail Panel
Container that holds both architecture diagram and output display for each stage
"""
from PyQt6.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout, QLabel, QSplitter
from PyQt6.QtCore import Qt
from typing import Dict, Any

from .subsystem_diagram import SubsystemDiagramWidget
from .output_display import OutputDisplayWidget


class SubsystemDetailPanel(QWidget):
    """Panel that shows subsystem architecture diagram and outputs"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.current_stage = "queue"
        self.current_data = {}
        self.init_ui()
        
    def init_ui(self):
        """Initialize the UI"""
        # Main horizontal layout
        main_layout = QHBoxLayout()
        main_layout.setContentsMargins(5, 5, 5, 5)
        
        # Create splitter for resizable panels
        splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Left side: Architecture diagram
        diagram_container = QWidget()
        diagram_layout = QVBoxLayout()
        diagram_layout.setContentsMargins(0, 0, 0, 0)
        
        # Diagram widget (no title)
        self.diagram_widget = SubsystemDiagramWidget()
        diagram_layout.addWidget(self.diagram_widget)
        
        diagram_container.setLayout(diagram_layout)
        splitter.addWidget(diagram_container)
        
        # Right side: Output display
        self.output_widget = OutputDisplayWidget()
        splitter.addWidget(self.output_widget)
        
        # Set initial splitter sizes (50% diagram, 50% output)
        splitter.setSizes([500, 500])
        splitter.setStretchFactor(0, 0)  # Diagram doesn't stretch
        splitter.setStretchFactor(1, 1)  # Output stretches
        
        main_layout.addWidget(splitter)
        self.setLayout(main_layout)
        
        # Style the splitter
        splitter.setStyleSheet("""
            QSplitter::handle {
                background-color: #555555;
                width: 3px;
            }
            QSplitter::handle:hover {
                background-color: #777777;
            }
        """)
        
    def set_stage(self, stage: str, data: Dict[str, Any] = None):
        """Update the panel for a specific stage"""
        self.current_stage = stage
        self.current_data = data or {}
        
        # Update both widgets - pass data to diagram widget
        self.diagram_widget.set_stage(stage, data)
        self.output_widget.set_stage_data(stage, data)
        
    def update_stage_data(self, data: Dict[str, Any]):
        """Update the data for the current stage"""
        self.current_data.update(data)
        self.output_widget.set_stage_data(self.current_stage, self.current_data)
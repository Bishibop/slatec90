"""
Panel-based visualization for SLATEC modernization pipeline
Shows functions flowing through vertical panels with backward flow for refinement
"""
from PyQt6.QtWidgets import (QWidget, QHBoxLayout, QVBoxLayout, QLabel, 
                            QFrame, QGraphicsOpacityEffect, QScrollArea,
                            QSizePolicy)
from PyQt6.QtCore import (Qt, QPropertyAnimation, QPoint, QEasingCurve, 
                         QParallelAnimationGroup, QSequentialAnimationGroup,
                         pyqtSignal, QTimer, pyqtProperty)
from PyQt6.QtGui import QPainter, QColor, QFont, QPalette
from typing import Dict, List, Optional
import math

from .styles import COLORS, ANIMATION
from .event_system import EventType, Event

class FunctionCard(QWidget):
    """A card representing a function being processed"""
    clicked = pyqtSignal(str)  # Emits function name when clicked
    
    def __init__(self, function_name: str, parent=None):
        super().__init__(parent)
        self.function_name = function_name
        self.status = "pending"
        self.progress = 0
        self.pass_rate = 0.0
        self.iteration = 0
        self.panel_name = ""
        
        self.init_ui()
        self.setFixedSize(180, 60)
        self.setCursor(Qt.CursorShape.PointingHandCursor)
        
    def init_ui(self):
        """Initialize the card UI"""
        layout = QVBoxLayout()
        layout.setContentsMargins(8, 4, 8, 4)
        layout.setSpacing(2)
        
        # Function name label
        self.name_label = QLabel(self.function_name)
        self.name_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        font = QFont()
        font.setBold(True)
        font.setPixelSize(12)
        self.name_label.setFont(font)
        layout.addWidget(self.name_label)
        
        # Progress bar (custom painted)
        self.progress_widget = QWidget()
        self.progress_widget.setFixedHeight(4)
        layout.addWidget(self.progress_widget)
        
        # Iteration label (shown above status when applicable)
        self.iteration_label = QLabel("")
        self.iteration_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        font = QFont()
        font.setPixelSize(9)
        font.setBold(True)
        self.iteration_label.setFont(font)
        self.iteration_label.setStyleSheet("color: #FFB74D;")  # Orange color for iterations
        layout.addWidget(self.iteration_label)
        
        # Status label
        self.status_label = QLabel("")
        self.status_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        font = QFont()
        font.setPixelSize(10)
        self.status_label.setFont(font)
        layout.addWidget(self.status_label)
        
        self.setLayout(layout)
        self.update_appearance()
        
    def update_appearance(self):
        """Update the card appearance based on status"""
        # Background color based on status
        color_map = {
            "pending": COLORS['surface_light'],
            "active": COLORS['accent'],
            "success": COLORS['success'],
            "error": COLORS['error'],
            "refining": COLORS['warning'],
        }
        
        bg_color = color_map.get(self.status, COLORS['surface'])
        
        # Style sheet
        self.setStyleSheet(f"""
            FunctionCard {{
                background-color: {bg_color};
                border-radius: 8px;
                border: 2px solid {COLORS['node_border']};
            }}
        """)
        
        # Update iteration label
        if self.iteration > 0:
            self.iteration_label.setText(f"Iteration {self.iteration}")
        else:
            self.iteration_label.setText("")
            
        # Update status label
        if self.pass_rate > 0 and self.pass_rate < 1.0:
            self.status_label.setText(f"{int(self.pass_rate*100)}% tests passed")
        elif self.status == "success" and self.panel_name == "Validate":
            self.status_label.setText("100% tests passed")
        elif self.status == "success" and self.panel_name == "Test Generation":
            self.status_label.setText("130 test cases generated")
        elif self.status == "success" and self.panel_name == "Modernize":
            self.status_label.setText("Modernized F90 code generated")
        elif self.status == "error":
            self.status_label.setText("Failed")
        elif self.status == "active" and self.panel_name == "Test Generation":
            self.status_label.setText("Generating test cases...")
        elif self.status == "active" and self.panel_name == "Modernize":
            self.status_label.setText("Modernizing function code...")
        elif self.status == "refining" and self.panel_name == "Modernize":
            self.status_label.setText("Refining function code...")
        elif self.status == "active" and self.panel_name == "Validate":
            self.status_label.setText("Validating...")
        else:
            self.status_label.setText("")
            
        self.update()
        
    def set_status(self, status: str):
        """Set the card status"""
        self.status = status
        self.update_appearance()
        
    def set_progress(self, progress: int):
        """Set progress (0-100)"""
        self.progress = progress
        self.update()
        
    def set_pass_rate(self, rate: float):
        """Set validation pass rate"""
        self.pass_rate = rate
        self.update_appearance()
        
    def set_iteration(self, iteration: int):
        """Set refinement iteration"""
        self.iteration = iteration
        self.update_appearance()
        
    def paintEvent(self, event):
        """Custom paint for progress bar"""
        super().paintEvent(event)
        
        if self.progress > 0:
            painter = QPainter(self)
            painter.setRenderHint(QPainter.RenderHint.Antialiasing)
            
            # Draw progress bar
            progress_rect = self.progress_widget.rect()
            progress_width = int(progress_rect.width() * self.progress / 100)
            
            painter.fillRect(progress_rect.x(), 
                           progress_rect.y() + self.progress_widget.y(),
                           progress_width, 
                           progress_rect.height(),
                           QColor(COLORS['success']))
            
    def mousePressEvent(self, event):
        """Handle mouse clicks"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.clicked.emit(self.function_name)
            
    def start_glow_animation(self):
        """Start a glow effect animation"""
        effect = QGraphicsOpacityEffect()
        self.setGraphicsEffect(effect)
        
        self.glow_anim = QPropertyAnimation(effect, b"opacity")
        self.glow_anim.setDuration(1000)
        self.glow_anim.setStartValue(1.0)
        self.glow_anim.setEndValue(0.7)
        self.glow_anim.setLoopCount(-1)
        self.glow_anim.setEasingCurve(QEasingCurve.Type.InOutSine)
        self.glow_anim.start()
        
    def stop_glow_animation(self):
        """Stop the glow effect"""
        if hasattr(self, 'glow_anim'):
            self.glow_anim.stop()
            self.setGraphicsEffect(None)

class ProcessPanel(QFrame):
    """A vertical panel representing a processing stage"""
    
    def __init__(self, name: str, icon: str = "", parent=None):
        super().__init__(parent)
        self.name = name
        self.icon = icon
        self.cards: Dict[str, FunctionCard] = {}
        self.init_ui()
        
    def init_ui(self):
        """Initialize the panel UI"""
        self.setFrameStyle(QFrame.Shape.Box)
        self.setStyleSheet(f"""
            ProcessPanel {{
                background-color: {COLORS['surface']};
                border: 1px solid {COLORS['node_border']};
                border-radius: 8px;
            }}
        """)
        
        layout = QVBoxLayout()
        layout.setSpacing(8)
        
        # Header
        header_text = self.name if not self.icon else f"{self.icon} {self.name}"
        header = QLabel(header_text)
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        header.setStyleSheet(f"""
            QLabel {{
                background-color: {COLORS['surface_light']};
                padding: 8px;
                border-radius: 4px;
                font-weight: bold;
                font-size: 14px;
            }}
        """)
        layout.addWidget(header)
        
        # Scroll area for cards
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        scroll.setStyleSheet("""
            QScrollArea {
                border: none;
                background: transparent;
            }
        """)
        
        # Container for cards
        self.card_container = QWidget()
        self.card_layout = QVBoxLayout()
        self.card_layout.setSpacing(4)
        self.card_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self.card_container.setLayout(self.card_layout)
        
        scroll.setWidget(self.card_container)
        layout.addWidget(scroll)
        
        self.setLayout(layout)
        self.setMinimumWidth(200)
        self.setSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Expanding)
        
    def add_card(self, card: FunctionCard):
        """Add a function card to this panel"""
        card.panel_name = self.name
        self.cards[card.function_name] = card
        self.card_layout.addWidget(card)
        
    def remove_card(self, function_name: str):
        """Remove a function card from this panel"""
        if function_name in self.cards:
            card = self.cards[function_name]
            self.card_layout.removeWidget(card)
            del self.cards[function_name]
            return card
        return None
        
    def has_card(self, function_name: str) -> bool:
        """Check if this panel contains a card"""
        return function_name in self.cards

class PipelineView(QWidget):
    """Main pipeline visualization with panels and card flow"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.panels: Dict[str, ProcessPanel] = {}
        self.all_cards: Dict[str, FunctionCard] = {}
        self.panel_order = []
        self.init_ui()
        
    def init_ui(self):
        """Initialize the pipeline UI"""
        layout = QHBoxLayout()
        layout.setSpacing(12)
        
        # Create panels
        panel_data = [
            ("Queue", ""),
            ("Test Generation", ""),
            ("Modernize", ""),
            ("Validate", ""),
            ("Output", ""),
        ]
        
        for name, icon in panel_data:
            panel = ProcessPanel(name, icon)
            self.panels[name] = panel
            self.panel_order.append(name)
            layout.addWidget(panel)
            
        self.setLayout(layout)
        
    def add_function(self, function_name: str):
        """Add a new function to the queue"""
        if function_name not in self.all_cards:
            card = FunctionCard(function_name)
            card.clicked.connect(self._on_card_clicked)
            self.all_cards[function_name] = card
            self.panels["Queue"].add_card(card)
            
    def move_card(self, function_name: str, to_panel: str, animate: bool = True):
        """Move a card to a different panel"""
        # If function doesn't exist yet, create it first
        if function_name not in self.all_cards:
            self.add_function(function_name)
            
        card = self.all_cards[function_name]
        from_panel = None
        
        # Find current panel
        for panel_name, panel in self.panels.items():
            if panel.has_card(function_name):
                from_panel = panel_name
                break
                
        if from_panel == to_panel:
            return
            
        if from_panel:
            # Remove from current panel
            self.panels[from_panel].remove_card(function_name)
            
        # Add to new panel
        self.panels[to_panel].add_card(card)
        
        if animate and from_panel:
            self._animate_card_movement(card, from_panel, to_panel)
            
    def move_card_backward(self, function_name: str, to_panel: str):
        """Move a card backward (for refinement)"""
        if function_name not in self.all_cards:
            return
            
        card = self.all_cards[function_name]
        card.set_status("refining")
        card.start_glow_animation()
        
        # Animate backward movement
        self.move_card(function_name, to_panel, animate=True)
        
        # Stop glow after animation
        QTimer.singleShot(1000, card.stop_glow_animation)
        
    def _animate_card_movement(self, card: FunctionCard, from_panel: str, to_panel: str):
        """Animate card movement between panels"""
        # For now, just show/hide with fade effect
        # In a full implementation, this would calculate positions and animate
        opacity_effect = QGraphicsOpacityEffect()
        card.setGraphicsEffect(opacity_effect)
        
        fade_out = QPropertyAnimation(opacity_effect, b"opacity")
        fade_out.setDuration(200)
        fade_out.setStartValue(1.0)
        fade_out.setEndValue(0.0)
        
        fade_in = QPropertyAnimation(opacity_effect, b"opacity")
        fade_in.setDuration(200)
        fade_in.setStartValue(0.0)
        fade_in.setEndValue(1.0)
        
        animation = QSequentialAnimationGroup()
        animation.addAnimation(fade_out)
        animation.addAnimation(fade_in)
        animation.finished.connect(lambda: card.setGraphicsEffect(None))
        animation.start()
        
    def _on_card_clicked(self, function_name: str):
        """Handle card clicks"""
        print(f"Card clicked: {function_name}")
        
    def handle_event(self, event: Event):
        """Handle events and update visualization"""
        if not event.function_name:
            return
            
        func_name = event.function_name
        
        # Map events to panel movements
        if event.event_type == EventType.FUNCTION_START:
            self.add_function(func_name)
            # Stay in Queue until test generation starts
            card = self.all_cards.get(func_name)
            if card:
                card.set_status("pending")
                card.set_progress(0)
            
                
        elif event.event_type == EventType.TEST_GEN_START:
            # Move to Test Generation panel when test generation starts
            self.move_card(func_name, "Test Generation")
            card = self.all_cards.get(func_name)
            if card:
                card.set_status("active")
                card.set_progress(25)
                
        elif event.event_type == EventType.TEST_GEN_COMPLETE:
            # Update progress when tests are done
            card = self.all_cards.get(func_name)
            if card:
                card.set_progress(33)
                
        elif event.event_type == EventType.MODERNIZE_START:
            self.move_card(func_name, "Modernize")
            card = self.all_cards.get(func_name)
            if card:
                card.set_status("active")
                card.set_progress(50)
                
        elif event.event_type == EventType.MODERNIZE_COMPLETE:
            # Update progress when modernization is done
            card = self.all_cards.get(func_name)
            if card:
                card.set_progress(66)
                
                
        elif event.event_type == EventType.VALIDATE_START:
            # Move to Validate when validation starts
            self.move_card(func_name, "Validate")
            card = self.all_cards.get(func_name)
            if card:
                card.set_status("active")
                card.set_progress(75)
                # Only clear pass rate on first validation, not during refinement
                if card.iteration == 0:
                    card.set_pass_rate(0.0)
                
        elif event.event_type == EventType.VALIDATE_COMPLETE:
            # Ensure card is in Validate panel before updating
            if func_name in self.all_cards:
                # Move to validate if not already there (in case events arrive out of order)
                current_panel = None
                for panel_name, panel in self.panels.items():
                    if panel.has_card(func_name):
                        current_panel = panel_name
                        break
                
                if current_panel != "Validate" and current_panel != "Output":
                    self.move_card(func_name, "Validate")
                
                card = self.all_cards.get(func_name)
                if card:
                    pass_rate = event.data.get('pass_rate', 0)
                    card.set_pass_rate(pass_rate)
                    
                    if pass_rate >= 1.0:
                        card.set_progress(100)
                    else:
                        card.set_progress(85)
                    
        elif event.event_type == EventType.REFINE_START:
            # Move backward to modernizer
            self.move_card_backward(func_name, "Modernize")
            card = self.all_cards.get(func_name)
            if card:
                iteration = event.data.get('iteration', 1)
                card.set_iteration(iteration)
                card.set_status("refining")  # Ensure status is set to refining
                # Keep the previous pass rate visible during refinement
                pass_rate = event.data.get('pass_rate', card.pass_rate)
                card.set_pass_rate(pass_rate)
                
        elif event.event_type == EventType.FUNCTION_COMPLETE:
            self.move_card(func_name, "Output")
            card = self.all_cards.get(func_name)
            if card:
                card.set_status("success")
                card.set_progress(100)
                card.set_pass_rate(1.0)
                
        elif event.event_type == EventType.FUNCTION_FAILED:
            card = self.all_cards.get(func_name)
            if card:
                card.set_status("error")
                card.set_progress(100)
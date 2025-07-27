"""
Flow diagram widget showing system architecture with animations
"""
from PyQt6.QtWidgets import QGraphicsView, QGraphicsScene, QGraphicsItem, QWidget, QGraphicsObject
from PyQt6.QtCore import (Qt, QRectF, QPointF, QTimer, QPropertyAnimation, 
                         QEasingCurve, pyqtProperty, QLineF, QObject)
from PyQt6.QtGui import (QPainter, QPen, QBrush, QColor, QFont, 
                        QRadialGradient, QPainterPath)
import math
from typing import Dict, List, Optional

from .styles import COLORS, NODE_STYLE, ANIMATION
from .event_system import EventType, Event

class FlowNode(QGraphicsObject):
    """A node in the flow diagram representing a processing stage"""
    
    def __init__(self, name: str, x: float, y: float, icon: Optional[str] = None):
        super().__init__()
        self.name = name
        self.icon = icon
        self.setPos(x, y)
        self.width = NODE_STYLE['width']
        self.height = NODE_STYLE['height']
        self.corner_radius = NODE_STYLE['corner_radius']
        
        # State
        self.state = 'idle'  # idle, active, success, error
        self._glow_opacity = 0.0
        self.active_functions = set()
        
        # Animation
        self.glow_animation = None
        
        # Enable hover events
        self.setAcceptHoverEvents(True)
        
    def boundingRect(self):
        """Return the bounding rectangle of the node"""
        return QRectF(-self.width/2, -self.height/2, self.width, self.height)
        
    def paint(self, painter: QPainter, option, widget):
        """Paint the node"""
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        # Node background with state-based color
        color_map = {
            'idle': COLORS['node_idle'],
            'active': COLORS['node_active'],
            'success': COLORS['node_success'],
            'error': COLORS['node_error'],
        }
        bg_color = QColor(color_map[self.state])
        
        # Draw glow effect when active
        if self.state == 'active' and self._glow_opacity > 0:
            glow_radius = self.width * 0.8
            gradient = QRadialGradient(0, 0, glow_radius)
            glow_color = QColor(COLORS['node_active'])
            glow_color.setAlphaF(self._glow_opacity * 0.3)
            gradient.setColorAt(0, glow_color)
            gradient.setColorAt(1, QColor(0, 0, 0, 0))
            painter.setBrush(QBrush(gradient))
            painter.setPen(Qt.PenStyle.NoPen)
            painter.drawEllipse(QPointF(0, 0), glow_radius, glow_radius)
        
        # Draw node rectangle
        painter.setBrush(QBrush(bg_color))
        painter.setPen(QPen(QColor(COLORS['node_border']), NODE_STYLE['border_width']))
        rect = self.boundingRect()
        painter.drawRoundedRect(rect, self.corner_radius, self.corner_radius)
        
        # Draw icon (placeholder for now)
        if self.icon:
            icon_rect = QRectF(-NODE_STYLE['icon_size']/2, -self.height/2 + 12,
                              NODE_STYLE['icon_size'], NODE_STYLE['icon_size'])
            painter.setPen(QPen(QColor(COLORS['text']), 2))
            painter.drawText(icon_rect, Qt.AlignmentFlag.AlignCenter, self.icon)
        
        # Draw text
        painter.setPen(QPen(QColor(COLORS['text'])))
        font = QFont()
        font.setPixelSize(NODE_STYLE['font_size'])
        font.setBold(self.state == 'active')
        painter.setFont(font)
        
        text_rect = QRectF(-self.width/2, -10, self.width, 20)
        painter.drawText(text_rect, Qt.AlignmentFlag.AlignCenter, self.name)
        
        # Draw active function count
        if self.active_functions:
            count_text = str(len(self.active_functions))
            painter.setPen(QPen(QColor(COLORS['text_dim'])))
            font.setPixelSize(10)
            painter.setFont(font)
            painter.drawText(rect, Qt.AlignmentFlag.AlignBottom | Qt.AlignmentFlag.AlignHCenter, 
                           f"({count_text} active)")
    
    def set_state(self, state: str):
        """Set the node state and trigger animations"""
        if self.state != state:
            self.state = state
            self.update()
            
            # Start glow animation for active state
            if state == 'active':
                self.start_glow_animation()
            elif self.glow_animation:
                self.glow_animation.stop()
                self._glow_opacity = 0.0
                
    def start_glow_animation(self):
        """Start the glow pulse animation"""
        if not self.glow_animation:
            self.glow_animation = QPropertyAnimation(self, b"glow_opacity")
            self.glow_animation.setDuration(ANIMATION['glow_duration'])
            self.glow_animation.setStartValue(0.0)
            self.glow_animation.setEndValue(1.0)
            self.glow_animation.setLoopCount(-1)  # Infinite
            self.glow_animation.setEasingCurve(QEasingCurve.Type.InOutSine)
        self.glow_animation.start()
        
    @pyqtProperty(float)
    def glow_opacity(self):
        return self._glow_opacity
        
    @glow_opacity.setter
    def glow_opacity(self, value):
        self._glow_opacity = value
        self.update()
        
    def add_active_function(self, func_name: str):
        """Add a function to the active set"""
        self.active_functions.add(func_name)
        self.update()
        
    def remove_active_function(self, func_name: str):
        """Remove a function from the active set"""
        self.active_functions.discard(func_name)
        self.update()

class FlowConnection(QGraphicsItem):
    """An animated connection between nodes"""
    
    def __init__(self, start_node: FlowNode, end_node: FlowNode):
        super().__init__()
        self.start_node = start_node
        self.end_node = end_node
        self.particles = []
        self.active = False
        
        # Particle animation timer
        self.particle_timer = QTimer()
        self.particle_timer.timeout.connect(self.update_particles)
        self.particle_timer.start(30)  # 30ms updates
        
        self.last_particle_time = 0
        self.particle_spawn_interval = 500  # ms between particles
        
    def boundingRect(self):
        """Return bounding rect that encompasses both nodes"""
        start = self.start_node.pos()
        end = self.end_node.pos()
        
        x1, y1 = min(start.x(), end.x()), min(start.y(), end.y())
        x2, y2 = max(start.x(), end.x()), max(start.y(), end.y())
        
        # Add padding for particles and line width
        padding = 20
        return QRectF(x1 - padding, y1 - padding, 
                     x2 - x1 + 2*padding, y2 - y1 + 2*padding)
        
    def paint(self, painter: QPainter, option, widget):
        """Paint the connection line and particles"""
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        # Calculate connection points (edge of nodes)
        start_pos = self.start_node.pos()
        end_pos = self.end_node.pos()
        
        # Calculate angle and edge points
        angle = math.atan2(end_pos.y() - start_pos.y(), 
                          end_pos.x() - start_pos.x())
        
        # Offset from node centers to edges
        start_offset = QPointF(self.start_node.width/2 * math.cos(angle),
                              self.start_node.height/2 * math.sin(angle))
        end_offset = QPointF(self.end_node.width/2 * math.cos(angle + math.pi),
                            self.end_node.height/2 * math.sin(angle + math.pi))
        
        start_point = start_pos + start_offset
        end_point = end_pos + end_offset
        
        # Draw connection line
        line_color = QColor(COLORS['connection_active'] if self.active 
                           else COLORS['connection'])
        painter.setPen(QPen(line_color, 2, Qt.PenStyle.DashLine))
        painter.drawLine(start_point, end_point)
        
        # Draw arrow head
        arrow_length = 12
        arrow_angle = 0.5  # radians
        
        arrow_p1 = end_point + QPointF(
            arrow_length * math.cos(angle + math.pi - arrow_angle),
            arrow_length * math.sin(angle + math.pi - arrow_angle)
        )
        arrow_p2 = end_point + QPointF(
            arrow_length * math.cos(angle + math.pi + arrow_angle),
            arrow_length * math.sin(angle + math.pi + arrow_angle)
        )
        
        painter.setPen(QPen(line_color, 2, Qt.PenStyle.SolidLine))
        painter.drawLine(end_point, arrow_p1)
        painter.drawLine(end_point, arrow_p2)
        
        # Draw particles
        if self.active:
            painter.setPen(Qt.PenStyle.NoPen)
            for particle_pos in self.particles:
                # Calculate particle position on line
                t = particle_pos  # 0.0 to 1.0
                particle_point = QPointF(
                    start_point.x() + t * (end_point.x() - start_point.x()),
                    start_point.y() + t * (end_point.y() - start_point.y())
                )
                
                # Draw particle with glow
                gradient = QRadialGradient(particle_point, ANIMATION['particle_size'] * 2)
                particle_color = QColor(COLORS['particle'])
                gradient.setColorAt(0, particle_color)
                particle_color.setAlphaF(0.0)
                gradient.setColorAt(1, particle_color)
                
                painter.setBrush(QBrush(gradient))
                painter.drawEllipse(particle_point, 
                                  ANIMATION['particle_size'],
                                  ANIMATION['particle_size'])
                
    def set_active(self, active: bool):
        """Set connection active state"""
        self.active = active
        if not active:
            self.particles.clear()
        self.update()
        
    def update_particles(self):
        """Update particle positions"""
        if not self.active:
            return
            
        current_time = self.particle_timer.interval() * len(self.particles)
        
        # Spawn new particle
        if current_time - self.last_particle_time >= self.particle_spawn_interval:
            self.particles.append(0.0)
            self.last_particle_time = current_time
            
        # Update particle positions
        speed = ANIMATION['particle_speed'] / 1000.0 * self.particle_timer.interval()
        line_length = (self.end_node.pos() - self.start_node.pos()).manhattanLength()
        if line_length > 0:
            speed_normalized = speed / line_length
            
            # Move particles
            self.particles = [p + speed_normalized for p in self.particles]
            
            # Remove particles that reached the end
            self.particles = [p for p in self.particles if p <= 1.0]
            
        self.update()

class FlowDiagramWidget(QGraphicsView):
    """Widget displaying the system architecture flow diagram"""
    
    def __init__(self):
        super().__init__()
        self.scene = QGraphicsScene()
        self.setScene(self.scene)
        
        # Nodes
        self.nodes: Dict[str, FlowNode] = {}
        self.connections: List[FlowConnection] = []
        self.function_states: Dict[str, str] = {}  # Track which stage each function is in
        
        self.setup_ui()
        self.create_nodes()
        self.create_connections()
        
    def setup_ui(self):
        """Configure the view"""
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.setBackgroundBrush(QBrush(QColor(COLORS['background'])))
        self.setFrameStyle(0)  # No frame
        
        # Set scene rect
        self.scene.setSceneRect(-400, -200, 800, 400)
        
    def create_nodes(self):
        """Create the flow diagram nodes"""
        # Define node positions and properties
        node_data = [
            ("Source\n(F77)", -300, 0, "📄"),
            ("Test\nGenerator", -150, -100, "🧪"),
            ("Modernizer", 0, 0, "🔄"),
            ("Compiler", 150, 0, "⚙️"),
            ("Validator", 300, -50, "✓"),
            ("Output\n(F90)", 450, 0, "📦"),
        ]
        
        for name, x, y, icon in node_data:
            node = FlowNode(name, x, y, icon)
            self.scene.addItem(node)
            self.nodes[name.split('\n')[0]] = node
            
        # Add refinement loop indicator
        refine_node = FlowNode("Refine", 150, 100, "🔁")
        refine_node.width = 100
        refine_node.height = 50
        self.scene.addItem(refine_node)
        self.nodes["Refine"] = refine_node
        
    def create_connections(self):
        """Create connections between nodes"""
        # Main flow
        connections = [
            ("Source", "Test"),
            ("Source", "Modernizer"),
            ("Test", "Modernizer"),
            ("Modernizer", "Compiler"),
            ("Compiler", "Validator"),
            ("Validator", "Output"),
            # Refinement loop
            ("Validator", "Refine"),
            ("Refine", "Modernizer"),
        ]
        
        for start_name, end_name in connections:
            start_node = self.nodes[start_name]
            end_node = self.nodes[end_name]
            connection = FlowConnection(start_node, end_node)
            self.scene.addItem(connection)
            self.connections.append(connection)
            
    def handle_event(self, event: Event):
        """Handle events and update visualization"""
        if event.function_name:
            func_name = event.function_name
            
            # Map events to nodes
            node_mapping = {
                EventType.SOURCE_READ: "Source",
                EventType.TEST_GEN_START: "Test",
                EventType.TEST_GEN_COMPLETE: "Test",
                EventType.MODERNIZE_START: "Modernizer",
                EventType.MODERNIZE_COMPLETE: "Modernizer",
                EventType.COMPILE_START: "Compiler",
                EventType.COMPILE_SUCCESS: "Compiler",
                EventType.COMPILE_FAILED: "Compiler",
                EventType.VALIDATE_START: "Validator",
                EventType.VALIDATE_COMPLETE: "Validator",
                EventType.REFINE_START: "Refine",
                EventType.REFINE_COMPLETE: "Refine",
            }
            
            if event.event_type in node_mapping:
                node_name = node_mapping[event.event_type]
                node = self.nodes.get(node_name)
                
                if node:
                    # Update node state
                    if event.event_type in [EventType.SOURCE_READ, EventType.TEST_GEN_START,
                                          EventType.MODERNIZE_START, EventType.COMPILE_START,
                                          EventType.VALIDATE_START, EventType.REFINE_START]:
                        node.add_active_function(func_name)
                        node.set_state('active')
                        self.function_states[func_name] = node_name
                        
                        # Activate relevant connections
                        self.update_connections()
                        
                    elif event.event_type in [EventType.TEST_GEN_COMPLETE, EventType.MODERNIZE_COMPLETE,
                                            EventType.COMPILE_SUCCESS, EventType.VALIDATE_COMPLETE,
                                            EventType.REFINE_COMPLETE]:
                        node.remove_active_function(func_name)
                        if not node.active_functions:
                            node.set_state('idle')
                        
            # Handle function completion
            if event.event_type == EventType.FUNCTION_COMPLETE:
                self.nodes["Output"].set_state('success')
                # Reset after a delay
                QTimer.singleShot(2000, lambda: self.nodes["Output"].set_state('idle'))
                
            elif event.event_type == EventType.FUNCTION_FAILED:
                # Find the last active node for this function
                if func_name in self.function_states:
                    node_name = self.function_states[func_name]
                    if node_name in self.nodes:
                        self.nodes[node_name].set_state('error')
                        QTimer.singleShot(2000, lambda: self.nodes[node_name].set_state('idle'))
                        
    def update_connections(self):
        """Update connection states based on active nodes"""
        # This is simplified - in a real implementation you'd track
        # which specific connections are active based on the flow
        for connection in self.connections:
            start_active = connection.start_node.state == 'active'
            end_active = connection.end_node.state == 'active'
            connection.set_active(start_active or end_active)
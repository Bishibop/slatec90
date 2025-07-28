"""
Main window for SLATEC modernization visualization
"""
import sys
from PyQt6.QtWidgets import (QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
                            QSplitter, QGroupBox, QLabel, QListWidget, 
                            QTextEdit, QProgressBar, QTabWidget, QGridLayout,
                            QListWidgetItem, QPushButton, QSizePolicy)
from PyQt6.QtCore import Qt, QTimer, pyqtSignal, QThread
from PyQt6.QtGui import QFont, QIcon
from datetime import datetime
import queue

from .styles import DARK_THEME, COLORS
from .panel_view import PipelineView
from .event_system import EventType, Event, event_emitter

class EventListenerThread(QThread):
    """Thread to listen for events from the orchestrator"""
    event_received = pyqtSignal(Event)
    
    def __init__(self):
        super().__init__()
        self.running = True
        
    def run(self):
        """Listen for events and emit Qt signals"""
        event_queue = event_emitter.get_queue()
        while self.running:
            try:
                event = event_queue.get(timeout=0.1)
                self.event_received.emit(event)
            except queue.Empty:
                continue
                
    def stop(self):
        self.running = False


class MetricsWidget(QWidget):
    """Widget showing overall metrics"""
    def __init__(self):
        super().__init__()
        self.init_ui()
        
    def init_ui(self):
        layout = QGridLayout()
        
        # Metrics
        self.metrics = {
            'total': self._create_metric("Total Functions", "0"),
            'completed': self._create_metric("Completed", "0", COLORS['success']),
            'failed': self._create_metric("Failed", "0", COLORS['error']),
            'active': self._create_metric("Active", "0", COLORS['accent']),
            'success_rate': self._create_metric("Success Rate", "0%"),
            'avg_time': self._create_metric("Avg Time", "0s"),
        }
        
        # Arrange in grid
        positions = [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2)]
        for (key, widgets), (row, col) in zip(self.metrics.items(), positions):
            layout.addWidget(widgets[0], row*2, col)
            layout.addWidget(widgets[1], row*2+1, col)
            
        self.setLayout(layout)
        
    def _create_metric(self, label, value, color=None):
        """Create a metric label pair"""
        label_widget = QLabel(label)
        label_widget.setAlignment(Qt.AlignmentFlag.AlignCenter)
        label_widget.setStyleSheet("color: #a0a0a0; font-size: 11px;")
        
        value_widget = QLabel(value)
        value_widget.setAlignment(Qt.AlignmentFlag.AlignCenter)
        value_widget.setStyleSheet(f"font-size: 18px; font-weight: bold; color: {color or '#e0e0e0'};")
        
        return (label_widget, value_widget)
        
    def update_metric(self, key, value):
        """Update a metric value"""
        if key in self.metrics:
            self.metrics[key][1].setText(str(value))

class SLATECVisualizationWindow(QMainWindow):
    """Main window for SLATEC modernization visualization"""
    
    def __init__(self):
        super().__init__()
        self.start_time = datetime.now()
        self.stats = {
            'total': 0,
            'completed': 0,
            'failed': 0,
            'active': 0,
        }
        self.init_ui()
        self.setup_event_listener()
        
    def init_ui(self):
        """Initialize the user interface"""
        self.setWindowTitle("SLATEC Modernization Visualization")
        self.setGeometry(100, 100, 1400, 900)
        self.setStyleSheet(DARK_THEME)
        
        # Central widget
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        
        # Main layout
        main_layout = QVBoxLayout()
        central_widget.setLayout(main_layout)
        
        # Top section: Metrics
        metrics_group = QGroupBox("Overall Metrics")
        metrics_layout = QVBoxLayout()
        self.metrics_widget = MetricsWidget()
        metrics_layout.addWidget(self.metrics_widget)
        metrics_group.setLayout(metrics_layout)
        main_layout.addWidget(metrics_group)
        
        # Middle section: Pipeline view
        pipeline_group = QGroupBox("Modernization Pipeline")
        pipeline_layout = QVBoxLayout()
        self.pipeline_view = PipelineView()
        pipeline_layout.addWidget(self.pipeline_view)
        pipeline_group.setLayout(pipeline_layout)
        main_layout.addWidget(pipeline_group)
        
        # Bottom section: Event log
        logs_group = QGroupBox("Event Log")
        logs_layout = QVBoxLayout()
        logs_layout.setContentsMargins(5, 5, 5, 5)  # Reduce internal margins
        self.log_viewer = QTextEdit()
        self.log_viewer.setReadOnly(True)
        self.log_viewer.setMinimumHeight(150)  # Changed from maximum to minimum
        self.log_viewer.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        logs_layout.addWidget(self.log_viewer)
        logs_group.setLayout(logs_layout)
        logs_group.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Expanding)
        main_layout.addWidget(logs_group)
        
        # Status bar
        self.statusBar().showMessage("Ready to visualize modernization process")
        
        # Update timer
        self.update_timer = QTimer()
        self.update_timer.timeout.connect(self.update_time_metrics)
        self.update_timer.start(1000)  # Update every second
        
    def setup_event_listener(self):
        """Set up the event listener thread"""
        self.event_thread = EventListenerThread()
        self.event_thread.event_received.connect(self.handle_event)
        self.event_thread.start()
        
    def handle_event(self, event: Event):
        """Handle events from the orchestrator"""
        # Log the event
        timestamp = event.timestamp.strftime("%H:%M:%S")
        self.log_viewer.append(f"[{timestamp}] {event.event_type.name}: {event.function_name or 'System'}")
        
        # Update pipeline view
        self.pipeline_view.handle_event(event)
        
        # Handle specific event types
        if event.event_type == EventType.RUN_START:
            self.stats['total'] = event.data.get('total_functions', 0)
            self.metrics_widget.update_metric('total', self.stats['total'])
            self.statusBar().showMessage(f"Starting run with {self.stats['total']} functions")
            
        elif event.event_type == EventType.FUNCTION_START:
            self.stats['active'] += 1
            self.update_metrics()
            
        elif event.event_type == EventType.FUNCTION_COMPLETE:
            self.stats['completed'] += 1
            self.stats['active'] -= 1
            self.update_metrics()
            
        elif event.event_type == EventType.FUNCTION_FAILED:
            self.stats['failed'] += 1
            self.stats['active'] -= 1
            self.update_metrics()
            
    def update_metrics(self):
        """Update overall metrics"""
        self.metrics_widget.update_metric('completed', self.stats['completed'])
        self.metrics_widget.update_metric('failed', self.stats['failed'])
        self.metrics_widget.update_metric('active', self.stats['active'])
        
        # Calculate success rate
        total_processed = self.stats['completed'] + self.stats['failed']
        if total_processed > 0:
            success_rate = self.stats['completed'] / total_processed
            self.metrics_widget.update_metric('success_rate', f"{success_rate:.1%}")
            
    def update_time_metrics(self):
        """Update time-based metrics"""
        if self.stats['completed'] > 0:
            elapsed = (datetime.now() - self.start_time).total_seconds()
            avg_time = elapsed / self.stats['completed']
            self.metrics_widget.update_metric('avg_time', f"{avg_time:.1f}s")
            
    def closeEvent(self, event):
        """Clean up when closing"""
        self.event_thread.stop()
        self.event_thread.wait()
        event.accept()
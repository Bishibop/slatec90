#!/usr/bin/env python3
"""
Manual demo script for SLATEC visualization - controlled by buttons
Click buttons to step through the process
"""
import sys
from PyQt6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout, 
                           QHBoxLayout, QPushButton, QLabel, QSplitter)
from PyQt6.QtCore import Qt

from visualization import SLATECVisualizationWindow, EventType, event_emitter, Event


class ManualDemoController:
    """Controls the demo flow step by step"""
    
    def __init__(self):
        self.current_step = 0
        self.function_name = "PYTHAG"
        self.test_count = 75
        self.pass_rates = [0.73, 0.89, 1.0]  # Initial, iteration 1, iteration 2
        self.current_iteration = 0
        
        # Define the sequence of steps with descriptions
        self.steps = [
            ("Add PYTHAG to Queue", self.start_function),
            ("Start Test Generation", self.start_test_generation),
            ("Complete Test Generation (75 tests)", self.complete_test_generation),
            ("Start Modernization", self.start_modernization),
            ("Complete Modernization", self.complete_modernization),
            ("Start Validation", self.start_validation),
            ("Complete Validation (73% pass)", self.complete_validation),
            ("Start Refinement #1", self.start_refinement_1),
            ("Complete Modernization #1", self.complete_modernization_1),
            ("Start Validation #1", self.start_validation_1),
            ("Complete Validation #1 (89% pass)", self.complete_validation_1),
            ("Start Refinement #2", self.start_refinement_2),
            ("Complete Modernization #2", self.complete_modernization_2),
            ("Start Validation #2", self.start_validation_2),
            ("Complete Validation #2 (100% pass)", self.complete_validation_2),
            ("Move to Output (Success)", self.complete_function)
        ]
        
    def get_current_step_description(self):
        """Get description of current step"""
        if self.current_step < len(self.steps):
            return self.steps[self.current_step][0]
        return "Demo complete!"
        
    def next_step(self):
        """Execute the next step in the sequence"""
        if self.current_step < len(self.steps):
            description, action = self.steps[self.current_step]
            action()
            self.current_step += 1
            return True
        return False
    
    def reset(self):
        """Reset the demo"""
        self.current_step = 0
        self.current_iteration = 0
    
    # Step implementations
    def start_function(self):
        """Add function to queue"""
        event_emitter.emit(EventType.FUNCTION_START, self.function_name)
        
    def start_test_generation(self):
        """Move to test generation"""
        event_emitter.emit(EventType.TEST_GEN_START, self.function_name)
        
    def complete_test_generation(self):
        """Complete test generation"""
        event_emitter.emit(EventType.TEST_GEN_COMPLETE, self.function_name, 
                         test_count=self.test_count)
        
    def start_modernization(self):
        """Start modernization"""
        event_emitter.emit(EventType.MODERNIZE_START, self.function_name)
        
    def complete_modernization(self):
        """Complete modernization"""
        event_emitter.emit(EventType.MODERNIZE_COMPLETE, self.function_name)
        
    def start_validation(self):
        """Start validation"""
        event_emitter.emit(EventType.VALIDATE_START, self.function_name)
        
    def complete_validation(self):
        """Complete initial validation with low pass rate"""
        event_emitter.emit(EventType.VALIDATE_COMPLETE, self.function_name,
                         pass_rate=self.pass_rates[0],
                         passed_tests=int(self.test_count * self.pass_rates[0]),
                         total_tests=self.test_count)
        
    def start_refinement_1(self):
        """Start first refinement iteration"""
        self.current_iteration = 1
        event_emitter.emit(EventType.REFINE_START, self.function_name, 
                         iteration=1, pass_rate=self.pass_rates[0])
        
    def complete_modernization_1(self):
        """Complete first refinement modernization"""
        event_emitter.emit(EventType.MODERNIZE_COMPLETE, self.function_name)
        
    def start_validation_1(self):
        """Start validation after first refinement"""
        event_emitter.emit(EventType.VALIDATE_START, self.function_name)
        
    def complete_validation_1(self):
        """Complete validation with improved pass rate"""
        event_emitter.emit(EventType.VALIDATE_COMPLETE, self.function_name,
                         pass_rate=self.pass_rates[1],
                         passed_tests=int(self.test_count * self.pass_rates[1]),
                         total_tests=self.test_count)
        
    def start_refinement_2(self):
        """Start second refinement iteration"""
        self.current_iteration = 2
        event_emitter.emit(EventType.REFINE_START, self.function_name, 
                         iteration=2, pass_rate=self.pass_rates[1])
        
    def complete_modernization_2(self):
        """Complete second refinement modernization"""
        event_emitter.emit(EventType.MODERNIZE_COMPLETE, self.function_name)
        
    def start_validation_2(self):
        """Start final validation"""
        event_emitter.emit(EventType.VALIDATE_START, self.function_name)
        
    def complete_validation_2(self):
        """Complete validation with 100% pass rate"""
        event_emitter.emit(EventType.VALIDATE_COMPLETE, self.function_name,
                         pass_rate=self.pass_rates[2],
                         passed_tests=self.test_count,
                         total_tests=self.test_count)
        
    def complete_function(self):
        """Move function to output"""
        event_emitter.emit(EventType.FUNCTION_COMPLETE, self.function_name, 
                         status='success')


class ManualDemoWindow(QMainWindow):
    """Window with button controls for the demo"""
    
    def __init__(self):
        super().__init__()
        self.controller = ManualDemoController()
        self.init_ui()
        
        # Start event emitter
        event_emitter.start()
        
        # Emit initial run start
        event_emitter.emit(EventType.RUN_START, data={
            'total_functions': 1,
            'parallel': False,
            'workers': 1
        })
        
    def init_ui(self):
        """Initialize the user interface"""
        self.setWindowTitle("SLATEC Modernization Visualization - Manual Demo")
        self.setGeometry(100, 100, 1500, 1000)
        
        # Central widget
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        
        # Main layout
        main_layout = QVBoxLayout()
        central_widget.setLayout(main_layout)
        
        # Create visualization window
        self.viz_window = SLATECVisualizationWindow()
        
        # Control panel
        control_panel = QWidget()
        control_layout = QHBoxLayout()
        control_panel.setLayout(control_layout)
        
        # Step label
        self.step_label = QLabel(self.controller.get_current_step_description())
        self.step_label.setStyleSheet("font-size: 14px; font-weight: bold; padding: 10px;")
        control_layout.addWidget(self.step_label)
        
        control_layout.addStretch()
        
        # Buttons
        self.prev_button = QPushButton("← Restart")
        self.prev_button.clicked.connect(self.restart_demo)
        control_layout.addWidget(self.prev_button)
        
        self.next_button = QPushButton("Next Step →")
        self.next_button.clicked.connect(self.next_step)
        self.next_button.setStyleSheet("QPushButton { background-color: #4CAF50; color: white; font-weight: bold; padding: 10px 20px; }")
        control_layout.addWidget(self.next_button)
        
        # Add control panel and visualization
        main_layout.addWidget(control_panel)
        
        # Add the visualization window's central widget
        main_layout.addWidget(self.viz_window.centralWidget())
        
        # Copy references to visualization components
        self.pipeline_view = self.viz_window.pipeline_view
        self.metrics_widget = self.viz_window.metrics_widget
        self.log_viewer = self.viz_window.log_viewer
        
        # Update initial state
        self.update_ui()
        
    def next_step(self):
        """Execute next step"""
        if self.controller.next_step():
            self.update_ui()
        else:
            self.next_button.setEnabled(False)
            self.next_button.setText("Demo Complete")
            
    def restart_demo(self):
        """Restart the demo"""
        # Clear visualization
        for panel in self.pipeline_view.panels.values():
            for card in list(panel.cards.values()):
                panel.remove_card(card.function_name)
        self.pipeline_view.all_cards.clear()
        
        # Reset controller
        self.controller.reset()
        
        # Update UI
        self.next_button.setEnabled(True)
        self.next_button.setText("Next Step →")
        self.update_ui()
        
    def update_ui(self):
        """Update UI elements"""
        self.step_label.setText(self.controller.get_current_step_description())
        
        # Show step progress
        current = self.controller.current_step + 1
        total = len(self.controller.steps)
        if current <= total:
            self.statusBar().showMessage(f"Step {current}/{total}")
        else:
            self.statusBar().showMessage("Demo complete!")


def main():
    """Main entry point"""
    app = QApplication(sys.argv)
    app.setStyle('Fusion')
    
    # Create and show window
    window = ManualDemoWindow()
    window.show()
    
    print("Manual Demo with Buttons")
    print("Click 'Next Step' to advance through the demo")
    print("Click 'Restart' to start over")
    
    sys.exit(app.exec())


if __name__ == '__main__':
    main()
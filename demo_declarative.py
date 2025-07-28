#!/usr/bin/env python3
"""
Declarative manual demo - directly control card position and state
"""
import sys
from PyQt6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout, 
                           QHBoxLayout, QPushButton, QLabel, QSplitter)
from PyQt6.QtCore import Qt

from visualization.panel_view import PipelineView, FunctionCard
from visualization.main_window import MetricsWidget
from visualization.styles import DARK_THEME
from visualization.subsystem_panel import SubsystemDetailPanel


class DemoState:
    """Represents a state in the demo"""
    def __init__(self, panel, status, progress, pass_rate=0.0, iteration=0, description="", subsystem_data=None):
        self.panel = panel
        self.status = status
        self.progress = progress
        self.pass_rate = pass_rate
        self.iteration = iteration
        self.description = description
        self.subsystem_data = subsystem_data or {}


class DeclarativeDemoController:
    """Controls demo using declarative states"""
    
    def __init__(self, pipeline_view, subsystem_panel=None):
        self.pipeline_view = pipeline_view
        self.subsystem_panel = subsystem_panel
        self.function_name = "PYTHAG"
        self.current_step = 0
        
        # Define all states following move-then-stay pattern
        self.states = [
            # Start in queue
            DemoState("Queue", "pending", 0, 0.0, 0, "PYTHAG function in queue"),
            
            # Move to Test Generation -> then show results
            DemoState("Test Generation", "active", 25, 0.0, 0, "Generating tests"),
            DemoState("Test Generation", "success", 33, 0.0, 0, "130 tests generated"),
            
            # Move to Modernize -> then show completion
            DemoState("Modernization", "active", 50, 0.0, 0, "Generating modern function"),
            DemoState("Modernization", "success", 66, 0.0, 0, "Modern function generated"),
            
            # Move to Validation -> then show results
            DemoState("Validation", "active", 75, 0.0, 0, "Validating"),
            DemoState("Validation", "active", 85, 0.73, 0, "73 test cases pass (73%)"),
            
            # Move back to Modernize for refinement -> show completion
            DemoState("Modernization", "refining", 50, 0.73, 1, "Refining function (iteration 1)"),
            DemoState("Modernization", "success", 66, 0.73, 1, "Refined function generated"),
            
            # Move back to Validation -> show improved results
            DemoState("Validation", "active", 75, 0.73, 1, "Re-validating"),
            DemoState("Validation", "active", 85, 0.89, 1, "116 test cases pass (89%)"),
            
            # Move back to Modernize for final refinement -> show completion
            DemoState("Modernization", "refining", 50, 0.89, 2, "Refining function (iteration 2)"),
            DemoState("Modernization", "success", 66, 0.89, 2, "Final function generated"),
            
            # Move back to Validation -> show perfect results
            DemoState("Validation", "active", 75, 0.89, 2, "Validating"),
            DemoState("Validation", "success", 100, 1.0, 2, "130 test cases pass (100%)"),
            
            # Move to Output
            DemoState("Output", "success", 100, 1.0, 0, "PYTHAG successfully modernized"),
        ]
        
        # Create the card
        self.card = None
        
    def get_current_description(self):
        """Get description of current state"""
        if self.current_step < len(self.states):
            return self.states[self.current_step].description
        return "Demo complete!"
        
    def apply_current_state(self):
        """Apply the current state to the visualization"""
        if self.current_step >= len(self.states):
            return False
            
        state = self.states[self.current_step]
        
        # Create card if it doesn't exist
        if self.card is None:
            self.card = FunctionCard(self.function_name)
            self.pipeline_view.all_cards[self.function_name] = self.card
            
        # Find current panel containing the card
        current_panel = None
        for panel_name, panel in self.pipeline_view.panels.items():
            if panel.has_card(self.function_name):
                current_panel = panel_name
                break
                
        # Move card if needed
        if current_panel != state.panel:
            if current_panel:
                self.pipeline_view.panels[current_panel].remove_card(self.function_name)
            self.pipeline_view.panels[state.panel].add_card(self.card)
            
        # Update card state
        self.card.set_status(state.status)
        self.card.set_progress(state.progress)
        self.card.set_pass_rate(state.pass_rate)
        self.card.set_iteration(state.iteration)
        
        # Update subsystem panel if available
        if self.subsystem_panel:
            # Determine subsystem stage from panel name
            stage = state.panel.lower()
            if stage == "queue":
                stage = "queue"
            elif "test" in stage:
                stage = "test generation"
            elif "modern" in stage:
                stage = "modernize"
            elif "valid" in stage:
                stage = "validate"
            elif "output" in stage:
                stage = "output"
            else:
                stage = "queue"
                
            # Create subsystem data based on state
            subsystem_data = state.subsystem_data.copy()
            subsystem_data.update({
                'function_name': self.function_name,
                'completed': state.status in ['success'],
                'pass_rate': state.pass_rate,
                'iteration': state.iteration,
                'progress': state.progress,
                'test_count': '130'
            })
            
            self.subsystem_panel.set_stage(stage, subsystem_data)
        
        return True
        
    def next_step(self):
        """Move to next step"""
        if self.current_step < len(self.states) - 1:
            self.current_step += 1
            self.apply_current_state()
            return True
        return False
        
    def reset(self):
        """Reset demo"""
        # Remove card from all panels
        if self.card:
            for panel in self.pipeline_view.panels.values():
                if panel.has_card(self.function_name):
                    panel.remove_card(self.function_name)
                    
        # Clear and reset
        self.pipeline_view.all_cards.clear()
        self.card = None
        self.current_step = 0


class DeclarativeDemoWindow(QMainWindow):
    """Simple window with declarative demo control"""
    
    def __init__(self):
        super().__init__()
        self.init_ui()
        
    def init_ui(self):
        """Initialize UI"""
        self.setWindowTitle("SLATEC Modernization - Declarative Demo")
        self.setGeometry(100, 100, 1400, 900)
        self.setStyleSheet(DARK_THEME)
        
        # Central widget
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        
        # Main layout
        main_layout = QVBoxLayout()
        central_widget.setLayout(main_layout)
        
        # Control panel
        control_panel = QWidget()
        control_layout = QHBoxLayout()
        control_panel.setLayout(control_layout)
        
        # Step label
        self.step_label = QLabel("")
        self.step_label.setStyleSheet("font-size: 16px; font-weight: bold; padding: 10px;")
        control_layout.addWidget(self.step_label)
        
        control_layout.addStretch()
        
        # Buttons
        self.restart_button = QPushButton("Restart")
        self.restart_button.clicked.connect(self.restart_demo)
        self.restart_button.setStyleSheet("QPushButton { padding: 10px 20px; }")
        control_layout.addWidget(self.restart_button)
        
        self.next_button = QPushButton("Next Step →")
        self.next_button.clicked.connect(self.next_step)
        self.next_button.setStyleSheet("""
            QPushButton { 
                background-color: #4CAF50; 
                color: white; 
                font-weight: bold; 
                padding: 10px 30px;
                font-size: 14px;
            }
            QPushButton:disabled {
                background-color: #666;
            }
        """)
        control_layout.addWidget(self.next_button)
        
        # Add control panel
        main_layout.addWidget(control_panel)
        
        # Create horizontal splitter for main content
        content_splitter = QSplitter(Qt.Orientation.Vertical)
        
        # Upper section: Pipeline view
        self.pipeline_view = PipelineView()
        content_splitter.addWidget(self.pipeline_view)
        
        # Lower section: Subsystem detail panel
        self.subsystem_panel = SubsystemDetailPanel()
        content_splitter.addWidget(self.subsystem_panel)
        
        # Set splitter sizes (45% pipeline, 55% subsystem)
        content_splitter.setSizes([450, 550])
        content_splitter.setStretchFactor(0, 1)  # Pipeline stretches
        content_splitter.setStretchFactor(1, 0)  # Subsystem fixed-ish
        
        # Style the splitter
        content_splitter.setStyleSheet("""
            QSplitter::handle {
                background-color: #555555;
                height: 3px;
            }
            QSplitter::handle:hover {
                background-color: #777777;
            }
        """)
        
        main_layout.addWidget(content_splitter)
        
        # Create controller with both views
        self.controller = DeclarativeDemoController(self.pipeline_view, self.subsystem_panel)
        
        # Apply initial state to sync display with description
        self.controller.apply_current_state()
        
        # Update initial state
        self.update_ui()
        
    def next_step(self):
        """Execute next step"""
        if self.controller.next_step():
            self.update_ui()
            
            # Check if we're at the end
            if self.controller.current_step >= len(self.controller.states) - 1:
                self.next_button.setEnabled(False)
                self.next_button.setText("Demo Complete")
        
    def restart_demo(self):
        """Restart the demo"""
        self.controller.reset()
        self.controller.apply_current_state()  # Apply initial state
        self.next_button.setEnabled(True)
        self.next_button.setText("Next Step →")
        self.update_ui()
        
    def update_ui(self):
        """Update UI elements"""
        self.step_label.setText(self.controller.get_current_description())
        
        # Show progress
        current = self.controller.current_step + 1
        total = len(self.controller.states)
        if current <= total:
            self.statusBar().showMessage(f"Step {current}/{total}")
        else:
            self.statusBar().showMessage("Demo complete!")
            
    def keyPressEvent(self, event):
        """Handle keyboard shortcuts"""
        if event.key() == Qt.Key.Key_Right:
            self.next_step()
        elif event.key() == Qt.Key.Key_R:
            self.restart_demo()
        elif event.key() == Qt.Key.Key_Escape:
            self.close()


def main():
    """Main entry point"""
    app = QApplication(sys.argv)
    app.setStyle('Fusion')
    
    # Create and show window
    window = DeclarativeDemoWindow()
    window.show()
    
    print("Declarative Demo Controls:")
    print("- Click 'Next Step' or press → to advance")
    print("- Click 'Restart' or press R to restart") 
    print("- Press ESC to exit")
    print()
    print("This demo directly controls card position and state")
    print("for reliable, predictable visualization.")
    
    sys.exit(app.exec())


if __name__ == '__main__':
    main()
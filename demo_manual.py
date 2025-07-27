#!/usr/bin/env python3
"""
Manual demo script for SLATEC visualization - controlled by keyboard
Use arrow keys to step through the process
"""
import sys
from PyQt6.QtWidgets import QApplication, QMainWindow, QLabel
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QKeyEvent

from visualization import SLATECVisualizationWindow, EventType, event_emitter, Event


class ManualDemoController:
    """Controls the demo flow step by step"""
    
    def __init__(self):
        self.current_step = 0
        self.function_name = "PYTHAG"
        self.test_count = 75
        self.pass_rates = [0.73, 0.89, 1.0]  # Initial, iteration 1, iteration 2
        self.current_iteration = 0
        
        # Define the sequence of steps
        self.steps = [
            self.start_function,
            self.start_test_generation,
            self.complete_test_generation,
            self.start_modernization,
            self.complete_modernization,
            self.start_validation,
            self.complete_validation,
            self.start_refinement_1,
            self.complete_modernization_1,
            self.start_validation_1,
            self.complete_validation_1,
            self.start_refinement_2,
            self.complete_modernization_2,
            self.start_validation_2,
            self.complete_validation_2,
            self.complete_function
        ]
        
    def next_step(self):
        """Execute the next step in the sequence"""
        if self.current_step < len(self.steps):
            self.steps[self.current_step]()
            self.current_step += 1
            return True
        return False
    
    def previous_step(self):
        """Go back one step (limited functionality)"""
        # For simplicity, just restart the demo
        if self.current_step > 0:
            self.current_step = 0
            self.current_iteration = 0
            return True
        return False
    
    def get_status(self):
        """Get current status for display"""
        if self.current_step >= len(self.steps):
            return "Demo complete! Press R to restart"
        return f"Step {self.current_step + 1}/{len(self.steps)} - Press → to continue"
    
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


class ManualDemoWindow(SLATECVisualizationWindow):
    """Extended window with keyboard controls"""
    
    def __init__(self):
        super().__init__()
        self.controller = ManualDemoController()
        self.setWindowTitle("SLATEC Modernization Visualization - Manual Demo")
        
        # Add instructions
        self.statusBar().showMessage(self.controller.get_status())
        
        # Make sure window can receive keyboard events
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setFocus()
        
        # Start event emitter
        event_emitter.start()
        
        # Emit initial run start
        event_emitter.emit(EventType.RUN_START, data={
            'total_functions': 1,
            'parallel': False,
            'workers': 1
        })
        
    def keyPressEvent(self, event: QKeyEvent):
        """Handle keyboard input"""
        key = event.key()
        print(f"Key pressed: {key}")  # Debug output
        
        if key == Qt.Key.Key_Right:
            print("Right arrow pressed")
            # Next step
            if self.controller.next_step():
                self.statusBar().showMessage(self.controller.get_status())
        
        elif key == Qt.Key.Key_Left:
            print("Left arrow pressed")
            # Previous step (restart)
            if self.controller.previous_step():
                # Clear visualization
                for panel in self.pipeline_view.panels.values():
                    for card in list(panel.cards.values()):
                        panel.remove_card(card.function_name)
                self.pipeline_view.all_cards.clear()
                
                # Restart
                self.controller.next_step()
                self.statusBar().showMessage(self.controller.get_status())
        
        elif key == Qt.Key.Key_R or key == Qt.Key.Key_R.value:
            print("R key pressed")
            # Restart demo
            self.controller.current_step = 0
            self.controller.current_iteration = 0
            
            # Clear visualization
            for panel in self.pipeline_view.panels.values():
                for card in list(panel.cards.values()):
                    panel.remove_card(card.function_name)
            self.pipeline_view.all_cards.clear()
            
            self.statusBar().showMessage(self.controller.get_status())
            
        elif key == Qt.Key.Key_Escape:
            print("Escape pressed")
            # Exit
            self.close()
            
        else:
            super().keyPressEvent(event)


def main():
    """Main entry point"""
    app = QApplication(sys.argv)
    app.setStyle('Fusion')
    
    # Create and show visualization window
    window = ManualDemoWindow()
    window.show()
    
    print("Manual Demo Controls:")
    print("→ (Right Arrow) - Next step")
    print("← (Left Arrow) - Restart demo")
    print("R - Restart demo")
    print("ESC - Exit")
    print()
    print("The demo will step through the PYTHAG function modernization process")
    print("including two refinement iterations before achieving 100% test pass rate.")
    
    sys.exit(app.exec())


if __name__ == '__main__':
    main()
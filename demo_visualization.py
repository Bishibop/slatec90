#!/usr/bin/env python3
"""
Demo script to test the visualization with simulated events
"""
import sys
import time
import threading
import random
from PyQt6.QtWidgets import QApplication

from visualization import SLATECVisualizationWindow, EventType, event_emitter, Event

def simulate_function_processing_from_queue(func_name: str, success_rate: float = 0.8):
    """Simulate processing a function that's already in the queue"""
    
    # Start with test generation (function already in queue)
    time.sleep(2.0)  # Pause to see it in queue
    
    # Generate tests
    event_emitter.emit(EventType.TEST_GEN_START, func_name)
    time.sleep(2.5)  # Test generation takes time
    test_count = random.randint(50, 100)
    event_emitter.emit(EventType.TEST_GEN_COMPLETE, func_name, test_count=test_count)
    time.sleep(1.5)
    
    # Modernize
    event_emitter.emit(EventType.MODERNIZE_START, func_name)
    time.sleep(3.0)  # Modernization is slow
    event_emitter.emit(EventType.MODERNIZE_COMPLETE, func_name)
    time.sleep(1.5)
    
    # Validate
    event_emitter.emit(EventType.VALIDATE_START, func_name)
    time.sleep(2.0)  # Validation takes time
    
    # Initial validation - make it always need refinement for demo
    pass_rate = random.uniform(0.6, 0.85)  # Always less than 1.0
    event_emitter.emit(EventType.VALIDATE_COMPLETE, func_name, 
                      pass_rate=pass_rate, 
                      passed_tests=int(test_count * pass_rate),
                      total_tests=test_count)
    time.sleep(2.0)  # Pause to see the initial validation result
    
    # Refinement loop
    iterations = 2  # Always do 2 iterations for demo
    for i in range(iterations):
        # Emit refinement start - this should move card back to Modernize
        event_emitter.emit(EventType.REFINE_START, func_name, iteration=i+1, pass_rate=pass_rate)
        time.sleep(3.0)  # Longer pause to see the backward movement
        
        # Card should now be in Modernize panel with "refining" status
        # Re-modernize
        time.sleep(3.0)  # Modernization takes time
        event_emitter.emit(EventType.MODERNIZE_COMPLETE, func_name)
        time.sleep(1.5)
        
        # Move to validate again
        event_emitter.emit(EventType.VALIDATE_START, func_name)
        time.sleep(2.5)  # Validation takes time
        
        # Update pass rate
        if i == iterations - 1:
            pass_rate = 1.0  # Last iteration succeeds
        else:
            pass_rate = min(0.95, pass_rate + random.uniform(0.1, 0.2))
            
        event_emitter.emit(EventType.VALIDATE_COMPLETE, func_name,
                         pass_rate=pass_rate,
                         passed_tests=int(test_count * pass_rate),
                         total_tests=test_count)
        time.sleep(2.0)
    
    # Complete
    time.sleep(1.0)
    event_emitter.emit(EventType.FUNCTION_COMPLETE, func_name, status='success')
    return True

def simulate_function_processing(func_name: str, success_rate: float = 0.8):
    """Simulate processing a single function"""
    
    # Start processing
    event_emitter.emit(EventType.FUNCTION_START, func_name)
    time.sleep(2.0)  # Longer delay to ensure card appears in Queue
    
    # Generate tests
    event_emitter.emit(EventType.TEST_GEN_START, func_name)
    time.sleep(2.5)  # Test generation takes time
    test_count = random.randint(50, 100)
    event_emitter.emit(EventType.TEST_GEN_COMPLETE, func_name, test_count=test_count)
    time.sleep(1.5)
    
    # Modernize
    event_emitter.emit(EventType.MODERNIZE_START, func_name)
    time.sleep(3.0)  # Modernization is slow
    event_emitter.emit(EventType.MODERNIZE_COMPLETE, func_name)
    time.sleep(1.5)
    
    # Validate
    event_emitter.emit(EventType.VALIDATE_START, func_name)
    time.sleep(2.0)  # Validation takes time
    
    # Initial validation
    pass_rate = random.uniform(0.7, 0.95) if random.random() < success_rate else random.uniform(0.3, 0.7)
    event_emitter.emit(EventType.VALIDATE_COMPLETE, func_name, 
                      pass_rate=pass_rate, 
                      passed_tests=int(test_count * pass_rate),
                      total_tests=test_count)
    time.sleep(0.8)
    
    # Refinement if needed
    if pass_rate < 1.0:
        iterations = random.randint(1, 2)
        for i in range(iterations):
            time.sleep(1.0)
            event_emitter.emit(EventType.REFINE_START, func_name, iteration=i+1, pass_rate=pass_rate)
            time.sleep(1.0)
            
            # Back to modernizer
            event_emitter.emit(EventType.MODERNIZE_START, func_name)
            time.sleep(2.5)  # Modernization takes time
            event_emitter.emit(EventType.MODERNIZE_COMPLETE, func_name)
            time.sleep(0.8)
            
            # Recompile
            event_emitter.emit(EventType.COMPILE_START, func_name)
            time.sleep(1.5)
            event_emitter.emit(EventType.COMPILE_SUCCESS, func_name)
            time.sleep(0.8)
            
            # Re-validate
            event_emitter.emit(EventType.VALIDATE_START, func_name)
            time.sleep(2.0)  # Validation takes time
            pass_rate = min(1.0, pass_rate + random.uniform(0.1, 0.3))
            event_emitter.emit(EventType.VALIDATE_COMPLETE, func_name,
                             pass_rate=pass_rate,
                             passed_tests=int(test_count * pass_rate),
                             total_tests=test_count)
            time.sleep(0.8)
            
            if pass_rate >= 1.0:
                break
    
    # Complete
    time.sleep(0.5)
    if pass_rate >= 1.0:
        event_emitter.emit(EventType.FUNCTION_COMPLETE, func_name, status='success')
        return True
    else:
        event_emitter.emit(EventType.FUNCTION_FAILED, func_name, status='failed')
        return False

def run_demo_simulation():
    """Run the demo simulation"""
    # Start event emitter
    event_emitter.start()
    
    # List of functions to simulate
    functions = ['PYTHAG']
    
    # Emit run start
    event_emitter.emit(EventType.RUN_START, data={
        'total_functions': len(functions),
        'parallel': False,  # Sequential processing
        'workers': 1
    })
    
    time.sleep(1)
    
    # Add all functions to queue first
    for func in functions:
        event_emitter.emit(EventType.FUNCTION_START, func)
        time.sleep(0.3)  # Small delay between adding to queue
    
    # Let them all appear in queue
    time.sleep(2.0)
    
    # Now process functions one at a time
    completed = 0
    failed = 0
    
    for i, func in enumerate(functions):
        # Process each function completely before moving to next
        success = simulate_function_processing_from_queue(func, 0.7 if i % 3 == 0 else 0.85)
        
        if success:
            completed += 1
        else:
            failed += 1
            
        # Pause between functions
        time.sleep(2.0)
    
    # Emit run complete
    time.sleep(1)
    event_emitter.emit(EventType.RUN_COMPLETE, data={
        'completed': completed,
        'failed': failed,
        'success_rate': completed / len(functions) if len(functions) > 0 else 0
    })

def main():
    """Main entry point"""
    app = QApplication(sys.argv)
    app.setStyle('Fusion')
    
    # Create and show visualization window
    window = SLATECVisualizationWindow()
    window.show()
    window.setWindowTitle("SLATEC Modernization Visualization - Demo Mode")
    
    # Start simulation in background thread
    simulation_thread = threading.Thread(target=run_demo_simulation, daemon=True)
    simulation_thread.start()
    
    print("Running visualization demo with simulated events...")
    print("This demonstrates the visualization without running the actual orchestrator.")
    
    sys.exit(app.exec())

if __name__ == '__main__':
    main()
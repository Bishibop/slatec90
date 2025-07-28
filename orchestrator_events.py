"""
Event-emitting mixin for SLATEC orchestrator
This module provides minimal modifications to add event emission
"""
import sys
from pathlib import Path

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from visualization.event_system import EventType, emit_event, event_emitter

class OrchestratorEventsMixin:
    """Mixin class to add event emission to the orchestrator"""
    
    def emit_event(self, event_type: EventType, function_name=None, **data):
        """Emit an event if visualization is enabled"""
        if hasattr(self, 'enable_visualization') and self.enable_visualization:
            emit_event(event_type, function_name, **data)
            
    def process_function_with_events(self, func_name):
        """Wrapper around process_function that emits events"""
        # Emit function start
        self.emit_event(EventType.FUNCTION_START, func_name)
        
        # Store original methods to wrap them
        original_methods = {
            'read_source': self.read_source,
            'test_gen.generate': self.test_gen.generate,
            'modernizer.modernize': self.modernizer.modernize,
            'validator.compile_modern': self.validator.compile_modern,
            'validator.validate': self.validator.validate,
            'modernizer.refine': self.modernizer.refine,
        }
        
        # Wrap read_source
        def read_source_wrapped(func_name):
            self.emit_event(EventType.SOURCE_READ, func_name, status='reading')
            result = original_methods['read_source'](func_name)
            self.emit_event(EventType.SOURCE_READ, func_name, status='complete')
            return result
            
        # Wrap test generation
        def test_gen_wrapped(func_name, source_code):
            self.emit_event(EventType.TEST_GEN_START, func_name)
            result = original_methods['test_gen.generate'](func_name, source_code)
            test_count = result.count('TEST_START') if isinstance(result, str) else 0
            self.emit_event(EventType.TEST_GEN_COMPLETE, func_name, test_count=test_count)
            return result
            
        # Wrap modernization
        def modernize_wrapped(func_name, f77_code, test_content):
            self.emit_event(EventType.MODERNIZE_START, func_name)
            result = original_methods['modernizer.modernize'](func_name, f77_code, test_content)
            self.emit_event(EventType.MODERNIZE_COMPLETE, func_name)
            return result
            
        # Wrap compilation
        def compile_wrapped(func_name, modern_file):
            self.emit_event(EventType.COMPILE_START, func_name)
            result = original_methods['validator.compile_modern'](func_name, modern_file)
            if result:
                self.emit_event(EventType.COMPILE_SUCCESS, func_name)
            else:
                self.emit_event(EventType.COMPILE_FAILED, func_name)
            return result
            
        # Wrap validation
        def validate_wrapped(func_name, test_file):
            self.emit_event(EventType.VALIDATE_START, func_name)
            result = original_methods['validator.validate'](func_name, test_file)
            self.emit_event(EventType.VALIDATE_COMPLETE, func_name, 
                          pass_rate=result.get('pass_rate', 0),
                          passed_tests=result.get('passed', 0),
                          total_tests=result.get('total', 0))
            return result
            
        # Wrap refinement
        def refine_wrapped(func_name, current_code, errors, **kwargs):
            self.emit_event(EventType.REFINE_START, func_name, 
                          iteration=kwargs.get('iteration', 1))
            result = original_methods['modernizer.refine'](
                func_name, current_code, errors, **kwargs)
            self.emit_event(EventType.REFINE_COMPLETE, func_name)
            return result
        
        # Temporarily replace methods
        self.read_source = read_source_wrapped
        self.test_gen.generate = test_gen_wrapped
        self.modernizer.modernize = modernize_wrapped
        self.validator.compile_modern = compile_wrapped
        self.validator.validate = validate_wrapped
        self.modernizer.refine = refine_wrapped
        
        try:
            # Call original process_function
            result = super().process_function(func_name)
            
            # Emit completion event
            if result:
                self.emit_event(EventType.FUNCTION_COMPLETE, func_name, status='success')
            else:
                self.emit_event(EventType.FUNCTION_FAILED, func_name, status='failed')
                
            return result
            
        finally:
            # Restore original methods
            self.read_source = original_methods['read_source']
            self.test_gen.generate = original_methods['test_gen.generate']
            self.modernizer.modernize = original_methods['modernizer.modernize']
            self.validator.compile_modern = original_methods['validator.compile_modern']
            self.validator.validate = original_methods['validator.validate']
            self.modernizer.refine = original_methods['modernizer.refine']
            
    def process_all_with_events(self, parallel=True):
        """Wrapper around process_all that emits events"""
        self.emit_event(EventType.RUN_START, data={
            'total_functions': len(self.function_data.get('functions', [])),
            'parallel': parallel
        })
        
        # Replace process_function with event-emitting version
        original_process = self.process_function
        self.process_function = self.process_function_with_events
        
        try:
            result = super().process_all(parallel)
            self.emit_event(EventType.RUN_COMPLETE, data={
                'completed': len(self.progress.get('completed', [])),
                'failed': len(self.progress.get('failed', []))
            })
            return result
        finally:
            self.process_function = original_process
#!/usr/bin/env python3
"""
Visual version of SLATEC orchestrator with event emission
This extends the original orchestrator with visualization support
"""
import sys
from pathlib import Path

# Add parent directory to path
parent_dir = Path(__file__).parent.parent
sys.path.insert(0, str(parent_dir))

# Import original orchestrator
from slatec_orchestrator import SLATECOrchestrator
from visualization.event_system import EventType, emit_event, event_emitter

class VisualSLATECOrchestrator(SLATECOrchestrator):
    """Extended orchestrator with event emission for visualization"""
    
    def __init__(self, *args, enable_visualization=True, **kwargs):
        super().__init__(*args, **kwargs)
        self.enable_visualization = enable_visualization
        if enable_visualization:
            event_emitter.start()
            self.logger.info("Visualization events enabled")
            
    def process_function(self, func_name):
        """Override to add event emission"""
        # Emit function start
        if self.enable_visualization:
            emit_event(EventType.FUNCTION_START, func_name)
        
        # Store the original logger info method
        original_logger_info = self.logger.info
        
        # Create a wrapper that emits events for key log messages
        def logger_info_wrapper(msg, *args, **kwargs):
            original_logger_info(msg, *args, **kwargs)
            
            # Emit events based on log messages
            if self.enable_visualization:
                if "Generating test cases" in msg:
                    emit_event(EventType.TEST_GEN_START, func_name)
                elif "Generated" in msg and "test cases" in msg:
                    # Extract test count from message
                    import re
                    match = re.search(r'Generated (\d+) test cases', msg)
                    test_count = int(match.group(1)) if match else 0
                    emit_event(EventType.TEST_GEN_COMPLETE, func_name, test_count=test_count)
                elif "Modernizing" in msg and func_name in msg:
                    emit_event(EventType.MODERNIZE_START, func_name)
                elif "Modernization complete" in msg:
                    emit_event(EventType.MODERNIZE_COMPLETE, func_name)
                elif "Validation iteration" in msg:
                    match = re.search(r'iteration (\d+)', msg)
                    iteration = int(match.group(1)) if match else 1
                    emit_event(EventType.ITERATION_START, func_name, iteration=iteration)
                elif "Compilation failed" in msg:
                    emit_event(EventType.COMPILE_FAILED, func_name)
                elif "validated successfully" in msg:
                    emit_event(EventType.VALIDATE_COMPLETE, func_name, status='success')
                elif "Refining" in msg:
                    match = re.search(r'pass rate: ([\d.]+)%', msg)
                    pass_rate = float(match.group(1))/100 if match else 0
                    emit_event(EventType.REFINE_START, func_name, pass_rate=pass_rate)
        
        # Temporarily replace logger.info
        self.logger.info = logger_info_wrapper
        
        try:
            # Wrap compilation to catch events
            original_compile = self.validator.compile_modern
            def compile_wrapper(func_name, modern_file):
                emit_event(EventType.COMPILE_START, func_name)
                result = original_compile(func_name, modern_file)
                if result:
                    emit_event(EventType.COMPILE_SUCCESS, func_name)
                else:
                    emit_event(EventType.COMPILE_FAILED, func_name)
                return result
            self.validator.compile_modern = compile_wrapper
            
            # Wrap validation to catch events
            original_validate = self.validator.validate
            def validate_wrapper(func_name, test_file):
                emit_event(EventType.VALIDATE_START, func_name)
                result = original_validate(func_name, test_file)
                emit_event(EventType.VALIDATE_COMPLETE, func_name,
                          pass_rate=result.get('pass_rate', 0),
                          passed_tests=result.get('passed', 0),
                          total_tests=result.get('total', 0))
                return result
            self.validator.validate = validate_wrapper
            
            # Call parent process_function
            result = super().process_function(func_name)
            
            # Emit completion event
            if self.enable_visualization:
                if result:
                    emit_event(EventType.FUNCTION_COMPLETE, func_name, status='success')
                else:
                    emit_event(EventType.FUNCTION_FAILED, func_name, status='failed')
                    
            return result
            
        finally:
            # Restore original methods
            self.logger.info = original_logger_info
            self.validator.compile_modern = original_compile
            self.validator.validate = original_validate
            
    def process_all(self, parallel=True):
        """Override to add run-level events"""
        if self.enable_visualization:
            remaining = [f for f in self.function_data['functions'] 
                        if f not in self.progress['completed']]
            emit_event(EventType.RUN_START, data={
                'total_functions': len(remaining),
                'parallel': parallel,
                'workers': self.config.get('parallel_workers', 4) if parallel else 1
            })
        
        result = super().process_all(parallel)
        
        if self.enable_visualization:
            emit_event(EventType.RUN_COMPLETE, data={
                'completed': len(self.progress.get('completed', [])),
                'failed': len(self.progress.get('failed', [])),
                'success_rate': len(self.progress.get('completed', [])) / 
                               max(1, len(self.progress.get('completed', [])) + 
                                   len(self.progress.get('failed', [])))
            })
            
        return result

def main():
    """Main entry point - same as original but with visual orchestrator"""
    import argparse
    
    parser = argparse.ArgumentParser(description='SLATEC Modernization Orchestrator with Visualization')
    parser.add_argument('--list', dest='list_name', 
                       help='Name of function list to process')
    parser.add_argument('--list-file', dest='list_file',
                       help='Path to custom function list JSON file')
    parser.add_argument('--function', dest='function',
                       help='Process a single function')
    parser.add_argument('--functions', dest='functions',
                       help='Comma-separated list of functions to process')
    parser.add_argument('--sequential', action='store_true',
                       help='Process functions sequentially instead of in parallel')
    parser.add_argument('--summary', action='store_true',
                       help='Show summary of progress')
    parser.add_argument('--debug', action='store_true',
                       help='Enable debug logging')
    parser.add_argument('--no-visualization', action='store_true',
                       help='Disable visualization events')
    
    args = parser.parse_args()
    
    # Determine what to process
    list_name = args.list_name
    list_file = args.list_file
    functions = None
    
    if args.function:
        functions = [args.function.upper()]
    elif args.functions:
        functions = [f.strip().upper() for f in args.functions.split(',')]
    
    if not any([list_name, list_file, functions, args.summary]):
        parser.error("Must specify --list, --list-file, --function, --functions, or --summary")
    
    # Create orchestrator with visualization
    orchestrator = VisualSLATECOrchestrator(
        list_name=list_name,
        list_file=list_file, 
        functions=functions,
        debug=args.debug,
        enable_visualization=not args.no_visualization
    )
    
    if args.summary:
        orchestrator.show_summary()
    else:
        # Process all
        orchestrator.process_all(parallel=not args.sequential)

if __name__ == '__main__':
    main()
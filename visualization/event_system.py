"""
Event system for SLATEC modernization visualization
Provides thread-safe event emission and listening
"""
import queue
import threading
from datetime import datetime
from typing import Dict, Any, Optional, Callable
from enum import Enum, auto
import json

class EventType(Enum):
    """Types of events emitted during modernization"""
    # Function-level events
    FUNCTION_START = auto()
    FUNCTION_COMPLETE = auto()
    FUNCTION_FAILED = auto()
    
    # Processing stage events
    SOURCE_READ = auto()
    TEST_GEN_START = auto()
    TEST_GEN_COMPLETE = auto()
    MODERNIZE_START = auto()
    MODERNIZE_COMPLETE = auto()
    COMPILE_START = auto()
    COMPILE_SUCCESS = auto()
    COMPILE_FAILED = auto()
    VALIDATE_START = auto()
    VALIDATE_COMPLETE = auto()
    REFINE_START = auto()
    REFINE_COMPLETE = auto()
    
    # Progress events
    ITERATION_START = auto()
    ITERATION_COMPLETE = auto()
    PROGRESS_UPDATE = auto()
    
    # System events
    RUN_START = auto()
    RUN_COMPLETE = auto()
    ERROR = auto()

class Event:
    """Represents a single event in the system"""
    def __init__(self, event_type: EventType, function_name: Optional[str] = None, 
                 data: Optional[Dict[str, Any]] = None):
        self.timestamp = datetime.now()
        self.event_type = event_type
        self.function_name = function_name
        self.data = data or {}
        
    def to_dict(self) -> Dict[str, Any]:
        """Convert event to dictionary for serialization"""
        return {
            'timestamp': self.timestamp.isoformat(),
            'event_type': self.event_type.name,
            'function_name': self.function_name,
            'data': self.data
        }
        
    def __str__(self) -> str:
        return f"Event({self.event_type.name}, {self.function_name}, {self.timestamp.strftime('%H:%M:%S')})"

class EventEmitter:
    """Thread-safe event emitter for the orchestrator"""
    _instance = None
    _lock = threading.Lock()
    
    def __new__(cls):
        if cls._instance is None:
            with cls._lock:
                if cls._instance is None:
                    cls._instance = super().__new__(cls)
        return cls._instance
    
    def __init__(self):
        if not hasattr(self, '_initialized'):
            self._initialized = True
            self._queue = queue.Queue()
            self._listeners = []
            self._file_logger = None
            self._running = False
            self._thread = None
            
    def set_file_logger(self, filepath: str):
        """Enable logging events to a file"""
        self._file_logger = filepath
        
    def emit(self, event_type: EventType, function_name: Optional[str] = None, 
             **data):
        """Emit an event"""
        event = Event(event_type, function_name, data)
        self._queue.put(event)
        
        # Log to file if enabled
        if self._file_logger:
            try:
                with open(self._file_logger, 'a') as f:
                    f.write(json.dumps(event.to_dict()) + '\n')
            except:
                pass  # Don't let logging errors break the system
                
    def add_listener(self, callback: Callable[[Event], None]):
        """Add a listener for events"""
        self._listeners.append(callback)
        
    def remove_listener(self, callback: Callable[[Event], None]):
        """Remove a listener"""
        if callback in self._listeners:
            self._listeners.remove(callback)
            
    def start(self):
        """Start the event processing thread"""
        if not self._running:
            self._running = True
            self._thread = threading.Thread(target=self._process_events, daemon=True)
            self._thread.start()
            
    def stop(self):
        """Stop the event processing thread"""
        self._running = False
        if self._thread:
            self._thread.join()
            
    def _process_events(self):
        """Process events from the queue"""
        while self._running:
            try:
                event = self._queue.get(timeout=0.1)
                for listener in self._listeners:
                    try:
                        listener(event)
                    except Exception as e:
                        print(f"Error in event listener: {e}")
            except queue.Empty:
                continue
                
    def get_queue(self) -> queue.Queue:
        """Get direct access to the event queue for custom processing"""
        return self._queue

# Global event emitter instance
event_emitter = EventEmitter()

def emit_event(event_type: EventType, function_name: Optional[str] = None, **data):
    """Convenience function to emit events"""
    event_emitter.emit(event_type, function_name, **data)
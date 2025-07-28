#!/usr/bin/env python3
"""
Run the SLATEC modernization visualization
Can be run standalone or alongside the orchestrator
"""
import sys
import subprocess
import threading
from pathlib import Path
from PyQt6.QtWidgets import QApplication

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

from visualization import SLATECVisualizationWindow

def run_standalone():
    """Run visualization in standalone mode"""
    app = QApplication(sys.argv)
    
    # Set application style
    app.setStyle('Fusion')
    
    # Create and show main window
    window = SLATECVisualizationWindow()
    window.show()
    
    print("Visualization running in standalone mode.")
    print("To see live updates, run the orchestrator with visualization enabled:")
    print("  python slatec_orchestrator_visual.py --list simple")
    print("\nOr to test with a single function:")
    print("  python slatec_orchestrator_visual.py --function PYTHAG")
    
    sys.exit(app.exec())

def run_with_orchestrator(orchestrator_args):
    """Run visualization alongside the orchestrator"""
    app = QApplication([])
    app.setStyle('Fusion')
    
    # Create and show visualization window
    window = SLATECVisualizationWindow()
    window.show()
    
    # Run orchestrator in a separate thread
    def run_orchestrator():
        cmd = [sys.executable, 'slatec_orchestrator_visual.py'] + orchestrator_args
        print(f"Starting orchestrator: {' '.join(cmd)}")
        process = subprocess.Popen(cmd, cwd=Path(__file__).parent)
        process.wait()
        
    orchestrator_thread = threading.Thread(target=run_orchestrator, daemon=True)
    orchestrator_thread.start()
    
    sys.exit(app.exec())

def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(
        description='SLATEC Modernization Visualization',
        epilog='Examples:\n'
               '  %(prog)s                    # Run standalone\n'
               '  %(prog)s --with-orchestrator --list simple\n'
               '  %(prog)s --with-orchestrator --function PYTHAG',
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument('--with-orchestrator', action='store_true',
                       help='Run with orchestrator (pass orchestrator args after this)')
    
    args, orchestrator_args = parser.parse_known_args()
    
    if args.with_orchestrator:
        if not orchestrator_args:
            parser.error("When using --with-orchestrator, provide orchestrator arguments")
        run_with_orchestrator(orchestrator_args)
    else:
        run_standalone()

if __name__ == '__main__':
    main()
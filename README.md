# SLATEC Modernization Visualization

A real-time visualization tool for monitoring the SLATEC function modernization process. This desktop application provides an interactive system architecture diagram showing the flow of functions through various processing stages.

## Features

- **Live System Architecture Diagram**: Shows the modernization pipeline with animated nodes and connections
- **Real-time Updates**: Monitors orchestrator events and updates visualization instantly
- **Function Pipeline View**: Track multiple functions being processed in parallel
- **Metrics Dashboard**: Overall statistics including success rate, completion count, and average processing time
- **Event Log**: Detailed timeline of all processing events
- **Dark Theme**: Professional appearance optimized for extended viewing

## Installation

1. Install PyQt6 dependencies:
```bash
pip install -r requirements.txt
```

## Usage

### Option 1: Demo Mode (No Orchestrator Required)

Test the visualization with simulated events:

```bash
python demo_visualization.py
```

This runs a demonstration showing how the visualization responds to modernization events without needing the actual orchestrator.

### Option 2: Standalone Mode

Run the visualization and connect to an orchestrator later:

```bash
python run_visualization.py
```

Then in another terminal, run the visual orchestrator:
```bash
python slatec_orchestrator_visual.py --function PYTHAG
```

### Option 3: Integrated Mode

Run visualization and orchestrator together:

```bash
# Process a single function
python run_visualization.py --with-orchestrator --function PYTHAG

# Process a list of functions
python run_visualization.py --with-orchestrator --list simple

# Process multiple functions
python run_visualization.py --with-orchestrator --functions PYTHAG,CDIV,CSROOT
```

### Option 4: Use Visual Orchestrator Directly

The visual orchestrator can be used as a drop-in replacement for the standard orchestrator:

```bash
# All standard orchestrator commands work
python slatec_orchestrator_visual.py --list simple
python slatec_orchestrator_visual.py --function ENORM
python slatec_orchestrator_visual.py --summary

# Disable visualization if needed
python slatec_orchestrator_visual.py --list simple --no-visualization
```

## Architecture

### Visual Components

1. **System Architecture Diagram**
   - **Nodes**: Represent processing stages (Source, Test Generator, Modernizer, Compiler, Validator, Output)
   - **Connections**: Show data flow with animated particles
   - **States**: Idle (gray), Active (blue), Success (green), Error (red)
   - **Refinement Loop**: Shows iterative improvement process

2. **Function Pipeline**
   - Lists all functions being processed
   - Shows current status, progress bar, and pass rate
   - Color-coded status indicators

3. **Metrics Panel**
   - Total functions to process
   - Completed count (with success indicator)
   - Failed count (with error indicator)
   - Active count (currently processing)
   - Overall success rate
   - Average processing time per function

4. **Event Log**
   - Timestamped events for debugging
   - Shows which function triggered each event

### Event System

The visualization responds to these orchestrator events:

- `FUNCTION_START/COMPLETE/FAILED`: Function-level progress
- `SOURCE_READ`: Reading F77 source file
- `TEST_GEN_START/COMPLETE`: Test case generation
- `MODERNIZE_START/COMPLETE`: F77 to F90 conversion
- `COMPILE_START/SUCCESS/FAILED`: Fortran compilation
- `VALIDATE_START/COMPLETE`: Test validation
- `REFINE_START/COMPLETE`: Iterative refinement
- `RUN_START/COMPLETE`: Overall run progress

### Technical Details

- **Framework**: PyQt6 with QGraphicsScene for smooth animations
- **Communication**: Thread-safe event queue between orchestrator and UI
- **Performance**: Efficient updates using Qt's signal/slot mechanism
- **Animations**: Particle effects, node glowing, smooth transitions

## Extending the Visualization

To add new visualizations or modify existing ones:

1. **Add new event types** in `visualization/event_system.py`
2. **Modify node behavior** in `visualization/flow_diagram.py`
3. **Add new metrics** in `visualization/main_window.py`
4. **Customize styling** in `visualization/styles.py`

## Troubleshooting

### PyQt6 Installation Issues

If you encounter issues installing PyQt6:

```bash
# macOS
brew install qt6
pip install PyQt6

# Linux
sudo apt-get install qt6-base-dev
pip install PyQt6

# Windows
pip install PyQt6
```

### Visualization Not Updating

1. Ensure you're using `slatec_orchestrator_visual.py` not the standard orchestrator
2. Check that events are being emitted (check the event log panel)
3. Verify no errors in the console

### Performance Issues

- The visualization is optimized for monitoring up to ~20 parallel functions
- For larger batches, consider using `--no-visualization` flag
- Particle animations can be disabled by modifying `ANIMATION` settings in styles.py
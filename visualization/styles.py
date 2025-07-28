"""
Visual styles and themes for SLATEC visualization
"""

# Dark theme color palette
COLORS = {
    'background': '#1e1e1e',
    'surface': '#2d2d2d',
    'surface_light': '#3a3a3a',
    'text': '#e0e0e0',
    'text_dim': '#a0a0a0',
    'accent': '#0d7ec4',
    'success': '#4caf50',
    'warning': '#ff9800',
    'error': '#f44336',
    'info': '#2196f3',
    
    # Node colors
    'node_idle': '#3a3a3a',
    'node_active': '#0d7ec4',
    'node_success': '#4caf50',
    'node_error': '#f44336',
    'node_border': '#555555',
    
    # Connection colors
    'connection': '#555555',
    'connection_active': '#0d7ec4',
    'particle': '#00ff00',
}

# Application stylesheet
DARK_THEME = """
QMainWindow {
    background-color: #1e1e1e;
}

QWidget {
    background-color: #1e1e1e;
    color: #e0e0e0;
    font-family: 'Segoe UI', Arial, sans-serif;
    font-size: 12px;
}

QGroupBox {
    background-color: #2d2d2d;
    border: 1px solid #3a3a3a;
    border-radius: 4px;
    margin-top: 8px;
    padding-top: 16px;
}

QGroupBox::title {
    subcontrol-origin: margin;
    left: 10px;
    padding: 0 5px 0 5px;
}

QListWidget {
    background-color: #2d2d2d;
    border: 1px solid #3a3a3a;
    border-radius: 4px;
    outline: none;
}

QListWidget::item {
    padding: 4px;
    border-bottom: 1px solid #3a3a3a;
}

QListWidget::item:selected {
    background-color: #0d7ec4;
}

QListWidget::item:hover {
    background-color: #3a3a3a;
}

QTextEdit {
    background-color: #2d2d2d;
    border: 1px solid #3a3a3a;
    border-radius: 4px;
    padding: 4px;
}

QScrollBar:vertical {
    background-color: #2d2d2d;
    width: 12px;
    border-radius: 6px;
}

QScrollBar::handle:vertical {
    background-color: #555555;
    border-radius: 6px;
    min-height: 20px;
}

QScrollBar::handle:vertical:hover {
    background-color: #666666;
}

QLabel {
    color: #e0e0e0;
}

QLabel#status_label {
    font-weight: bold;
    padding: 4px;
}

/* Progress bars */
QProgressBar {
    background-color: #2d2d2d;
    border: 1px solid #3a3a3a;
    border-radius: 4px;
    text-align: center;
    height: 20px;
}

QProgressBar::chunk {
    background-color: #0d7ec4;
    border-radius: 3px;
}

/* Buttons */
QPushButton {
    background-color: #3a3a3a;
    border: 1px solid #555555;
    border-radius: 4px;
    padding: 6px 12px;
    color: #e0e0e0;
}

QPushButton:hover {
    background-color: #454545;
    border-color: #0d7ec4;
}

QPushButton:pressed {
    background-color: #2d2d2d;
}

/* Tab widget */
QTabWidget::pane {
    background-color: #2d2d2d;
    border: 1px solid #3a3a3a;
}

QTabBar::tab {
    background-color: #3a3a3a;
    color: #a0a0a0;
    padding: 8px 16px;
    margin-right: 2px;
}

QTabBar::tab:selected {
    background-color: #2d2d2d;
    color: #e0e0e0;
}

QTabBar::tab:hover {
    background-color: #454545;
}

/* Splitters */
QSplitter::handle {
    background-color: #3a3a3a;
}

QSplitter::handle:horizontal {
    width: 4px;
}

QSplitter::handle:vertical {
    height: 4px;
}

/* Headers */
QHeaderView::section {
    background-color: #2d2d2d;
    color: #e0e0e0;
    padding: 4px;
    border: 1px solid #3a3a3a;
}
"""

# Node styling parameters
NODE_STYLE = {
    'width': 160,
    'height': 80,
    'corner_radius': 8,
    'border_width': 2,
    'font_size': 12,
    'icon_size': 24,
}

# Animation parameters
ANIMATION = {
    'particle_speed': 100,  # pixels per second
    'particle_size': 4,
    'glow_duration': 1000,  # milliseconds
    'transition_duration': 300,  # milliseconds
}
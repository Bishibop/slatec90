# SLATEC Migration Visualization Guide

This guide explains how to use the generated DOT files to create professional dependency visualizations that can be printed or exported.

## Generated DOT Files

### 1. `hierarchical_dependencies.dot` - Complete Dependency Hierarchy
- **Content**: All 573 available functions organized by dependency levels (0-3)
- **Structure**: Functions grouped by dependency level, then by functional category
- **Best for**: Understanding the complete migration landscape
- **Print recommendation**: Large format (A2/A1) or multi-page

### 2. `critical_path.dot` - High-Priority Migration Targets  
- **Content**: 29 critical functions with high dependent counts
- **Structure**: Grouped by completion status and priority level
- **Best for**: Migration planning and identifying next targets
- **Print recommendation**: Standard 8.5x11" or A4

### 3. `zero_dependencies.dot` - Immediate Migration Candidates
- **Content**: 168 zero-dependency functions organized by status
- **Structure**: Clustered by completion status (completed/available/etc.)
- **Best for**: Identifying functions that can be migrated immediately
- **Print recommendation**: Standard 8.5x11" or A4

### 4. `migration_candidates.dot` - Simple Migration Overview
- **Content**: Zero-dependency functions split into completed vs. available
- **Structure**: Two main clusters showing progress
- **Best for**: Quick status overview
- **Print recommendation**: Standard 8.5x11" or A4

## Visualization Tools

### Option 1: Graphviz Online (Recommended for Quick Viewing)
1. Go to [viz-js.com](http://viz-js.com) or [dreampuf.github.io/GraphvizOnline](https://dreampuf.github.io/GraphvizOnline/)
2. Copy and paste the DOT file content
3. Export as PNG/SVG/PDF

### Option 2: yEd Live (Recommended for Print Layout)
1. Go to [yworks.com/yed-live](https://www.yworks.com/yed-live/)
2. Import DOT file (File → Import → DOT)
3. Apply layout:
   - For hierarchical graphs: Layout → Hierarchical → Top to Bottom
   - For critical path: Layout → Organic → Smart
4. Adjust for print:
   - View → Fit Content
   - View → Zoom to fit page
5. Export as PDF or high-resolution PNG

### Option 3: Local Graphviz (Best for High-Quality Output)
```bash
# Install Graphviz
# macOS: brew install graphviz
# Ubuntu: sudo apt install graphviz
# Windows: Download from graphviz.org

# Generate images
dot -Tpdf hierarchical_dependencies.dot -o hierarchical_dependencies.pdf
dot -Tpng critical_path.dot -o critical_path.png -Gdpi=300
dot -Tsvg zero_dependencies.dot -o zero_dependencies.svg
```

### Option 4: draw.io (Good for Editing)
1. Go to [app.diagrams.net](https://app.diagrams.net)
2. Create new diagram
3. File → Import from → Device (select .dot file)
4. Use Arrange → Layout to adjust positioning
5. Export as PDF/PNG

## Print Optimization Tips

### For Large Hierarchical Graph
- **Layout**: Use "dot" layout with `rankdir=TB` (top-to-bottom)
- **Paper size**: A2 (420×594 mm) or A1 (594×841 mm) minimum
- **DPI**: 300 for professional printing
- **Font size**: Minimum 8pt for readability when printed

### For Critical Path and Zero-Dependency Graphs
- **Layout**: Use "neato" or "fdp" layout for compact clustering
- **Paper size**: A4 (210×297 mm) or US Letter (8.5×11")
- **Orientation**: Landscape for better fit
- **DPI**: 300 for crisp text

### Command Examples for Print-Optimized Output
```bash
# High-resolution PDF for large format printing
dot -Tpdf -Gdpi=300 hierarchical_dependencies.dot -o hierarchical_print.pdf

# Landscape layout for letter-size paper
dot -Tpdf -Grankdir=LR -Gsize="11,8.5!" -Gdpi=300 critical_path.dot -o critical_path_landscape.pdf

# Compressed layout for zero-dependency overview
neato -Tpdf -Gsize="8.5,11!" -Gdpi=300 zero_dependencies.dot -o zero_deps_compact.pdf
```

## Customization Options

### Modifying Node Colors
Edit the `colors` dictionary in the Python scripts:
```python
colors = {
    'completed': '#90EE90',      # Light green
    'in_progress': '#FFD700',    # Gold
    'available': '#87CEEB',      # Sky blue
    'not_available': '#FFB6C1'   # Light pink
}
```

### Adjusting Layout Parameters
Add these to the DOT file after the opening brace:
```dot
  ranksep=1.0;        // Increase vertical spacing
  nodesep=0.5;        // Increase horizontal spacing
  fontsize=10;        // Adjust font size
  fontname="Arial";   // Change font
```

### Creating Custom Subsets
Modify the generation scripts to filter specific function categories:
```python
# Example: Only show BLAS functions
blas_functions = [f for f in functions if any(f.startswith(x) for x in ['S', 'D', 'C', 'Z']) 
                  and any(x in f for x in ['AXPY', 'DOT', 'NRM2', 'SCAL', 'COPY'])]
```

## Recommended Workflow

1. **Start with Critical Path**: Print `critical_path.dot` to identify next migration targets
2. **Use Zero Dependencies**: Print `zero_dependencies.dot` to see immediate opportunities  
3. **Reference Hierarchical**: Keep `hierarchical_dependencies.dot` as complete reference
4. **Track Progress**: Regenerate graphs after each migration to see updated status

## File Sizes and Performance

| Graph Type | Nodes | Edges | File Size | Render Time |
|------------|-------|-------|-----------|-------------|
| Hierarchical | 573 | ~1200 | ~45KB | 5-10 seconds |
| Critical Path | 29 | ~50 | ~3KB | 1-2 seconds |
| Zero Dependencies | 168 | 0 | ~8KB | 1-2 seconds |
| Migration Candidates | 168 | 0 | ~6KB | 1-2 seconds |

## Troubleshooting

### Graph Too Large/Complex
- Use `critical_path.dot` instead of `hierarchical_dependencies.dot`
- Generate subset graphs for specific categories
- Increase paper size or use multi-page layout

### Text Too Small
- Increase DPI: `-Gdpi=600`
- Use larger font: add `fontsize=12` to DOT file
- Print on larger paper size

### Layout Issues
- Try different layout engines: `dot`, `neato`, `fdp`, `circo`
- Adjust spacing: `ranksep`, `nodesep`
- Use landscape orientation

### Memory Issues with Large Graphs
- Filter to smaller subsets
- Use simplified layouts (remove some edge connections)
- Generate multiple smaller graphs instead of one large one
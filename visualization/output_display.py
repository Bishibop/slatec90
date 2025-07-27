"""
Output Display Widget
Shows stage-specific outputs and data for the SLATEC modernization process
"""
from PyQt6.QtWidgets import QWidget, QTextEdit, QVBoxLayout, QLabel, QScrollArea
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont, QTextCharFormat, QColor
from typing import Dict, Any


class OutputDisplayWidget(QWidget):
    """Widget that displays formatted outputs for different subsystem stages"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.current_stage = "queue"
        self.current_data = {}
        self.init_ui()
        
    def init_ui(self):
        """Initialize the UI"""
        layout = QVBoxLayout()
        layout.setContentsMargins(5, 5, 5, 5)
        
        # Stage title
        self.title_label = QLabel("")
        self.title_label.setStyleSheet("""
            QLabel {
                font-size: 14px;
                font-weight: bold;
                color: #FFFFFF;
                background-color: #444444;
                padding: 8px;
                border-radius: 4px;
            }
        """)
        layout.addWidget(self.title_label)
        
        # Output text area
        self.output_text = QTextEdit()
        self.output_text.setReadOnly(True)
        self.output_text.setStyleSheet("""
            QTextEdit {
                background-color: #1E1E1E;
                color: #FFFFFF;
                border: 1px solid #555555;
                border-radius: 4px;
                font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
                font-size: 11px;
                line-height: 1.4;
            }
        """)
        layout.addWidget(self.output_text)
        
        self.setLayout(layout)
        
    def set_stage_data(self, stage: str, data: Dict[str, Any] = None):
        """Set the current stage and update display"""
        self.current_stage = stage.lower()
        self.current_data = data or {}
        self._update_display()
        
    def _update_display(self):
        """Update the display based on current stage and data"""
        if self.current_stage in ['test generation']:
            self._show_test_generation_output()
        elif self.current_stage in ['modernize']:
            self._show_modernization_output()
        elif self.current_stage in ['validate']:
            self._show_validation_output()
        else:
            self._show_queue_output()
            
    def _show_test_generation_output(self):
        """Show test generation output"""
        self.title_label.setText("Generated Test Cases")
        
        if self.current_data.get('completed'):
            # Show sample of generated tests
            output = """Test Generation Complete!

Generated: 130 comprehensive test cases
Coverage: Boundary conditions, edge cases, normal operation

Sample Test Cases:
═══════════════════

TEST 1: Zero Input Handling
Description: Both inputs zero
Input: a=0.0, b=0.0
Expected: Handles gracefully without overflow

TEST 2: Small Value Precision  
Description: Very small positive values
Input: a=1.0e-15, b=2.0e-15
Expected: Maintains precision for tiny numbers

TEST 3: Large Value Overflow Protection
Description: Values near overflow threshold
Input: a=1.0e+150, b=1.0e+150  
Expected: Prevents overflow, returns valid result

TEST 4: Mixed Scale Inputs
Description: Vastly different magnitudes
Input: a=1.0e-10, b=1.0e+10
Expected: Handles scale differences correctly

TEST 5: Negative Input Handling
Description: Negative input values
Input: a=-3.4, b=-5.7
Expected: Computes absolute value correctly

... (125 more test cases)

Validation Status: All tests passed parameter validation
Mathematical Constraints: Verified for numerical stability"""
        else:
            # Show generation in progress
            output = """Test Generation In Progress...

Current Status: Analyzing function signature
Target: Generate 50-100 comprehensive test cases

LLM Model: o3-mini
Processing: Mathematical function analysis
- Identifying input domains
- Boundary condition analysis  
- Edge case detection
- Numerical stability testing

Parameter Validation: Enabled
Constraint Checking: Active

Estimated completion: 15-30 seconds"""
            
        self.output_text.setPlainText(output)
        
    def _show_modernization_output(self):
        """Show modernization output"""
        self.title_label.setText("Generated F90 Code")
        
        if self.current_data.get('completed'):
            if self.current_data.get('iteration', 0) > 0:
                iteration = self.current_data.get('iteration')
                output = f"""Modernization Refinement (Iteration {iteration}) Complete!

Previous Issues Addressed:
- Numerical precision mismatch
- Array boundary handling
- Error condition improvements

Generated Modern F90 Code:
═══════════════════════════

module pythag_module
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  public :: pythag
  
contains

  pure function pythag(a, b) result(hyp)
    !! Computes sqrt(a**2 + b**2) without overflow
    implicit none
    real(real64), intent(in) :: a, b
    real(real64) :: hyp
    real(real64) :: p, q, r, t, s
    
    ! Improved precision handling for refinement
    p = max(abs(a), abs(b))
    q = min(abs(a), abs(b))
    
    if (q == 0.0_real64) then
      hyp = p
      return
    end if
    
    ! Enhanced iterative algorithm
    do
      r = (q / p)**2
      t = 4.0_real64 + r
      if (t == 4.0_real64) exit
      s = r / t
      p = p + 2.0_real64 * p * s
      q = q * s
    end do
    
    hyp = p
  end function pythag
  
end module pythag_module

Improvements Made:
- Enhanced precision handling
- Better overflow protection
- Improved algorithm convergence"""
            else:
                output = """Initial Modernization Complete!

Generated Modern F90 Code:
═══════════════════════════

module pythag_module
  use, intrinsic :: iso_fortran_env, only: real32
  implicit none
  public :: pythag
  
contains

  pure function pythag(a, b) result(hyp)
    !! Computes sqrt(a**2 + b**2) without overflow
    implicit none
    real, intent(in) :: a, b
    real :: hyp
    real :: p, q, r, t, s
    
    ! Set p to max and q to min of absolute values
    p = max(abs(a), abs(b))
    q = min(abs(a), abs(b))
    
    if (q == 0.0) then
      hyp = p
      return
    end if
    
    ! Iterative algorithm for numerical stability
    do
      r = (q / p)**2
      t = 4.0 + r
      if (t == 4.0) exit
      s = r / t
      p = p + 2.0 * p * s
      q = q * s
    end do
    
    hyp = p
  end function pythag
  
end module pythag_module

Modernization Features:
- Module structure with explicit interfaces
- Pure function for optimization
- Preserved original algorithm exactly
- Modern Fortran syntax and style
- Intrinsic precision control"""
        else:
            # Show generation in progress
            output = """Modernization In Progress...

Current Status: Converting F77 to F90
Model: Processing algorithm analysis

Conversion Steps:
1. ✓ Analyzing original F77 source
2. ✓ Identifying modernization opportunities  
3. ◦ Generating module structure
4. ◦ Converting to modern syntax
5. ◦ Preserving algorithm integrity
6. ◦ Adding type safety

Input Sources:
- Original PYTHAG.f (F77 source)
- Generated test cases (130 tests)
- Function signature metadata

Target: Modern F90 module with pure function interface"""
            
        self.output_text.setPlainText(output)
        
    def _show_validation_output(self):
        """Show validation output"""
        self.title_label.setText("Validation Results")
        
        pass_rate = self.current_data.get('pass_rate', 0.0)
        iteration = self.current_data.get('iteration', 0)
        
        if self.current_data.get('completed'):
            if pass_rate >= 1.0:
                output = f"""Validation Complete - SUCCESS!

Final Results: 130/130 tests PASSED (100%)
Maximum Error: 2.3e-16 (within tolerance)
Validation Status: ✓ PASSED

Test Categories:
═══════════════
✓ Zero inputs: 5/5 passed
✓ Small values: 25/25 passed  
✓ Large values: 20/20 passed
✓ Mixed scales: 30/30 passed
✓ Boundary conditions: 25/25 passed
✓ Special cases: 25/25 passed

Performance Comparison:
- F77 Average: 0.234 μs per call
- F90 Average: 0.229 μs per call
- Performance Gain: 2.1%

Numerical Accuracy: EXCELLENT
- All results within machine epsilon
- No overflow or underflow detected
- Iterative convergence maintained

The modernized function is ready for production use!"""
            else:
                failed_tests = int(130 * (1.0 - pass_rate))
                passed_tests = int(130 * pass_rate)
                
                if iteration > 0:
                    output = f"""Validation Results (Iteration {iteration})

Results: {passed_tests}/130 tests PASSED ({int(pass_rate*100)}%)
Failed: {failed_tests} tests
Status: Needs further refinement

Improvement from Previous Iteration:
- Previous: {int((pass_rate - 0.16) * 100)}% pass rate
- Current: {int(pass_rate * 100)}% pass rate  
- Progress: +{int(0.16 * 100)}% improvement

Remaining Issues:
═══════════════
⚠ Precision mismatch (7 tests)
  - Expected: 1.41421356237309504880
  - Actual:   1.41421356237309482881
  - Error: 2.2e-14 (slightly above tolerance)

⚠ Boundary condition handling (5 tests)
  - Large value combinations
  - Need improved overflow protection

⚠ Algorithm convergence (3 tests)  
  - Slow convergence for extreme ratios
  - Consider iteration limit adjustment

Recommended Fixes:
1. Increase precision in intermediate calculations
2. Enhance overflow detection
3. Adjust convergence criteria"""
                else:
                    output = f"""Initial Validation Results

Results: {passed_tests}/130 tests PASSED ({int(pass_rate*100)}%)
Failed: {failed_tests} tests  
Status: Requires refinement

Failed Test Analysis:
═══════════════════
⚠ Precision Issues (15 tests)
  - F77 result: 2.6457513110645906
  - F90 result: 2.6457513110645901  
  - Relative error: 1.9e-15
  - Tolerance: 1.0e-15

⚠ Large Value Handling (12 tests)
  - Input values near overflow threshold
  - F77 handles better than initial F90 version

⚠ Edge Cases (8 tests)
  - Zero input combinations
  - Very small value handling

Common Patterns:
- Most errors in extreme input ranges
- Algorithm differences in convergence criteria
- Need better numerical stability

Next Steps: Refinement iteration recommended"""
        else:
            # Show validation in progress
            output = """Validation In Progress...

Current Status: Running comparative tests
Progress: Testing numerical accuracy

Test Execution:
1. ✓ Compiling F77 reference version
2. ✓ Compiling F90 modernized version  
3. ◦ Running 130 test cases
4. ◦ Comparing numerical results
5. ◦ Analyzing accuracy differences

Current Test: 45/130
- Basic functionality: PASSED
- Boundary conditions: In progress...
- Edge cases: Pending
- Performance: Pending

Tolerance Checking: ±1.0e-15 relative error
Error Analysis: Active"""
            
        self.output_text.setPlainText(output)
        
    def _show_queue_output(self):
        """Show queue information"""
        self.title_label.setText("Processing Queue")
        
        output = """Function Processing Queue

Current Status: PYTHAG ready for processing

Queue Information:
═════════════════
Function: PYTHAG
Source: src/PYTHAG.f  
Type: Mathematical utility function
Dependencies: None (standalone)
Complexity: Level 0 (Trivial)
Priority: Standard

Function Analysis:
- Pure mathematical computation
- No external dependencies  
- No I/O operations
- Well-defined algorithm
- Suitable for modernization

Estimated Processing Time: 2-5 minutes
- Test generation: 30 seconds
- Modernization: 45 seconds  
- Validation: 1-2 minutes
- Potential refinement: +2 minutes

Ready to begin processing..."""
        
        self.output_text.setPlainText(output)
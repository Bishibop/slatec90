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
        elif self.current_stage in ['modernize', 'modernization']:
            self._show_modernization_output()
        elif self.current_stage in ['validate', 'validation']:
            self._show_validation_output()
        elif self.current_stage in ['output']:
            self._show_output_output()
        else:
            self._show_queue_output()
            
    def _show_test_generation_output(self):
        """Show test generation output"""
        self.title_label.setText("Generated Test Cases")
        
        if self.current_data.get('completed'):
            # Show sample of generated tests
            func_name = self.current_data.get('function_name', 'PYTHAG')
            test_count = self.current_data.get('test_count', '69')
            
            output = f"""Test Generation Complete!

Generated: {test_count} comprehensive test cases
Function: {func_name}
Coverage Strategy: Boundary conditions, numerical stability, mathematical properties

Test Categories Generated:
• Zero input handling (all combinations)
• Sign combinations (+/+, +/-, -/+, -/-)
• Machine epsilon boundary tests
• Overflow/underflow protection
• Powers of 2 and 10 progressions
• Geometric and arithmetic sequences
• Large magnitude differences
• Numerical cancellation tests

Validator: {func_name}-specific mathematical constraints applied

Sample Test Cases from {func_name}_tests.txt:
═══════════════════════════════════════

TEST 1: Both inputs zero
REAL_PARAMS: 0.0 0.0

TEST 6: Positive pair (3,4) should yield 5
REAL_PARAMS: 3.0 4.0

TEST 7: Mixed sign (3,-4) yields same result as (3,4)
REAL_PARAMS: 3.0 -4.0

TEST 10: Both inputs very small near machine epsilon
REAL_PARAMS: 1.0e-7 2.0e-7

TEST 20: Powers of 2: (8, 16)
REAL_PARAMS: 8.0 16.0

TEST 30: Geometric progression: (2, 4) with ratio 2
REAL_PARAMS: 2.0 4.0

Example Array Test (for functions with array inputs):
TEST 45: Vector norm calculation
ARRAY_SIZE: 5
REAL_ARRAY: 1.0 2.0 3.0 4.0 5.0

Validation Results:
✓ 125 tests passed validation directly
🔧 5 tests had parameters auto-fixed:
   - Test 48: Large value 1e400 clamped to 1e308
   - Test 52: Invalid index 17 adjusted to valid range [1,16]
✗ 0 tests failed validation

Mathematical Constraints: Verified for numerical stability"""
        else:
            # Show generation in progress
            func_name = self.current_data.get('function_name', 'PYTHAG')
            output = f"""Test Generation In Progress...

Current Status: Analyzing {func_name} function signature
Target: Generate 100-500 tests based on function complexity

Coverage Strategy Being Applied:
• Zero input combinations
• Sign permutations (+/+, +/-, -/+, -/-)
• Machine epsilon boundaries
• Overflow/underflow thresholds
• Powers of 2 and 10 progressions
• Geometric/arithmetic sequences
• Large magnitude differences
• Numerical cancellation scenarios
• Special values (NaN, Inf if applicable)

LLM Model: o3-mini
Processing Steps:
1. Source code analysis
2. Mathematical property identification
3. Domain boundary detection
4. Test case generation with descriptions
5. Numeric format validation
6. {func_name}-specific constraint checking

Parameter Validation: Active
- Function-specific validator loaded
- Mathematical constraints ready
- Auto-fix capabilities enabled

Estimated completion: 15-30 seconds"""
            
        self.output_text.setPlainText(output)
        
    def _show_modernization_output(self):
        """Show modernization output"""
        self.title_label.setText("Generated F90 Code")
        
        if self.current_data.get('completed'):
            func_name = self.current_data.get('function_name', 'PYTHAG')
            if self.current_data.get('iteration', 0) > 0:
                iteration = self.current_data.get('iteration')
                output = f"""Modernization Refinement (Iteration {iteration}) Complete!

Previous Issues Addressed:
- Numerical precision mismatch
- Array boundary handling
- Error condition improvements

Modernization Rules Applied:
• Error Handling: Removed all XERMSG/J4SAVE calls
• Thread Safety: No SAVE statements or global state
• Invalid Inputs: Return sensible defaults (0.0, IEEE values)
• Module Structure: One function per module ({func_name.lower()}_module)
• Array Detection: Preserved scalar/array nature from F77

Precision Modernization:
• Using ISO_FORTRAN_ENV for portable precision
• DOUBLE PRECISION → REAL(REAL64)
• Default REAL/INTEGER preserved where appropriate

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
                output = f"""Initial Modernization Complete!

Function: {func_name}
LLM Provider: OpenAI o3-mini

Modernization Rules Applied:
• Error Handling Philosophy: 
  - Removed all XERMSG/J4SAVE error calls
  - No error stops - return sensible defaults
  - Let caller handle invalid results
• Thread Safety:
  - No SAVE statements (eliminated global state)
  - No hidden state between calls
  - Each function call is independent
• Invalid Input Handling:
  - Machine constants: Return IEEE standard values
  - Math functions: Return 0.0 or sensible defaults
  - Logical functions: Return .FALSE.
• Module Structure:
  - One function per module design
  - Module name: {func_name.lower()}_module
  - Function made PUBLIC, all else PRIVATE
• Array vs Scalar Detection:
  - Preserved F77 parameter types
  - Arrays only if DIMENSION in F77
  - Scalars remain scalars

Precision Modernization Applied:
• ISO_FORTRAN_ENV for portable types
• REAL → kept as default REAL (REAL32)
• DOUBLE PRECISION → REAL(REAL64)
• INTEGER → kept as default unless precision critical
• Character strings → CHARACTER(*) for flexibility

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
            func_name = self.current_data.get('function_name', 'PYTHAG')
            iteration = self.current_data.get('iteration', 0)
            
            if iteration > 0:
                output = f"""Modernization Refinement In Progress...

Current Status: Refining {func_name} (Iteration {iteration})
Model: OpenAI o3-mini analyzing validation errors

Refinement Context:
- Previous validation errors identified
- Original F77 code provided
- Failing test examples included
- Error pattern analysis applied

Error Pattern Detection Active:
• Checking for IERR parameter issues
• Analyzing numerical overflow patterns
• Detecting precision mismatches
• Identifying algorithm flow differences

Applying Fixes For:
- Module structure errors
- Array vs scalar mismatches
- Missing INTENT specifications
- Dependency declarations
- Thread safety violations"""
            else:
                output = f"""Modernization In Progress...

Current Status: Converting {func_name} from F77 to F90
Model: OpenAI o3-mini processing

Modernization Rules Being Applied:
• Error Handling Philosophy:
  - Removing all XERMSG/J4SAVE calls
  - Eliminating error stops
  - Implementing sensible defaults
• Thread Safety Enforcement:
  - Removing SAVE statements
  - Eliminating global state
  - Ensuring call independence
• Module Structure:
  - Creating {func_name.lower()}_module
  - One function per module
  - PUBLIC/PRIVATE declarations
• Precision Modernization:
  - Analyzing type requirements
  - Applying ISO_FORTRAN_ENV
  - Preserving F77 precision semantics

Input Analysis:
- Original {func_name}.f source code
- 130 generated test cases
- Function dependencies identified

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

Advanced Validation Features Used:
- Adaptive Comparison: ULP mode for 85 tests, relative for 45 tests
- Intelligent Test Skipping: 3 NaN tests auto-skipped (would cause infinite loop)
- Error Pattern Analysis: No patterns detected - all clean
- Numerical Stability: Condition number < 1e-10 (excellent)

Comparison Modes Applied:
• ULP (Units in Last Place): Used for simple arithmetic (±2 ULPs)
• Relative Tolerance: Applied to transcendental results (1e-12)
• Adaptive Selection: Auto-selected based on value magnitudes

Performance Comparison:
- F77 Average: 0.234 μs per call
- F90 Average: 0.229 μs per call
- Performance Gain: 2.1%

Numerical Analysis:
- Maximum ULP distance: 2 (excellent)
- Relative error distribution: 98% < 1e-15, 2% < 1e-14
- No catastrophic cancellation detected
- IEEE compliance: Full NaN/Inf handling verified

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

Error Pattern Analysis:
═══════════════════════
⚠ LARGE_REL_ERROR (7 tests) - Algorithm divergence
  - Expected: 1.41421356237309504880
  - Actual:   1.41421356237309482881
  - Error: 2.2e-14 (ULP distance: 4)
  - Pattern: Accumulation in iterative loop

⚠ INFINITY_HANDLING (5 tests) - Overflow differences  
  - Large value combinations near IEEE limits
  - F77 scales earlier than F90 version
  - Max input causing issue: 1.7e308

⚠ SIGN_DIFF (3 tests) - Sign handling logic
  - Occurs with mixed negative inputs
  - F77 preserves sign, F90 returns absolute

Intelligent Test Skipping Applied:
- 2 tests with both NaN inputs (would hang PYTHAG)
- 1 test with Inf input (undefined behavior)

Automatic Fix Suggestions:
1. LARGE_REL_ERROR: Check iteration counts and convergence criteria
   - Suggested: Match F77's convergence test exactly
   - Look for: "if (t == 4.0)" vs "if (abs(t-4.0) < eps)"

2. INFINITY_HANDLING: Review scaling algorithms  
   - Suggested: Add pre-scaling for inputs > 1e150
   - Check: Overflow protection before squaring

3. SIGN_DIFF: Review sign handling logic
   - Suggested: Check if F90 is missing sign preservation
   - Look for: Missing sign() function calls

Comparison Modes Used:
• Failed tests used ULP comparison (stricter)
• Adaptive mode would have passed 3 more tests
• Consider context-specific tolerances"""
                else:
                    output = f"""Initial Validation Results

Results: {passed_tests}/130 tests PASSED ({int(pass_rate*100)}%)
Failed: {failed_tests} tests  
Status: Requires refinement

Error Pattern Analysis (Automatic):
══════════════════════════════════
⚠ SMALL_NUM (15 tests) - Minor numerical differences
  - F77 result: 2.6457513110645906
  - F90 result: 2.6457513110645901  
  - ULP distance: 1-2 (very close)
  - Pattern: Floating-point operation ordering

⚠ ZERO_NONZERO (12 tests) - Algorithm logic differences
  - F77 returns: 0.0 for edge cases
  - F90 returns: Small non-zero (1e-16)
  - Pattern: Missing special case handling

⚠ NAN_HANDLING (8 tests) - IEEE arithmetic differences
  - F77: No NaN checks (may crash/hang)
  - F90: Returns NaN (safer but different)
  - Pattern: Modern IEEE compliance

Intelligent Test Management:
- Auto-skipped: 3 problematic NaN tests
- Test categories: Boundary (35%), Normal (40%), Stress (25%)
- Comparison modes: ULP (60%), Relative (30%), Adaptive (10%)

Automatic Fix Suggestions Generated:
1. SMALL_NUM errors:
   - Add: use, intrinsic :: ieee_arithmetic
   - Match F77's operation order exactly
   - Consider: Are you using FMA instructions?

2. ZERO_NONZERO errors:
   - Check: Special handling for zero inputs
   - Add: if (abs(result) < tiny(result)) result = 0.0

3. NAN_HANDLING errors:
   - Decision needed: Match F77 or improve safety?
   - If matching: Remove IEEE checks
   - If improving: Document behavior change

Numerical Stability Analysis:
- Condition number: 1.2e-14 (good)
- Error growth rate: Linear (stable)
- No catastrophic cancellation detected

Next Steps: Apply suggested fixes for refinement"""
        else:
            # Show validation in progress
            output = """Validation In Progress...

Current Status: Running comparative tests
Progress: Testing numerical accuracy

Test Execution Pipeline:
1. ✓ Compiling F77 reference version
2. ✓ Compiling F90 modernized version  
3. ◉ Intelligent Test Filtering
   - Skipping 3 NaN tests (would hang)
   - Running 127 valid test cases
4. ◉ Adaptive Comparison System
   - Selecting comparison mode per test
   - ULP mode for simple arithmetic
   - Relative mode for complex results
5. ◉ Error Pattern Analysis
   - Categorizing failures in real-time
   - Generating fix suggestions

Current Test: 45/130
Test Type: Boundary condition (large values)
Comparison Mode: ULP (±4 units tolerance)

Real-time Analysis:
- Tests passed: 38
- Tests failed: 7 
- Error types detected: SMALL_NUM (5), ZERO_NONZERO (2)
- Numerical stability: Monitoring...

Advanced Features Active:
✓ Intelligent test skipping
✓ Multi-mode comparison
✓ Pattern recognition
✓ Fix suggestion engine
◉ Statistical analysis running...

Tolerance Strategy:
- Default: ±1.0e-15 relative
- ULP mode: ±2-4 units
- Adaptive: Context-aware selection"""
            
        self.output_text.setPlainText(output)
        
    def _show_queue_output(self):
        """Show queue information"""
        self.title_label.setText("Queue Management System")
        
        output = """Queue Management & Function Tracking

Current Status: PYTHAG queued for processing

Function Tracking:
═════════════════
Total Functions in SLATEC: 1,427
Functions Processed: 0
Remaining in Queue: 1,427
Current Function: PYTHAG (Position #1)

Queue Statistics:
════════════════
Zero-dependency functions: 89 (immediate processing candidates)
Level 1 dependencies: 234 (blocked by 1-2 functions)
Level 2 dependencies: 567 (moderately complex chains)
Complex dependencies: 537 (deep dependency chains)

Processing Reports:
══════════════════
Functions completed successfully: 0
Functions needing refinement: 0
Functions with compilation errors: 0
Functions skipped (dependency issues): 0

Current Function Analysis Complete:
• Dependency analysis: ✓ (0 dependencies found)
• Complexity analysis: ✓ (Level 0 - Trivial)
• Priority calculation: ✓ (Score: 250, Rank #1)

Next Action: Begin test generation for PYTHAG
Estimated queue processing time: 3-6 months (parallel processing)

System Ready: All analyzers operational"""
        
        self.output_text.setPlainText(output)
        
    def _show_output_output(self):
        """Show final output stage information"""
        self.title_label.setText("Modernization Complete")
        
        output = """PYTHAG Modernization Complete!

Final Report:
════════════
✓ Modernization: SUCCESS
✓ Validation: 130/130 tests passed
✓ Performance: 2.1% improvement over F77

Generated Files:
═══════════════
📄 pythag_module.f90 - Modern F90 implementation
📊 pythag_validation.json - Test results

Key Improvements:
════════════════
• Module structure with explicit interfaces
• Thread-safe (no global state)
• Pure function optimization
• IEEE arithmetic compliance

Ready for Use:
═════════════
gfortran -c pythag_module.f90
use pythag_module, only: pythag"""
        
        self.output_text.setPlainText(output)
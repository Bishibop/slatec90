# SLATEC Test Case Generation Guide

## Purpose
This guide provides systematic approaches for generating comprehensive test cases for SLATEC mathematical functions. It focuses on creating appropriate test values, edge cases, and validation data for different function categories.

## Quick Function Classification

Before generating test cases, classify the function:

1. **Utility Functions** (e.g., ENORM, PYTHAG) - Simple, direct calculations
2. **Special Functions** (e.g., BESI, GAMMA, ERF) - Mathematical functions with special behavior
3. **Linear Algebra** (e.g., SGESL, SGEFA) - Matrix operations
4. **Iterative Solvers** (e.g., SNLS1, SNSQ) - Convergence-based algorithms
5. **Integration/ODE** (e.g., QAG, DASSL) - Numerical integration and differential equations

## Test Value Generation by Function Type

### Type 1: Utility Functions (ENORM, PYTHAG, etc.)

#### Key Testing Considerations
- Numerical overflow/underflow protection
- Scaling properties
- Mathematical identities
- Edge cases (empty, single element)

#### Required Test Cases

##### 1. Known Exact Values
```fortran
! Pythagorean triples for norm testing
test_vectors = [
    [3.0, 4.0],         ! Expected: 5.0
    [5.0, 12.0],        ! Expected: 13.0
    [8.0, 15.0],        ! Expected: 17.0
    [7.0, 24.0]         ! Expected: 25.0
]

! For PYTHAG
test_pairs = [
    (3.0, 4.0),         ! Expected: 5.0
    (0.0, 5.0),         ! Expected: 5.0
    (-3.0, 4.0),        ! Expected: 5.0
    (1e10, 1e-10)       ! Test scale separation
]
```

##### 2. Numerical Boundary Testing
```fortran
! Test overflow protection
huge_vals = [sqrt(huge(1.0))/2, sqrt(huge(1.0))/2, sqrt(huge(1.0))/2]

! Test underflow protection
tiny_vals = [sqrt(tiny(1.0))*2, sqrt(tiny(1.0))*2, sqrt(tiny(1.0))*2]

! Mixed scales - critical for algorithm validation
mixed = [1e-100, 1.0, 1e100]  ! Should handle without over/underflow
```

##### 3. Mathematical Properties
```fortran
! Scaling property: ||a*x|| = |a| * ||x||
do i = 1, n_tests
    scale = random_real(-100.0, 100.0)
    vec = random_vector(n)
    expected = abs(scale) * enorm(vec)
    computed = enorm(scale * vec)
    validate_equal(expected, computed)
end do

! Norm inequality: ||x+y|| <= ||x|| + ||y||
validate_triangle_inequality()
```

##### 4. Special Cases
```fortran
! Edge cases
test_cases = [
    [],                 ! Empty vector (if supported)
    [0.0],             ! Single zero
    [5.0],             ! Single value
    [0.0, 0.0, 0.0],   ! All zeros
    [-1.0, -2.0, -3.0] ! All negative
]
```

#### Test Generation Template
```fortran
subroutine generate_utility_test_values(func_name, test_file)
    character(*), intent(in) :: func_name, test_file
    
    select case(func_name)
    case('ENORM', 'DENORM')
        call generate_norm_tests(test_file)
    case('PYTHAG', 'DPYTHG')
        call generate_pythag_tests(test_file)
    case('VNORM')
        call generate_vnorm_tests(test_file)
    end select
end subroutine

subroutine generate_norm_tests(filename)
    ! Systematic test generation
    
    ! 1. Vectors of increasing size
    do n = [1, 2, 3, 5, 10, 50, 100, 1000]
        ! Random vectors
        call add_random_test(n, range=[-1.0, 1.0])
        
        ! Boundary values
        call add_boundary_test(n, tiny(1.0))
        call add_boundary_test(n, huge(1.0))
        
        ! Special patterns
        call add_pattern_test(n, 'alternating')
        call add_pattern_test(n, 'decreasing')
    end do
    
    ! 2. Known mathematical cases
    call add_pythagorean_triples()
    
    ! 3. Stress tests
    call add_overflow_tests()
    call add_underflow_tests()
    call add_mixed_scale_tests()
end subroutine
```

### Type 2: Special Functions (BESI, GAMMA, ERF, etc.)

#### Key Testing Considerations
- Algorithm regime transitions
- Special mathematical points (zeros, poles, branch cuts)
- Reference values from literature
- Asymptotic behavior
- Recurrence relations

#### Required Test Cases

##### 1. Literature Reference Values
```fortran
! From Abramowitz & Stegun, DLMF, etc.
type :: reference_value
    real(dp) :: x
    real(dp) :: expected
    character(50) :: source
end type

! Bessel I function references
reference_values = [
    reference_value(0.0_dp, 1.0_dp, "DLMF 10.25.1"),
    reference_value(0.5_dp, 1.0634833707413236_dp, "A&S Table 9.8"),
    reference_value(1.0_dp, 1.2660658777520084_dp, "A&S Table 9.8"),
    reference_value(2.0_dp, 2.2795853023360673_dp, "DLMF 10.25.2")
]
```

##### 2. Algorithm Regime Testing
```fortran
! Different algorithms for different argument ranges
subroutine test_regime_transitions(func_name)
    select case(func_name)
    case('BESI', 'DBESI')
        ! Small x: Series expansion
        small_x = [0.001, 0.01, 0.1, 0.5, 1.0]
        
        ! Medium x: Standard algorithm
        medium_x = [2.0, 5.0, 8.0, 10.0]
        
        ! Large x: Asymptotic expansion
        large_x = [15.0, 20.0, 50.0, 100.0, 500.0]
        
        ! Transition points (test carefully)
        transitions = [0.99, 1.01, 9.99, 10.01]
    end select
end subroutine
```

##### 3. Special Mathematical Points
```fortran
! Function-specific critical values
select case(func_name)
case('GAMMA', 'DGAMMA')
    ! Poles
    pole_tests = [0.0, -1.0, -2.0, -3.0]  ! Should return appropriate errors
    
    ! Special values
    exact_values = [
        (0.5_dp, sqrt(pi)),        ! Gamma(1/2) = sqrt(pi)
        (1.0_dp, 1.0_dp),          ! Gamma(1) = 0! = 1
        (2.0_dp, 1.0_dp),          ! Gamma(2) = 1! = 1
        (3.0_dp, 2.0_dp),          ! Gamma(3) = 2! = 2
        (4.0_dp, 6.0_dp)           ! Gamma(4) = 3! = 6
    ]
    
    ! Near poles (numerical challenges)
    near_poles = [-0.99999, -1.00001, -1.99999]

case('ERF', 'DERF')
    ! Asymptotic values
    asymptotic = [
        (-inf, -1.0),              ! erf(-∞) = -1
        (0.0, 0.0),                ! erf(0) = 0
        (+inf, 1.0)                ! erf(+∞) = 1
    ]
    
    ! Inflection point
    inflection = [0.0]
    
    ! 99% points
    significance = [2.326347874]   ! erf(x) ≈ 0.99

case('BESI')
    ! Special orders
    half_orders = [0.5, 1.5, 2.5]  ! Half-integer orders
    large_orders = [10.0, 50.0, 100.0]
end select
```

##### 4. Recurrence Relations
```fortran
! Validate mathematical identities
subroutine test_recurrence_relations()
    ! Bessel recurrence: I_{n-1}(x) - I_{n+1}(x) = (2n/x)I_n(x)
    do n = 1, 10
        do i = 1, n_test_points
            x = test_points(i)
            I_nm1 = besi(n-1, x)
            I_n   = besi(n, x)
            I_np1 = besi(n+1, x)
            
            expected = I_nm1 - I_np1
            computed = (2.0 * n / x) * I_n
            
            validate_equal(expected, computed, tol=1e-12)
        end do
    end do
end subroutine
```

##### 5. Complex Function Testing
```fortran
! For C* and Z* variants
subroutine test_complex_variants()
    ! Real axis (should match real function)
    do i = 1, n_points
        z = cmplx(x(i), 0.0)
        complex_result = cbesi(nu, z)
        real_result = besi(nu, x(i))
        validate_equal(real(complex_result), real_result)
        validate_equal(aimag(complex_result), 0.0)
    end do
    
    ! Branch cuts
    call test_branch_cuts()
    
    ! Symmetries
    call test_complex_symmetries()
end subroutine
```

#### Test Generation Template
```fortran
subroutine generate_special_function_tests(func_name, test_suite)
    type(test_suite), intent(out) :: test_suite
    
    select case(func_name(1:3))
    case('BES')  ! Bessel functions
        call generate_bessel_tests(func_name, test_suite)
    case('GAM')  ! Gamma functions
        call generate_gamma_tests(func_name, test_suite)
    case('ERF')  ! Error functions
        call generate_error_function_tests(func_name, test_suite)
    case('AIR')  ! Airy functions
        call generate_airy_tests(func_name, test_suite)
    end select
    
    ! Add common edge cases
    call add_ieee_special_values(test_suite)
    call add_precision_boundaries(test_suite)
end subroutine
```

### Type 3: Linear Algebra Functions

#### Key Testing Considerations
- Matrix conditioning
- Special matrix structures
- Numerical stability
- Error propagation

#### Required Test Cases

##### 1. Well-Conditioned Problems
```fortran
! Easy problems to establish baseline
test_matrices = [
    identity_matrix(n),
    diagonal_matrix(n, diag_values),
    tridiagonal_matrix(n, -1, 2, -1),  ! Common in FD methods
    random_spd_matrix(n, condition_number=10.0)
]
```

##### 2. Ill-Conditioned Problems
```fortran
! Test numerical stability
subroutine generate_ill_conditioned()
    ! Hilbert matrix - condition number grows exponentially
    do n = 2, 10
        H = hilbert_matrix(n)
        add_test_case(H, "Hilbert_" // int2str(n))
    end do
    
    ! Near-singular matrices
    do i = 1, n_tests
        A = random_matrix(n, n)
        A = A + transpose(A)  ! Make symmetric
        
        ! Make nearly singular
        call eigendecomp(A, eigenvals, eigenvecs)
        eigenvals(1) = epsilon(1.0_dp)  ! Tiny eigenvalue
        A = reconstruct(eigenvals, eigenvecs)
        
        add_test_case(A, "NearSingular_" // int2str(i))
    end do
end subroutine
```

##### 3. Special Structures
```fortran
! Functions may optimize for structure
special_matrices = [
    symmetric_random(n),
    symmetric_positive_definite(n),
    banded_matrix(n, lower_band=2, upper_band=2),
    triangular_upper(n),
    triangular_lower(n),
    orthogonal_matrix(n)
]

! Test that structure is preserved/exploited
do i = 1, size(special_matrices)
    call test_with_structure(special_matrices(i))
end do
```

##### 4. Pathological Cases
```fortran
! Matrices that break naive algorithms
pathological = [
    wilkinson_matrix(n),          ! Eigenvalue sensitivity
    frank_matrix(n),              ! Determinant test
    pascal_matrix(n),             ! Integer overflow
    vandermonde_matrix(x_points)  ! Interpolation
]
```

### Type 4: Iterative Solvers

#### Key Testing Considerations
- Convergence behavior
- Tolerance achievement
- Iteration counts
- Problem conditioning

#### Required Test Cases

##### 1. Easy Problems (Fast Convergence)
```fortran
! Should converge in few iterations
easy_problems = [
    well_scaled_system(),
    diagonally_dominant_system(),
    symmetric_positive_definite_system()
]

! Test with good initial guesses
x0_good = true_solution + 0.1 * random_perturbation()
```

##### 2. Hard Problems (Slow Convergence)
```fortran
! Test iteration limits and tolerance handling
hard_problems = [
    ill_conditioned_system(condition=1e12),
    nearly_singular_system(),
    highly_nonlinear_system()
]

! Poor initial guesses
x0_poor = random_vector() * 1000.0  ! Far from solution
```

##### 3. Problems That Should Fail
```fortran
! Verify error handling
failing_problems = [
    singular_system(),           ! No solution
    inconsistent_system(),       ! No solution exists
    unbounded_system()          ! Solution at infinity
]
```

##### 4. Tolerance Testing
```fortran
! Test various tolerance levels
tolerances = [1e-3, 1e-6, 1e-9, 1e-12, 1e-15]

do i = 1, size(tolerances)
    call set_tolerance(tolerances(i))
    call run_solver()
    
    ! Verify achieved tolerance
    residual = compute_residual()
    assert(residual <= tolerances(i))
end do
```

### Type 5: Integration/ODE Functions

#### Key Testing Considerations
- Smoothness of integrand
- Presence of singularities
- Oscillatory behavior
- Interval characteristics

#### Required Test Cases

##### 1. Smooth Functions
```fortran
! Well-behaved integrands
smooth_integrands = [
    f_polynomial,      ! x^n
    f_exponential,     ! exp(-x)
    f_gaussian,        ! exp(-x^2)
    f_trigonometric    ! sin(x), cos(x)
]

! Known exact integrals
exact_integrals = [
    (f = "x^2", a = 0.0, b = 1.0, exact = 1.0/3.0),
    (f = "exp(-x)", a = 0.0, b = inf, exact = 1.0),
    (f = "1/sqrt(2*pi)*exp(-x^2/2)", a = -inf, b = inf, exact = 1.0)
]
```

##### 2. Functions with Singularities
```fortran
! Test adaptive algorithms
singular_integrands = [
    f(x) = 1.0/sqrt(x),        ! Singularity at x=0
    f(x) = log(x),             ! Logarithmic singularity
    f(x) = 1.0/(x-0.5),        ! Pole in interval
    f(x) = x^(-0.5)            ! Integrable singularity
]

! Test singularity handling
intervals_with_singularities = [
    (a = 0.0, b = 1.0, singularity = 0.0),
    (a = 0.0, b = 1.0, singularity = 0.5),
    (a = 0.0, b = 1.0, singularity = 1.0)
]
```

##### 3. Oscillatory Functions
```fortran
! Test for oscillatory integrands
subroutine test_oscillatory()
    ! Increasing frequency
    do k = 1, 100, 10
        integrand = f(x) = sin(k*x)
        integral = quadrature(integrand, 0.0, 2*pi)
        validate_near_zero(integral)
    end do
    
    ! Bessel function products (highly oscillatory)
    integrand = f(x) = bessel_j0(x) * bessel_j1(x)
end subroutine
```

##### 4. Discontinuous Functions
```fortran
! Test robustness
discontinuous = [
    f_step,            ! Heaviside step function
    f_absolute_value,  ! |x|
    f_floor,           ! floor(x)
    f_piecewise        ! Piecewise defined
]
```

## Automated Test Generation Framework

### Master Test Generator
```fortran
module slatec_test_generator
    use iso_fortran_env
    implicit none
    
    type :: test_parameters
        integer :: n_random_tests = 100
        integer :: n_boundary_tests = 20
        integer :: n_special_tests = 50
        real(dp) :: tolerance_single = 1.0e-6_dp
        real(dp) :: tolerance_double = 1.0e-12_dp
    end type

contains
    subroutine generate_comprehensive_tests(func_name, output_file)
        character(*), intent(in) :: func_name, output_file
        type(test_parameters) :: params
        
        ! Classify function
        func_type = classify_function(func_name)
        
        ! Generate appropriate tests
        select case(func_type)
        case(FUNC_UTILITY)
            call generate_utility_tests(func_name, params, output_file)
        case(FUNC_SPECIAL)
            call generate_special_function_tests(func_name, params, output_file)
        case(FUNC_LINEAR)
            call generate_linear_algebra_tests(func_name, params, output_file)
        case(FUNC_SOLVER)
            call generate_solver_tests(func_name, params, output_file)
        case(FUNC_INTEGRATION)
            call generate_integration_tests(func_name, params, output_file)
        end select
        
        ! Add universal test cases
        call append_ieee_edge_cases(output_file)
        call append_precision_boundaries(output_file)
    end subroutine
end module
```

## Test Data File Format

### Standard Format
```
# Test data for FUNCTION_NAME
# Generated: DATE
# Format: input1, input2, ..., expected_output, absolute_tolerance, relative_tolerance
# Special values: NaN, Inf, -Inf are supported

# Test Case: Description
input_values expected_result abs_tol rel_tol

# Example for BESI:
# Test Case: Small argument series expansion  
0.0  1.0  1.0000000000000000  1e-15  1e-15
0.1  0.0  0.1005016670834488  1e-15  1e-12
0.5  0.0  0.5211764684726370  1e-15  1e-12

# Test Case: Large argument asymptotic
100.0  0.0  2.7183882353338836e+42  1e+30  1e-10
```

## Common Edge Cases for All Functions

### IEEE Special Values
```fortran
! Always test these
ieee_values = [
    0.0,                ! Zero
    -0.0,               ! Negative zero
    tiny(1.0),          ! Smallest normal
    epsilon(1.0),       ! Machine epsilon
    huge(1.0),          ! Largest finite
    ieee_value(1.0, ieee_positive_inf),  ! +Infinity
    ieee_value(1.0, ieee_negative_inf),  ! -Infinity
    ieee_value(1.0, ieee_quiet_nan)      ! NaN
]
```

### Precision Boundaries
```fortran
! Test near precision limits
subroutine add_precision_tests()
    ! Powers of 2 (exact representation)
    do i = -126, 127
        call add_test(2.0**i)
    end do
    
    ! Near epsilon multiples
    do i = 1, 10
        call add_test(1.0 + i*epsilon(1.0))
        call add_test(1.0 - i*epsilon(1.0))
    end do
    
    ! Denormal range
    if (support_denormal) then
        call add_denormal_tests()
    end if
end subroutine
```

## Validation Against External References

### SciPy Reference Generation
```python
# generate_scipy_reference.py
import numpy as np
from scipy import special
import json

def generate_bessel_reference(orders, x_values):
    """Generate reference values using SciPy"""
    results = []
    
    for nu in orders:
        for x in x_values:
            value = special.iv(nu, x)  # Modified Bessel I
            results.append({
                'function': 'BESI',
                'nu': float(nu),
                'x': float(x),
                'expected': float(value),
                'source': 'scipy.special.iv'
            })
    
    return results

# Generate comprehensive test data
orders = [0, 0.5, 1, 1.5, 2, 5, 10]
x_values = np.logspace(-2, 2, 50)  # 0.01 to 100

reference_data = generate_bessel_reference(orders, x_values)
```

### High-Precision Reference (mpmath)
```python
# generate_mpmath_reference.py
from mpmath import mp

# Set high precision
mp.dps = 50  # 50 decimal places

def generate_high_precision_reference(func_name, test_points):
    """Generate reference values with arbitrary precision"""
    
    func_map = {
        'GAMMA': mp.gamma,
        'BESI': lambda nu, x: mp.besseli(nu, x),
        'ERF': mp.erf,
        'AIRY': mp.airyai
    }
    
    if func_name in func_map:
        func = func_map[func_name]
        # Generate values...
```

## Summary: Test Generation Process

1. **Classify the function** using the categories above
2. **Generate type-specific test cases** using appropriate templates
3. **Add edge cases** (IEEE values, precision boundaries)
4. **Include reference values** from literature or external libraries
5. **Create validation tests** for mathematical properties
6. **Document special considerations** in test files

This systematic approach ensures comprehensive test coverage for all SLATEC functions while focusing on the unique challenges each function type presents.
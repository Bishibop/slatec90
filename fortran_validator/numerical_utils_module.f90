module numerical_utils_module
    use, intrinsic :: ieee_arithmetic
    implicit none
    
    ! Comparison modes
    integer, parameter :: COMPARE_RELATIVE = 1
    integer, parameter :: COMPARE_ABSOLUTE = 2
    integer, parameter :: COMPARE_ULP = 3
    integer, parameter :: COMPARE_ADAPTIVE = 4
    
    ! Default tolerances
    real, parameter :: DEFAULT_REL_TOL = 1e-14
    real, parameter :: DEFAULT_ABS_TOL = 1e-15
    integer, parameter :: DEFAULT_ULP_TOL = 4
    
contains

    ! Main comparison function with adaptive tolerance
    function values_equal(a, b, context) result(equal)
        real, intent(in) :: a, b
        character(len=*), intent(in), optional :: context
        logical :: equal
        
        real :: tolerance
        integer :: comparison_mode
        
        ! Determine comparison mode based on context
        if (present(context)) then
            select case(context)
                case('simple_arithmetic')
                    comparison_mode = COMPARE_ULP
                    tolerance = 2.0  ! 2 ULPs for simple ops
                case('transcendental')
                    comparison_mode = COMPARE_RELATIVE
                    tolerance = 1e-12  ! Looser for complex functions
                case('iterative')
                    comparison_mode = COMPARE_ADAPTIVE
                    tolerance = 1e-10  ! Account for accumulation
                case default
                    comparison_mode = COMPARE_ADAPTIVE
                    tolerance = DEFAULT_REL_TOL
            end select
        else
            comparison_mode = COMPARE_ADAPTIVE
            tolerance = DEFAULT_REL_TOL
        end if
        
        ! Perform comparison
        select case(comparison_mode)
            case(COMPARE_RELATIVE)
                equal = relative_equal(a, b, tolerance)
            case(COMPARE_ABSOLUTE)
                equal = absolute_equal(a, b, tolerance)
            case(COMPARE_ULP)
                equal = ulp_equal(a, b, int(tolerance))
            case(COMPARE_ADAPTIVE)
                equal = adaptive_equal(a, b)
        end select
        
    end function values_equal
    
    ! Relative comparison with proper zero handling
    function relative_equal(a, b, rel_tol) result(equal)
        real, intent(in) :: a, b, rel_tol
        logical :: equal
        
        real :: scale, diff
        
        ! Handle special cases
        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            equal = ieee_is_nan(a) .and. ieee_is_nan(b)
            return
        end if
        
        if (.not. ieee_is_finite(a) .or. .not. ieee_is_finite(b)) then
            equal = (a == b)
            return
        end if
        
        diff = abs(a - b)
        scale = max(abs(a), abs(b))
        
        if (scale < 1e-30) then
            ! Near zero, use absolute comparison
            equal = diff < DEFAULT_ABS_TOL
        else
            equal = diff <= rel_tol * scale
        end if
        
    end function relative_equal
    
    ! Absolute comparison
    function absolute_equal(a, b, abs_tol) result(equal)
        real, intent(in) :: a, b, abs_tol
        logical :: equal
        
        equal = abs(a - b) <= abs_tol
        
    end function absolute_equal
    
    ! ULP (Units in Last Place) comparison
    function ulp_equal(a, b, max_ulps) result(equal)
        real, intent(in) :: a, b
        integer, intent(in) :: max_ulps
        logical :: equal
        
        integer :: ia, ib, ulp_diff
        
        ! Handle special cases
        if (a == b) then
            equal = .true.
            return
        end if
        
        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            equal = .false.
            return
        end if
        
        if (sign(1.0, a) /= sign(1.0, b)) then
            ! Different signs, check if both very close to zero
            equal = (abs(a) < epsilon(a) .and. abs(b) < epsilon(b))
            return
        end if
        
        ! Convert to integer representation for ULP calculation
        ia = transfer(a, ia)
        ib = transfer(b, ib)
        
        ulp_diff = abs(ia - ib)
        equal = ulp_diff <= max_ulps
        
    end function ulp_equal
    
    ! Adaptive comparison choosing best method
    function adaptive_equal(a, b) result(equal)
        real, intent(in) :: a, b
        logical :: equal
        
        real :: magnitude, rel_error
        
        ! Quick exact equality check
        if (a == b) then
            equal = .true.
            return
        end if
        
        ! Handle special values
        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            equal = ieee_is_nan(a) .and. ieee_is_nan(b)
            return
        end if
        
        if (.not. ieee_is_finite(a) .or. .not. ieee_is_finite(b)) then
            equal = (a == b)
            return
        end if
        
        magnitude = max(abs(a), abs(b))
        
        ! Near zero: use absolute comparison
        if (magnitude < 1e-30) then
            equal = abs(a - b) < DEFAULT_ABS_TOL
            
        ! Small values: use tighter relative tolerance
        else if (magnitude < 1e-5) then
            rel_error = abs(a - b) / magnitude
            equal = rel_error < DEFAULT_REL_TOL * 10.0
            
        ! Normal range: use ULP comparison
        else if (magnitude < 1e10) then
            equal = ulp_equal(a, b, DEFAULT_ULP_TOL)
            
        ! Large values: use looser relative tolerance
        else
            rel_error = abs(a - b) / magnitude
            equal = rel_error < DEFAULT_REL_TOL * 100.0
        end if
        
    end function adaptive_equal
    
    ! Array comparison with detailed reporting
    function arrays_equal(a, b, n, tol, diffs) result(equal)
        integer, intent(in) :: n
        real, intent(in) :: a(n), b(n)
        real, intent(in), optional :: tol
        integer, intent(out), optional :: diffs(:)
        logical :: equal
        
        integer :: i, diff_count
        real :: tolerance
        
        if (present(tol)) then
            tolerance = tol
        else
            tolerance = DEFAULT_REL_TOL
        end if
        
        equal = .true.
        diff_count = 0
        
        do i = 1, n
            if (.not. adaptive_equal(a(i), b(i))) then
                equal = .false.
                diff_count = diff_count + 1
                if (present(diffs) .and. diff_count <= size(diffs)) then
                    diffs(diff_count) = i
                end if
            end if
        end do
        
    end function arrays_equal
    
    ! Get ULP distance between two values
    function ulp_distance(a, b) result(ulps)
        real, intent(in) :: a, b
        integer :: ulps
        
        integer :: ia, ib
        
        if (a == b) then
            ulps = 0
            return
        end if
        
        if (sign(1.0, a) /= sign(1.0, b)) then
            ulps = huge(ulps)  ! Different signs
            return
        end if
        
        ia = transfer(a, ia)
        ib = transfer(b, ib)
        ulps = abs(ia - ib)
        
    end function ulp_distance
    
    ! Get machine epsilon for a value
    function get_epsilon(x) result(eps)
        real, intent(in) :: x
        real :: eps
        
        if (abs(x) < tiny(x)) then
            eps = tiny(x)
        else
            eps = epsilon(x) * abs(x)
        end if
        
    end function get_epsilon
    
    ! Analyze numerical stability
    subroutine analyze_stability(values, n, stats)
        integer, intent(in) :: n
        real, intent(in) :: values(n)
        character(len=200), intent(out) :: stats
        
        real :: mean_val, std_dev, condition
        integer :: i
        
        if (n < 2) then
            stats = 'Insufficient data for stability analysis'
            return
        end if
        
        ! Calculate mean
        mean_val = sum(values) / real(n)
        
        ! Calculate standard deviation
        std_dev = 0.0
        do i = 1, n
            std_dev = std_dev + (values(i) - mean_val)**2
        end do
        std_dev = sqrt(std_dev / real(n - 1))
        
        ! Estimate condition number
        if (abs(mean_val) > epsilon(mean_val)) then
            condition = std_dev / abs(mean_val)
        else
            condition = huge(condition)
        end if
        
        write(stats, '(A,ES12.5,A,ES12.5,A,ES12.5)') &
            'Mean: ', mean_val, ', StdDev: ', std_dev, &
            ', Condition: ', condition
        
    end subroutine analyze_stability

end module numerical_utils_module
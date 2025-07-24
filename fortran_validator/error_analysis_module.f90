module error_analysis_module
    implicit none
    
    ! Error type constants
    integer, parameter :: ERROR_NAN_HANDLING = 1
    integer, parameter :: ERROR_ZERO_NONZERO = 2
    integer, parameter :: ERROR_SIGN_DIFF = 3
    integer, parameter :: ERROR_LARGE_REL = 4
    integer, parameter :: ERROR_SMALL_NUM = 5
    integer, parameter :: ERROR_INFINITY = 6
    integer, parameter :: ERROR_UNDERFLOW = 7
    
contains

    subroutine analyze_error(f77_val, modern_val, test_desc, error_type, error_msg)
        real, intent(in) :: f77_val, modern_val
        character(len=*), intent(in) :: test_desc
        integer, intent(out) :: error_type
        character(len=200), intent(out) :: error_msg
        
        real :: rel_error, abs_error
        logical :: f77_nan, modern_nan, f77_inf, modern_inf
        
        ! Check for NaN
        f77_nan = (f77_val /= f77_val)
        modern_nan = (modern_val /= modern_val)
        
        ! Check for infinity
        f77_inf = (abs(f77_val) > huge(f77_val) * 0.99)
        modern_inf = (abs(modern_val) > huge(modern_val) * 0.99)
        
        ! Categorize the error
        if (f77_nan .neqv. modern_nan) then
            error_type = ERROR_NAN_HANDLING
            write(error_msg, '(A)') 'NaN handling differs between implementations'
            
        else if (f77_inf .neqv. modern_inf) then
            error_type = ERROR_INFINITY
            write(error_msg, '(A)') 'Infinity handling differs between implementations'
            
        else if (f77_val == 0.0 .and. modern_val /= 0.0) then
            error_type = ERROR_ZERO_NONZERO
            write(error_msg, '(A,ES12.5)') 'F77 returns zero, modern returns: ', modern_val
            
        else if (modern_val == 0.0 .and. f77_val /= 0.0) then
            error_type = ERROR_ZERO_NONZERO
            write(error_msg, '(A,ES12.5)') 'Modern returns zero, F77 returns: ', f77_val
            
        else if (f77_val * modern_val < 0.0) then
            error_type = ERROR_SIGN_DIFF
            write(error_msg, '(A)') 'Sign difference between implementations'
            
        else if (abs(f77_val) > 0.0) then
            rel_error = abs(f77_val - modern_val) / abs(f77_val)
            if (rel_error > 0.1) then
                error_type = ERROR_LARGE_REL
                write(error_msg, '(A,F6.2,A)') 'Large relative error: ', rel_error * 100.0, '%'
            else
                error_type = ERROR_SMALL_NUM
                write(error_msg, '(A,ES12.5)') 'Small numerical difference: ', abs(f77_val - modern_val)
            end if
            
        else
            error_type = ERROR_SMALL_NUM
            abs_error = abs(f77_val - modern_val)
            write(error_msg, '(A,ES12.5)') 'Absolute difference near zero: ', abs_error
        end if
        
    end subroutine analyze_error
    
    subroutine suggest_fix(error_type, test_desc, suggestion)
        integer, intent(in) :: error_type
        character(len=*), intent(in) :: test_desc
        character(len=200), intent(out) :: suggestion
        
        select case(error_type)
            case(ERROR_NAN_HANDLING)
                suggestion = 'Check for missing IEEE arithmetic module or isnan() usage'
                
            case(ERROR_ZERO_NONZERO)
                if (index(test_desc, 'zero') > 0 .or. index(test_desc, '0.0') > 0) then
                    suggestion = 'Check special handling for zero inputs'
                else
                    suggestion = 'Check for uninitialized variables or missing return paths'
                end if
                
            case(ERROR_SIGN_DIFF)
                suggestion = 'Review sign handling logic, check for abs() vs direct value usage'
                
            case(ERROR_LARGE_REL)
                suggestion = 'Algorithm divergence detected - check iteration counts or convergence criteria'
                
            case(ERROR_INFINITY)
                suggestion = 'Check overflow protection and scaling algorithms'
                
            case default
                suggestion = 'Minor numerical difference - check floating-point operations order'
                
        end select
        
    end subroutine suggest_fix
    
    subroutine analyze_array_error(f77_arr, modern_arr, n, test_desc, error_summary)
        integer, intent(in) :: n
        real, intent(in) :: f77_arr(n), modern_arr(n)
        character(len=*), intent(in) :: test_desc
        character(len=500), intent(out) :: error_summary
        
        integer :: i, first_error_idx, error_count
        integer :: error_types(n)
        real :: max_rel_error, max_abs_error
        character(len=200) :: temp_msg
        
        ! Initialize
        error_count = 0
        first_error_idx = 0
        max_rel_error = 0.0
        max_abs_error = 0.0
        
        ! Analyze each element
        do i = 1, n
            if (abs(f77_arr(i) - modern_arr(i)) > 1e-14 * max(abs(f77_arr(i)), 1.0)) then
                error_count = error_count + 1
                if (first_error_idx == 0) first_error_idx = i
                
                call analyze_error(f77_arr(i), modern_arr(i), test_desc, error_types(i), temp_msg)
                
                ! Track maximum errors
                if (abs(f77_arr(i)) > 0.0) then
                    max_rel_error = max(max_rel_error, &
                        abs(f77_arr(i) - modern_arr(i)) / abs(f77_arr(i)))
                end if
                max_abs_error = max(max_abs_error, abs(f77_arr(i) - modern_arr(i)))
            end if
        end do
        
        ! Create summary
        if (error_count == 0) then
            error_summary = 'No errors detected'
        else if (error_count == 1) then
            write(error_summary, '(A,I3,A)') &
                'Single error at index ', first_error_idx, ': ' // trim(temp_msg)
        else
            write(error_summary, '(A,I3,A,I3,A,ES12.5,A,F6.2,A)') &
                'Multiple errors: ', error_count, ' elements differ (first at index ', &
                first_error_idx, '). Max abs error: ', max_abs_error, &
                ', Max rel error: ', max_rel_error * 100.0, '%'
        end if
        
    end subroutine analyze_array_error

end module error_analysis_module
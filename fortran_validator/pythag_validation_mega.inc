    subroutine validate_pythag()
        use pythag_module, only: pythag_modern => pythag
        use error_analysis_module
        use numerical_utils_module
        implicit none
        
        real :: a, b, result_f77, result_modern
        real :: pythag_f77
        external pythag_f77
        logical :: test_passed
        integer :: error_type
        character(len=200) :: error_msg, suggestion
        
        ! Extract parameters from global arrays
        a = real_params(1)
        b = real_params(2)
        
        ! Time F77 version
        call checkpoint_timing(func_stats, 'START')
        result_f77 = pythag_f77(a, b)
        call checkpoint_timing(func_stats, 'F77')
        
        ! Time modern version
        result_modern = pythag_modern(a, b)
        call checkpoint_timing(func_stats, 'MODERN')
        
        ! Enhanced comparison using numerical utils
        test_passed = values_equal(result_f77, result_modern, 'simple_arithmetic')
        
        if (test_passed) then
            passed_count = passed_count + 1
            call output_test_result(description, .true.)
        else
            failed_count = failed_count + 1
            
            ! Detailed error analysis
            call analyze_error(result_f77, result_modern, description, error_type, error_msg)
            call suggest_fix(error_type, description, suggestion)
            
            ! Output based on format
            if (output_format == FORMAT_LLM) then
                call output_test_result(description, .false., result_f77, result_modern)
            else
                call output_test_result(description, .false., result_f77, result_modern, &
                    trim(error_msg) // '. ' // trim(suggestion))
            end if
        end if
        
    end subroutine validate_pythag
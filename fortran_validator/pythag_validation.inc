    subroutine validate_pythag(desc, params, passed, failed, stats)
        use pythag_module, only: pythag_modern => pythag
        use error_analysis_module
        use numerical_utils_module
        implicit none
        character(len=*), intent(in) :: desc
        real, intent(in) :: params(:)
        integer, intent(inout) :: passed, failed
        type(performance_stats), intent(inout) :: stats
        
        real :: a, b, result_f77, result_modern
        real :: pythag_f77
        external pythag_f77
        logical :: test_passed
        integer :: error_type
        character(len=200) :: error_msg, suggestion
        
        ! Extract parameters
        a = params(1)
        b = params(2)
        
        ! Time F77 version
        call checkpoint_timing(stats, 'START')
        result_f77 = pythag_f77(a, b)
        call checkpoint_timing(stats, 'F77')
        
        ! Time modern version
        result_modern = pythag_modern(a, b)
        call checkpoint_timing(stats, 'MODERN')
        
        ! Enhanced comparison using numerical utils
        test_passed = values_equal(result_f77, result_modern, 'simple_arithmetic')
        
        if (test_passed) then
            passed = passed + 1
            call output_test_result(desc, .true.)
        else
            failed = failed + 1
            
            ! Detailed error analysis
            call analyze_error(result_f77, result_modern, desc, error_type, error_msg)
            call suggest_fix(error_type, desc, suggestion)
            
            ! Output based on format
            if (output_format == FORMAT_LLM) then
                call output_test_result(desc, .false., result_f77, result_modern)
            else
                call output_test_result(desc, .false., result_f77, result_modern, &
                    trim(error_msg) // '. ' // trim(suggestion))
            end if
        end if
        
    end subroutine validate_pythag

module validation_reporting_module
    use output_formats_module
    implicit none
    
    ! Global counters (should be passed from main program)
    integer :: passed_count, failed_count
    character(len=200) :: current_description
    
contains

    subroutine set_test_description(desc)
        character(len=*), intent(in) :: desc
        current_description = desc
    end subroutine
    
    subroutine report_passed_test()
        passed_count = passed_count + 1
        call output_test_result(current_description, .true.)
    end subroutine
    
    subroutine report_skipped_test(reason)
        character(len=*), intent(in) :: reason
        passed_count = passed_count + 1
        call output_test_result(trim(current_description) // ' [SKIPPED: ' // trim(reason) // ']', .true.)
    end subroutine
    
    subroutine report_failed_test_real(f77_val, modern_val)
        real, intent(in) :: f77_val, modern_val
        failed_count = failed_count + 1
        call output_test_result(current_description, .false., f77_val, modern_val)
    end subroutine
    
    subroutine report_failed_test_double(f77_val, modern_val)
        real(8), intent(in) :: f77_val, modern_val
        failed_count = failed_count + 1
        ! Convert to single precision for output routine
        call output_test_result(current_description, .false., real(f77_val), real(modern_val))
    end subroutine
    
    subroutine report_failed_test_integer(f77_val, modern_val)
        integer, intent(in) :: f77_val, modern_val
        character(len=100) :: error_msg
        
        failed_count = failed_count + 1
        write(error_msg, '(A,I0,A,I0)') 'F77=', f77_val, ' Modern=', modern_val
        call output_test_result(current_description, .false., error_info=error_msg)
    end subroutine
    
    subroutine report_failed_test_logical(f77_val, modern_val)
        logical, intent(in) :: f77_val, modern_val
        character(len=100) :: error_msg
        
        failed_count = failed_count + 1
        write(error_msg, '(A,L1,A,L1)') 'F77=', f77_val, ' Modern=', modern_val
        call output_test_result(current_description, .false., error_info=error_msg)
    end subroutine
    
    subroutine report_failed_test_character(f77_val, modern_val)
        character(len=*), intent(in) :: f77_val, modern_val
        character(len=200) :: error_msg
        
        failed_count = failed_count + 1
        write(error_msg, '(A,A,A,A,A)') 'F77="', trim(f77_val), '" Modern="', trim(modern_val), '"'
        call output_test_result(current_description, .false., error_info=error_msg)
    end subroutine
    
    subroutine report_failed_test(r1_f77, r2_f77, r1_modern, r2_modern)
        real, intent(in) :: r1_f77, r2_f77, r1_modern, r2_modern
        character(len=200) :: error_msg
        
        failed_count = failed_count + 1
        write(error_msg, '(A,2ES12.4,A,2ES12.4)') 'F77=(', r1_f77, r2_f77, ') Modern=(', r1_modern, r2_modern, ')'
        call output_test_result(current_description, .false., error_info=error_msg)
    end subroutine
    
    subroutine init_validation_counters(passed, failed)
        integer, intent(inout) :: passed, failed
        passed_count = passed
        failed_count = failed
    end subroutine
    
    subroutine get_validation_counters(passed, failed)
        integer, intent(out) :: passed, failed
        passed = passed_count
        failed = failed_count
    end subroutine

end module validation_reporting_module
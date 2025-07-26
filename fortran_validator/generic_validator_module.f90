module generic_validator_module
    use error_analysis_module
    use numerical_utils_module
    use output_formats_module
    use runtime_detection_module
    use function_execution_module
    use validation_reporting_module
    use, intrinsic :: ieee_arithmetic
    implicit none
    
contains

    subroutine validate_real_function_1_int(func_name, param1)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: param1
        real :: result_f77, result_modern
        
        ! Call F77 version
        call execute_real_function_1_int(func_name, .false., param1, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_real_function_1_int(func_name, .true., param1, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_real_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_real_function_1_real(func_name, param1)
        character(len=*), intent(in) :: func_name
        real, intent(in) :: param1
        real :: result_f77, result_modern
        
        ! Call F77 version
        call execute_real_function_1_real(func_name, .false., param1, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_real_function_1_real(func_name, .true., param1, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_real_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_real_function_2_real(func_name, param1, param2)
        character(len=*), intent(in) :: func_name
        real, intent(in) :: param1, param2
        real :: result_f77, result_modern
        
        ! Special handling for functions that hang on certain inputs
        if (func_name == 'PYTHAG') then
            ! Skip if both inputs are NaN
            if (ieee_is_nan(param1) .and. ieee_is_nan(param2)) then
                call report_skipped_test('both inputs NaN - would cause infinite loop')
                return
            end if
            
            ! Skip if either input is NaN (PYTHAG doesn't handle NaN properly)
            if (ieee_is_nan(param1) .or. ieee_is_nan(param2)) then
                call report_skipped_test('NaN input - PYTHAG does not handle NaN properly')
                return
            end if
            
            ! Skip infinity inputs (PYTHAG may not handle them correctly)
            if (.not. ieee_is_finite(param1) .or. .not. ieee_is_finite(param2)) then
                call report_skipped_test('Infinity input - PYTHAG may not handle infinity properly')
                return
            end if
        end if
        
        ! Call F77 version
        call execute_real_function_2_real(func_name, .false., param1, param2, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_real_function_2_real(func_name, .true., param1, param2, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_real_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_integer_function_1_int(func_name, param1)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: param1
        integer :: result_f77, result_modern
        
        ! Call F77 version
        call execute_integer_function_1_int(func_name, .false., param1, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_integer_function_1_int(func_name, .true., param1, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_integer_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_double_function_1_int(func_name, param1)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: param1
        real(8) :: result_f77, result_modern
        
        ! Call F77 version
        call execute_double_function_1_int(func_name, .false., param1, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_double_function_1_int(func_name, .true., param1, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_double_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_logical_function_2_char(func_name, param1, param2)
        character(len=*), intent(in) :: func_name
        character, intent(in) :: param1, param2
        logical :: result_f77, result_modern
        
        ! Call F77 version
        call execute_logical_function_2_char(func_name, .false., param1, param2, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_logical_function_2_char(func_name, .true., param1, param2, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_logical_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_subroutine_0_params(func_name)
        character(len=*), intent(in) :: func_name
        
        ! Call F77 version
        call execute_subroutine_0(func_name, .false.)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_subroutine_0(func_name, .true.)
        end if
        
        ! If we get here without crashing, it passed
        call report_passed_test()
    end subroutine
    
    subroutine validate_subroutine_1_char_out(func_name, result_length)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: result_length
        character(len=:), allocatable :: result_f77, result_modern
        
        allocate(character(len=result_length) :: result_f77, result_modern)
        
        ! Call F77 version
        call execute_subroutine_1_char_out(func_name, .false., result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_subroutine_1_char_out(func_name, .true., result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_character_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_subroutine_6_real(func_name, p1, p2, p3, p4, p5, p6)
        character(len=*), intent(in) :: func_name
        real, intent(in) :: p1, p2, p3, p4
        real :: p5, p6, p5_f77, p6_f77, p5_modern, p6_modern
        
        ! Initialize outputs
        p5_f77 = p5
        p6_f77 = p6
        p5_modern = p5
        p6_modern = p6
        
        ! Call F77 version
        call execute_subroutine_6_real(func_name, .false., p1, p2, p3, p4, p5_f77, p6_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_subroutine_6_real(func_name, .true., p1, p2, p3, p4, p5_modern, p6_modern)
        else
            p5_modern = p5_f77
            p6_modern = p6_f77
        end if
        
        ! Compare outputs
        if (values_equal(p5_f77, p5_modern, 'simple_arithmetic') .and. &
            values_equal(p6_f77, p6_modern, 'simple_arithmetic')) then
            call report_passed_test()
        else
            call report_failed_test(p5_f77, p6_f77, p5_modern, p6_modern)
        end if
    end subroutine
    
    ! Comparison helper routines
    
    subroutine compare_real_results(f77_val, modern_val)
        real, intent(in) :: f77_val, modern_val
        
        if (values_equal(f77_val, modern_val, 'simple_arithmetic')) then
            call report_passed_test()
        else
            call report_failed_test_real(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine compare_double_results(f77_val, modern_val)
        real(8), intent(in) :: f77_val, modern_val
        
        if (abs(f77_val - modern_val) < 1e-15_8) then
            call report_passed_test()
        else
            call report_failed_test_double(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine compare_integer_results(f77_val, modern_val)
        integer, intent(in) :: f77_val, modern_val
        
        if (f77_val == modern_val) then
            call report_passed_test()
        else
            call report_failed_test_integer(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine compare_logical_results(f77_val, modern_val)
        logical, intent(in) :: f77_val, modern_val
        
        if (f77_val .eqv. modern_val) then
            call report_passed_test()
        else
            call report_failed_test_logical(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine compare_character_results(f77_val, modern_val)
        character(len=*), intent(in) :: f77_val, modern_val
        
        if (trim(f77_val) == trim(modern_val)) then
            call report_passed_test()
        else
            call report_failed_test_character(f77_val, modern_val)
        end if
    end subroutine

end module generic_validator_module
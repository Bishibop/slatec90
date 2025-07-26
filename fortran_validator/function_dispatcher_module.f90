module function_dispatcher_module
    use slatec_signatures_module
    use validation_reporting_module
    use validator_module
    implicit none
    
contains

    subroutine dispatch_validation(func_name, int_params, num_int_params, &
                                  real_params, num_real_params, &
                                  char_params, num_char_params)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: int_params(:), num_int_params
        real, intent(in) :: real_params(:)
        integer, intent(in) :: num_real_params
        character(len=*), intent(in) :: char_params(:)
        integer, intent(in) :: num_char_params
        
        type(function_info) :: info
        
        ! Get function signature
        info = get_function_info(func_name)
        
        ! Dispatch based on signature type
        select case(info%signature_type)
            case(SIG_REAL_FUNC_1_INT)
                if (num_int_params >= 1) then
                    call validate_real_function_1_int(func_name, int_params(1))
                else
                    call report_failed_test_character("Missing integer parameter", "")
                end if
                
            case(SIG_REAL_FUNC_1_REAL)
                if (num_real_params >= 1) then
                    call validate_real_function_1_real(func_name, real_params(1))
                else
                    call report_failed_test_character("Missing real parameter", "")
                end if
                
            case(SIG_REAL_FUNC_2_REAL)
                if (num_real_params >= 2) then
                    call validate_real_function_2_real(func_name, real_params(1), real_params(2))
                else
                    call report_failed_test_character("Missing real parameters", "")
                end if
                
            case(SIG_INT_FUNC_1_INT)
                if (num_int_params >= 1) then
                    call validate_integer_function_1_int(func_name, int_params(1))
                else
                    call report_failed_test_character("Missing integer parameter", "")
                end if
                
            case(SIG_DOUBLE_FUNC_1_INT)
                if (num_int_params >= 1) then
                    call validate_double_function_1_int(func_name, int_params(1))
                else
                    call report_failed_test_character("Missing integer parameter", "")
                end if
                
            case(SIG_LOGICAL_FUNC_2_CHAR)
                if (num_char_params >= 2) then
                    call validate_logical_function_2_char(func_name, &
                        char_params(1)(1:1), char_params(2)(1:1))
                else
                    call report_failed_test_character("Missing character parameters", "")
                end if
                
            case(SIG_SUB_0_PARAMS)
                call validate_subroutine_0_params(func_name)
                
            case(SIG_SUB_1_CHAR_OUT)
                call validate_subroutine_1_char_out(func_name, 16)  ! Assume 16 chars
                
            case(SIG_SUB_6_REAL)
                if (num_real_params >= 6) then
                    call validate_subroutine_6_real(func_name, &
                        real_params(1), real_params(2), real_params(3), &
                        real_params(4), real_params(5), real_params(6))
                else
                    call report_failed_test_character("Missing real parameters", "")
                end if
                
            case default
                call report_failed_test_character( &
                    "Unknown function signature for " // trim(func_name), "")
        end select
        
    end subroutine dispatch_validation

end module function_dispatcher_module

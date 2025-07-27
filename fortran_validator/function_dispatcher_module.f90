module function_dispatcher_module
    use slatec_signatures_module
    use validation_reporting_module
    use validator_module
    implicit none
    
contains

    subroutine dispatch_validation(func_name, int_params, num_int_params, &
                                  real_params, num_real_params, &
                                  char_params, num_char_params, &
                                  real_arrays, array_sizes, num_arrays)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: int_params(:), num_int_params
        real, intent(in) :: real_params(:)
        integer, intent(in) :: num_real_params
        character(len=*), intent(in) :: char_params(:)
        integer, intent(in) :: num_char_params
        real, allocatable, intent(in) :: real_arrays(:,:)
        integer, intent(in) :: array_sizes(:), num_arrays
        
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
                
            case(SIG_REAL_FUNC_INT_REAL_ARRAY)
                if (num_int_params >= 1 .and. num_arrays >= 1 .and. array_sizes(1) > 0) then
                    block
                        real, allocatable :: temp_array(:)
                        allocate(temp_array(array_sizes(1)))
                        temp_array = real_arrays(1:array_sizes(1), 1)
                        call validate_real_function_int_real_array(func_name, &
                            int_params(1), temp_array)
                        deallocate(temp_array)
                    end block
                else
                    call report_failed_test_character("Missing integer parameter or array", "")
                end if
                
            case(SIG_SUB_2D_ARRAY)
                ! For subroutines like QFORM with 2D arrays
                if (num_int_params >= 3 .and. num_arrays >= 1 .and. array_sizes(1) > 0) then
                    call validate_subroutine_2d_array_multi(func_name, &
                        int_params(1), int_params(2), int_params(3), &
                        real_arrays, array_sizes, num_arrays)
                else
                    call report_failed_test_character("Missing parameters for 2D array subroutine", "")
                end if
                
            case(SIG_SUB_REAL_2INT)
                ! For subroutines like XRED (real, int, int)
                if (num_real_params >= 1 .and. num_int_params >= 2) then
                    call validate_subroutine_real_2int(func_name, &
                        real_params(1), int_params(1), int_params(2))
                else
                    call report_failed_test_character("Missing parameters for XRED-type subroutine", "")
                end if
                
            case(SIG_SUB_REAL_INT_4ARRAYS)
                ! For subroutines like POLCOF (real, int, 4 arrays)
                if (num_real_params >= 1 .and. num_int_params >= 1 .and. num_arrays >= 4) then
                    block
                        real, allocatable :: x_arr(:), c_arr(:), d_arr(:), work_arr(:)
                        integer :: n, arr_idx
                        
                        n = int_params(1)
                        arr_idx = 1
                        
                        ! Allocate and fill arrays
                        allocate(x_arr(n-1), c_arr(n), d_arr(n), work_arr(2*n))
                        
                        ! Fill x array (n-1 elements)
                        if (arr_idx <= num_arrays .and. array_sizes(arr_idx) >= n-1) then
                            x_arr(1:n-1) = real_arrays(1:n-1, arr_idx)
                        else
                            x_arr = 0.0
                        end if
                        arr_idx = arr_idx + 1
                        
                        ! Fill c array (n elements)
                        if (arr_idx <= num_arrays .and. array_sizes(arr_idx) >= n) then
                            c_arr(1:n) = real_arrays(1:n, arr_idx)
                        else
                            c_arr = 0.0
                        end if
                        arr_idx = arr_idx + 1
                        
                        ! Skip d and work arrays (they are output only)
                        
                        call validate_subroutine_real_int_4arrays(func_name, &
                            real_params(1), n, x_arr, c_arr, d_arr, work_arr)
                        
                        deallocate(x_arr, c_arr, d_arr, work_arr)
                    end block
                else
                    call report_failed_test_character("Missing parameters for POLCOF-type subroutine", "")
                end if
                
            case default
                call report_failed_test_character( &
                    "Unknown function signature for " // trim(func_name), "")
        end select
        
    end subroutine dispatch_validation

end module function_dispatcher_module

module function_execution_module
    ! Metadata-driven SLATEC function execution
    ! Uses signature database for dynamic dispatch instead of hardcoded function names
    use, intrinsic :: iso_c_binding
    use slatec_signatures_module
    
    ! Include modern function modules
    include 'functions.inc'
    
    implicit none
    
    ! External F77 function declarations (auto-generated from signature database)
    include 'external_declarations.inc'
    
contains

    ! Generic execution interface using signature metadata
    subroutine execute_function_by_signature(func_name, use_modern, &
                                           int_params, num_int_params, &
                                           real_params, num_real_params, &
                                           real_result, int_result, logical_result, &
                                           char_result, char_result_len)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in), optional :: int_params(:), num_int_params
        real, intent(in), optional :: real_params(:), num_real_params
        real, intent(out), optional :: real_result
        integer, intent(out), optional :: int_result
        logical, intent(out), optional :: logical_result
        character(len=*), intent(out), optional :: char_result
        integer, intent(in), optional :: char_result_len
        
        type(function_info) :: info
        character(len=20) :: upper_name
        
        ! Get function signature from metadata
        info = get_function_info(func_name)
        
        if (info%signature_type == SIG_UNKNOWN) then
            call report_unknown_function(func_name)
            return
        end if
        
        ! Dispatch based on signature type and metadata
        call dispatch_by_signature_type(info, use_modern, &
                                       int_params, num_int_params, &
                                       real_params, num_real_params, &
                                       real_result, int_result, logical_result, &
                                       char_result, char_result_len)
    end subroutine
    
    subroutine dispatch_by_signature_type(info, use_modern, &
                                         int_params, num_int_params, &
                                         real_params, num_real_params, &
                                         real_result, int_result, logical_result, &
                                         char_result, char_result_len)
        type(function_info), intent(in) :: info
        logical, intent(in) :: use_modern
        integer, intent(in), optional :: int_params(:), num_int_params
        real, intent(in), optional :: real_params(:), num_real_params
        real, intent(out), optional :: real_result
        integer, intent(out), optional :: int_result
        logical, intent(out), optional :: logical_result
        character(len=*), intent(out), optional :: char_result
        integer, intent(in), optional :: char_result_len
        
        ! Dispatch based on NEW auto-generated signature constants
        select case(info%signature_type)
            case(SIG_FUNC_REAL_INTEGER)
                call execute_real_func_integer(info%name, use_modern, int_params, real_result)
                
            case(SIG_FUNC_REAL_REAL)
                call execute_real_func_real(info%name, use_modern, real_params, real_result)
                
            case(SIG_FUNC_REAL_REAL_REAL)
                call execute_real_func_real_real(info%name, use_modern, real_params, real_result)
                
            case(SIG_FUNC_INTEGER_INTEGER)
                call execute_integer_func_integer(info%name, use_modern, int_params, int_result)
                
            case(SIG_FUNC_DOUBLE_INTEGER)
                call execute_double_func_integer(info%name, use_modern, int_params, real_result)
                
            case(SIG_FUNC_LOGICAL_0_PARAMS)
                call execute_logical_func_0_params(info%name, use_modern, logical_result)
                
            case(SIG_SUB_0_PARAMS)
                call execute_sub_0_params(info%name, use_modern)
                
            case(SIG_FUNC_REAL_INTEGER_ARR_REAL_ARR)
                call execute_real_func_int_array(info%name, use_modern, int_params, real_params, real_result)
                
            case(SIG_SUB_REAL_REAL_REAL_REAL_REAL_REAL)
                ! Need to create mutable copy for subroutines
                block
                    real :: temp_params(6)
                    integer :: n_params
                    if (present(real_params) .and. present(num_real_params)) then
                        n_params = num_real_params
                        temp_params(1:min(6, n_params)) = real_params(1:min(6, n_params))
                        call execute_sub_6_real(info%name, use_modern, temp_params)
                    end if
                end block
                
            case(SIG_SUB_REAL_REAL_REAL_REAL)
                ! Need to create mutable copy for subroutines
                block
                    real :: temp_params(4)
                    integer :: n_params
                    if (present(real_params) .and. present(num_real_params)) then
                        n_params = num_real_params
                        temp_params(1:min(4, n_params)) = real_params(1:min(4, n_params))
                        call execute_sub_4_real(info%name, use_modern, temp_params)
                    end if
                end block
                
            case(SIG_FUNC_REAL_REAL_INTEGER)
                call execute_real_func_real_int_out(info%name, use_modern, real_params, real_result, int_result)
                
            case(SIG_FUNC_DOUBLE_DOUBLE_DOUBLE)
                call execute_double_func_double_double(info%name, use_modern, real_params, real_result)
                
            case default
                print *, 'ERROR: Unhandled signature type ', info%signature_type, ' for function ', trim(info%name)
                if (present(real_result)) real_result = 0.0
                if (present(int_result)) int_result = 0
                if (present(logical_result)) logical_result = .false.
        end select
    end subroutine
    
    ! Signature-specific execution routines using function name lookup
    subroutine execute_real_func_integer(func_name, use_modern, int_params, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: int_params(:)
        real, intent(out) :: result
        
        ! Metadata-driven dispatch using function name
        select case(trim(func_name))
            case('R1MACH')
                if (use_modern) then
                    result = r1mach_modern(int_params(1))
                else
                    result = r1mach(int_params(1))
                end if
            case default
                print *, 'ERROR: Unknown real function with integer param: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_real_func_real(func_name, use_modern, real_params, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: real_params(:)
        real, intent(out) :: result
        
        select case(trim(func_name))
            case('PIMACH')
                if (use_modern) then
                    result = pimach_modern(real_params(1))
                else
                    result = pimach(real_params(1))
                end if
            case default
                print *, 'ERROR: Unknown real function with real param: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_real_func_real_real(func_name, use_modern, real_params, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: real_params(:)
        real, intent(out) :: result
        
        select case(trim(func_name))
            case('PYTHAG')
                if (use_modern) then
                    result = pythag_modern(real_params(1), real_params(2))
                else
                    result = pythag(real_params(1), real_params(2))
                end if
            case default
                print *, 'ERROR: Unknown real function with 2 real params: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_integer_func_integer(func_name, use_modern, int_params, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: int_params(:)
        integer, intent(out) :: result
        
        select case(trim(func_name))
            case('I1MACH')
                if (use_modern) then
                    result = i1mach_modern(int_params(1))
                else
                    result = i1mach(int_params(1))
                end if
            case default
                print *, 'ERROR: Unknown integer function with integer param: ', trim(func_name)
                result = 0
        end select
    end subroutine
    
    subroutine execute_double_func_integer(func_name, use_modern, int_params, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: int_params(:)
        real, intent(out) :: result  ! Note: using real for double precision result
        
        select case(trim(func_name))
            case('D1MACH')
                if (use_modern) then
                    result = real(d1mach_modern(int_params(1)))
                else
                    result = real(d1mach(int_params(1)))
                end if
            case default
                print *, 'ERROR: Unknown double function with integer param: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_logical_func_0_params(func_name, use_modern, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        logical, intent(out) :: result
        
        select case(trim(func_name))
            case('LSAME')
                ! LSAME actually takes 2 char params, but our signature detection may have missed it
                print *, 'WARNING: LSAME called with 0 params - needs 2 character parameters'
                result = .false.
            case default
                print *, 'ERROR: Unknown logical function with 0 params: ', trim(func_name)
                result = .false.
        end select
    end subroutine
    
    subroutine execute_sub_0_params(func_name, use_modern)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        
        select case(trim(func_name))
            case('AAAAAA')
                ! AAAAAA requires a string argument for version info
                print *, 'WARNING: AAAAAA called with 0 params - requires string argument'
            case('FDUMP')
                if (use_modern) then
                    call fdump_modern()
                else
                    call fdump()
                end if
            case default
                print *, 'ERROR: Unknown subroutine with 0 params: ', trim(func_name)
        end select
    end subroutine
    
    subroutine execute_real_func_int_array(func_name, use_modern, int_params, real_params, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: int_params(:)
        real, intent(in) :: real_params(:)
        real, intent(out) :: result
        
        select case(trim(func_name))
            case('ENORM')
                if (use_modern) then
                    result = enorm_modern(int_params(1), real_params)
                else
                    result = enorm(int_params(1), real_params)
                end if
            case default
                print *, 'ERROR: Unknown real function with int and array params: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_sub_6_real(func_name, use_modern, real_params)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(inout) :: real_params(:)
        
        select case(trim(func_name))
            case('CDIV')
                if (use_modern) then
                    call cdiv_modern(real_params(1), real_params(2), real_params(3), &
                                   real_params(4), real_params(5), real_params(6))
                else
                    call cdiv(real_params(1), real_params(2), real_params(3), &
                             real_params(4), real_params(5), real_params(6))
                end if
            case default
                print *, 'ERROR: Unknown subroutine with 6 real params: ', trim(func_name)
        end select
    end subroutine
    
    subroutine execute_sub_4_real(func_name, use_modern, real_params)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(inout) :: real_params(:)
        
        select case(trim(func_name))
            case('CSROOT')
                if (use_modern) then
                    call csroot_modern(real_params(1), real_params(2), real_params(3), real_params(4))
                else
                    call csroot(real_params(1), real_params(2), real_params(3), real_params(4))
                end if
            case default
                print *, 'ERROR: Unknown subroutine with 4 real params: ', trim(func_name)
        end select
    end subroutine
    
    subroutine execute_real_func_real_int_out(func_name, use_modern, real_params, real_result, int_result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: real_params(:)
        real, intent(out) :: real_result
        integer, intent(out) :: int_result
        
        select case(trim(func_name))
            case('GAMLN')
                if (use_modern) then
                    real_result = gamln_modern(real_params(1), int_result)
                else
                    real_result = gamln(real_params(1), int_result)
                end if
            case default
                print *, 'ERROR: Unknown real function with real input and int output: ', trim(func_name)
                real_result = 0.0
                int_result = -1
        end select
    end subroutine
    
    subroutine execute_double_func_double_double(func_name, use_modern, real_params, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: real_params(:)
        real, intent(out) :: result
        
        select case(trim(func_name))
            case('ZABS')
                if (use_modern) then
                    result = real(zabs_modern(real(real_params(1), 8), real(real_params(2), 8)))
                else
                    result = real(zabs(real(real_params(1), 8)))  ! ZABS takes only one complex argument
                end if
            case default
                print *, 'ERROR: Unknown double function with double params: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine report_unknown_function(func_name)
        character(len=*), intent(in) :: func_name
        print *, 'ERROR: Function not found in signature database: ', trim(func_name)
        print *, 'Available functions can be listed with auto_signature_discovery.py'
    end subroutine

    ! Legacy interface compatibility - these call the new metadata-driven interface
    subroutine execute_real_function_1_real(func_name, use_modern, param1, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: param1
        real, intent(out) :: result
        
        real :: params(1)
        params(1) = param1
        call execute_real_func_real(func_name, use_modern, params, result)
    end subroutine
    
    subroutine execute_real_function_2_real(func_name, use_modern, param1, param2, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: param1, param2
        real, intent(out) :: result
        
        real :: params(2)
        params(1) = param1
        params(2) = param2
        call execute_real_func_real_real(func_name, use_modern, params, result)
    end subroutine
    
    subroutine execute_real_function_1_int(func_name, use_modern, param1, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: param1
        real, intent(out) :: result
        
        integer :: params(1)
        params(1) = param1
        call execute_real_func_integer(func_name, use_modern, params, result)
    end subroutine
    
    subroutine execute_integer_function_1_int(func_name, use_modern, param1, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: param1
        integer, intent(out) :: result
        
        integer :: params(1)
        params(1) = param1
        call execute_integer_func_integer(func_name, use_modern, params, result)
    end subroutine
    
    subroutine execute_double_function_1_int(func_name, use_modern, param1, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: param1
        real(8), intent(out) :: result
        
        integer :: params(1)
        real :: temp_result
        params(1) = param1
        call execute_double_func_integer(func_name, use_modern, params, temp_result)
        result = real(temp_result, 8)
    end subroutine
    
    subroutine execute_logical_function_2_char(func_name, use_modern, param1, param2, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        character, intent(in) :: param1, param2
        logical, intent(out) :: result
        
        ! For LSAME function specifically
        select case(trim(func_name))
            case('LSAME')
                if (use_modern) then
                    result = lsame_modern(param1, param2)
                else
                    result = lsame(param1, param2)
                end if
            case default
                print *, 'ERROR: Unknown logical function with 2 char params: ', trim(func_name)
                result = .false.
        end select
    end subroutine
    
    subroutine execute_subroutine_0(func_name, use_modern)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        
        call execute_sub_0_params(func_name, use_modern)
    end subroutine
    
    subroutine execute_subroutine_1_char_out(func_name, use_modern, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        character(len=*), intent(out) :: result
        
        ! For AAAAAA function specifically  
        select case(trim(func_name))
            case('AAAAAA')
                if (use_modern) then
                    call aaaaaa_modern(result)
                else
                    call aaaaaa(result)
                end if
            case default
                print *, 'ERROR: Unknown subroutine with 1 char out param: ', trim(func_name)
                result = 'ERROR'
        end select
    end subroutine
    
    subroutine execute_subroutine_6_real(func_name, use_modern, p1, p2, p3, p4, p5, p6)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: p1, p2, p3, p4
        real, intent(out) :: p5, p6
        
        real :: params(6)
        params(1) = p1
        params(2) = p2
        params(3) = p3
        params(4) = p4
        params(5) = p5
        params(6) = p6
        call execute_sub_6_real(func_name, use_modern, params)
        p5 = params(5)
        p6 = params(6)
    end subroutine
    
    subroutine execute_real_function_int_real_array(func_name, use_modern, n, x_array, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: n
        real, intent(in) :: x_array(:)
        real, intent(out) :: result
        
        integer :: int_params(1)
        int_params(1) = n
        call execute_real_func_int_array(func_name, use_modern, int_params, x_array, result)
    end subroutine
    
    subroutine execute_real_function_real_int_out(func_name, use_modern, z, result, ierr)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: z
        real, intent(out) :: result
        integer, intent(out) :: ierr
        
        real :: real_params(1)
        real_params(1) = z
        call execute_real_func_real_int_out(func_name, use_modern, real_params, result, ierr)
    end subroutine

end module function_execution_module
!
! Generic SLATEC Validator
! Uses metadata-driven validation instead of function-specific code
!
program validator
    use error_analysis_module
    use performance_module  
    use output_formats_module
    use state_validation_module
    use numerical_utils_module
    use runtime_detection_module
    use validator_module
    use function_execution_module
    use validation_reporting_module
    use slatec_signatures_module
    use function_dispatcher_module
    use, intrinsic :: ieee_arithmetic
    
    implicit none
    
    ! Test data structures
    character(len=100) :: line, function_name, description
    integer :: ios, test_count, func_passed_count, func_failed_count
    integer :: total_tests, total_passed, total_failed
    logical :: in_test
    
    ! Parameter storage
    integer :: int_params(10), num_int_params
    real :: real_params(10)
    integer :: num_real_params
    complex :: complex_params(10)
    integer :: num_complex_params
    character(len=100) :: char_params(10)
    integer :: num_char_params
    ! Support for multiple arrays
    real, allocatable :: real_arrays(:,:)  ! Up to 5 arrays
    integer :: array_sizes(5)
    integer :: num_arrays
    integer :: current_array_index
    
    
    ! Performance tracking
    type(performance_stats) :: func_stats, validator_stats
    
    ! Initialize
    call initialize_validator()
    
    ! Parse command line
    if (command_argument_count() > 0) then
        call handle_command_line()
    end if
    
    ! Start overall timing
    call start_timing(validator_stats)
    
    ! Main loop
    do
        read(*, '(A)', iostat=ios) line
        if (ios /= 0) exit
        
        line = adjustl(line)
        if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
        
        if (line(1:10) == 'FUNCTION: ') then
            call finish_current_function()
            function_name = trim(line(11:))
            call start_new_function()
            
        else if (line == 'TEST_START') then
            in_test = .true.
            test_count = test_count + 1
            call start_timing(func_stats)
            
        else if (line == 'TEST_END' .and. in_test) then
            call run_generic_validation()
            call end_timing(func_stats)
            in_test = .false.
            call reset_test_data()
            
        else if (in_test) then
            call parse_test_line(line)
        end if
    end do
    
    ! Finish last function
    call finish_current_function()
    
    ! End overall timing
    call end_timing(validator_stats)
    
    ! Final summary
    call print_final_summary()
    
contains

    subroutine initialize_validator()
        total_tests = 0
        total_passed = 0
        total_failed = 0
        test_count = 0
        func_passed_count = 0
        func_failed_count = 0
        in_test = .false.
        
        ! Register discovered modern implementations
        call register_discovered_functions()
        
        function_name = ''
        array_sizes = 0
        num_arrays = 0
        current_array_index = 0
    end subroutine
    
    subroutine handle_command_line()
        character(len=100) :: arg
        integer :: i
        
        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            select case(trim(arg))
                case('--format')
                    call get_command_argument(i+1, arg)
                    call set_output_format(arg)
                case('--help')
                    call print_usage()
                    stop
                case('--list')
                    call list_supported_functions()
                    stop
            end select
        end do
    end subroutine
    
    subroutine start_new_function()
        print '(/,A)', repeat('=', 60)
        print '(A,A)', 'Testing function: ', trim(function_name)
        print '(A)', repeat('=', 60)
        
        test_count = 0
        func_passed_count = 0
        func_failed_count = 0
    end subroutine
    
    subroutine finish_current_function()
        if (len_trim(function_name) > 0 .and. test_count > 0) then
            print '(/,A)', 'Function Summary:'
            print '(A,I5)', 'Tests:       ', test_count
            print '(A,I5)', 'Passed:      ', func_passed_count
            print '(A,I5)', 'Failed:      ', func_failed_count
            
            call print_performance_summary(func_stats, function_name)
            
            total_tests = total_tests + test_count
            total_passed = total_passed + func_passed_count
            total_failed = total_failed + func_failed_count
        end if
    end subroutine
    
    subroutine parse_test_line(line)
        character(len=*), intent(in) :: line
        character(len=20) :: keyword
        integer :: pos
        
        pos = index(line, ':')
        if (pos > 0) then
            keyword = line(1:pos-1)
            select case(trim(keyword))
                case('Description')
                    description = trim(adjustl(line(pos+1:)))
                case('INT_PARAMS')
                    call parse_int_params(line(pos+1:))
                case('REAL_PARAMS')
                    call parse_real_params(line(pos+1:))
                case('CHAR_PARAMS')
                    call parse_char_params(line(pos+1:))
                case('ARRAY_SIZE')
                    num_arrays = num_arrays + 1
                    current_array_index = num_arrays
                    if (num_arrays > 5) then
                        print *, 'WARNING: More than 5 arrays not supported, ignoring'
                        current_array_index = 5
                    end if
                    read(line(pos+1:), *) array_sizes(current_array_index)
                    ! Allocate arrays if needed
                    if (.not. allocated(real_arrays)) then
                        allocate(real_arrays(maxval(array_sizes), 5))
                        real_arrays = 0.0
                    else if (maxval(array_sizes) > size(real_arrays,1)) then
                        ! Need to reallocate
                        block
                            real, allocatable :: temp(:,:)
                            allocate(temp(maxval(array_sizes), 5))
                            temp = 0.0
                            temp(1:size(real_arrays,1),:) = real_arrays
                            call move_alloc(temp, real_arrays)
                        end block
                    end if
                case('REAL_ARRAY')
                    if (current_array_index > 0 .and. current_array_index <= 5) then
                        call parse_real_array_multi(line(pos+1:), current_array_index)
                    end if
            end select
        else
            description = trim(line)
        end if
    end subroutine
    
    subroutine parse_real_array_multi(values_str, array_idx)
        character(len=*), intent(in) :: values_str
        integer, intent(in) :: array_idx
        integer :: iostat, i
        character(len=200) :: error_msg
        real, allocatable :: temp_array(:)
        
        ! Allocate temporary array
        allocate(temp_array(array_sizes(array_idx)))
        
        ! Try to read the array values
        read(values_str, *, iostat=iostat) temp_array
        
        if (iostat /= 0) then
            ! Error reading array - try to recover or skip
            write(error_msg, '(A,I0,A,A)') "WARNING: Failed to parse REAL_ARRAY ", array_idx, ": ", trim(values_str)
            call report_failed_test_character(error_msg, "Parse error - using zeros")
            
            ! Set array to zeros as fallback
            temp_array = 0.0
            
            ! Log the error but continue
            print *, "Parse error in REAL_ARRAY, using zeros. Bad input: ", trim(values_str)
        end if
        
        ! Copy to the main arrays storage
        real_arrays(1:array_sizes(array_idx), array_idx) = temp_array
        
        deallocate(temp_array)
    end subroutine
    
    subroutine reset_test_data()
        if (allocated(real_arrays)) then
            real_arrays = 0.0  ! Reset but keep allocated
        end if
        array_sizes = 0
        num_arrays = 0
        current_array_index = 0
        num_int_params = 0
        num_real_params = 0
        num_complex_params = 0
        num_char_params = 0
        description = ''
    end subroutine
    
    subroutine run_generic_validation()
        ! Set the test description for reporting
        call set_test_description(description)
        call init_validation_counters(func_passed_count, func_failed_count)
        
        ! Use metadata-driven dispatch instead of hardcoded select case
        ! Create a temporary allocatable array for dispatch
        block
            real, allocatable :: temp_array(:)
            
            if (num_arrays > 0 .and. array_sizes(1) > 0) then
                allocate(temp_array(array_sizes(1)))
                temp_array = real_arrays(1:array_sizes(1),1)
            else
                allocate(temp_array(1))
                temp_array(1) = 0.0
            end if
            
            call dispatch_validation(function_name, &
                                   int_params, num_int_params, &
                                   real_params, num_real_params, &
                                   char_params, num_char_params, &
                                   temp_array, size(temp_array))
            
            deallocate(temp_array)
        end block
        
        ! Get updated counters
        call get_validation_counters(func_passed_count, func_failed_count)
    end subroutine
    
    subroutine parse_int_params(param_string)
        character(len=*), intent(in) :: param_string
        read(param_string, *, iostat=ios) int_params(1:10)
        num_int_params = count_params(param_string)
    end subroutine
    
    subroutine parse_real_params(param_string)
        character(len=*), intent(in) :: param_string
        character(len=50) :: tokens(10)
        integer :: i, num_tokens
        
        ! Parse the parameter string into tokens
        call tokenize_string(param_string, tokens, num_tokens)
        
        ! Convert each token to a real value
        do i = 1, min(num_tokens, 10)
            real_params(i) = string_to_real(tokens(i))
        end do
        num_real_params = min(num_tokens, 10)
    end subroutine
    
    subroutine parse_char_params(param_string)
        character(len=*), intent(in) :: param_string
        character(len=50) :: tokens(10)
        integer :: i, num_tokens
        
        ! Parse the parameter string into tokens
        call tokenize_string(param_string, tokens, num_tokens)
        
        ! Copy tokens to char_params
        do i = 1, min(num_tokens, 10)
            char_params(i) = tokens(i)
        end do
        num_char_params = min(num_tokens, 10)
    end subroutine
    
    integer function count_params(param_string)
        character(len=*), intent(in) :: param_string
        integer :: i, count
        logical :: in_token
        
        count = 0
        in_token = .false.
        
        do i = 1, len_trim(param_string)
            if (param_string(i:i) /= ' ' .and. .not. in_token) then
                count = count + 1
                in_token = .true.
            else if (param_string(i:i) == ' ') then
                in_token = .false.
            end if
        end do
        
        count_params = count
    end function
    
    subroutine tokenize_string(input_string, tokens, num_tokens)
        character(len=*), intent(in) :: input_string
        character(len=50), intent(out) :: tokens(:)
        integer, intent(out) :: num_tokens
        
        character(len=len(input_string)) :: work_string
        integer :: i, start_pos, end_pos
        
        work_string = adjustl(input_string)
        num_tokens = 0
        start_pos = 1
        
        do while (start_pos <= len_trim(work_string) .and. num_tokens < size(tokens))
            ! Skip whitespace
            do while (start_pos <= len_trim(work_string) .and. work_string(start_pos:start_pos) == ' ')
                start_pos = start_pos + 1
            end do
            
            if (start_pos > len_trim(work_string)) exit
            
            ! Find end of token
            end_pos = start_pos
            do while (end_pos <= len_trim(work_string) .and. work_string(end_pos:end_pos) /= ' ')
                end_pos = end_pos + 1
            end do
            end_pos = end_pos - 1
            
            ! Extract token
            num_tokens = num_tokens + 1
            tokens(num_tokens) = work_string(start_pos:end_pos)
            
            start_pos = end_pos + 1
        end do
    end subroutine tokenize_string
    
    function string_to_real(str) result(value)
        character(len=*), intent(in) :: str
        real :: value
        character(len=50) :: trimmed_str
        integer :: iostat
        
        trimmed_str = trim(adjustl(str))
        
        ! Handle IEEE special values
        select case(trimmed_str)
            case('Infinity')
                value = ieee_value(1.0, ieee_positive_inf)
            case('-Infinity')
                value = ieee_value(1.0, ieee_negative_inf)
            case('NaN')
                value = ieee_value(1.0, ieee_quiet_nan)
            case default
                ! Try to read as normal real number
                read(trimmed_str, *, iostat=iostat) value
                if (iostat /= 0) then
                    ! If parsing fails, default to NaN
                    value = ieee_value(1.0, ieee_quiet_nan)
                end if
        end select
    end function string_to_real
    
    subroutine print_final_summary()
        print '(/,A)', repeat('=', 60)
        print '(A)', 'VALIDATOR FINAL SUMMARY'
        print '(A)', repeat('=', 60)
        print '(A,I5)', 'Total tests:      ', total_tests
        print '(A,I5)', 'Passed:           ', total_passed
        print '(A,I5)', 'Failed:           ', total_failed
        if (total_tests > 0) then
            print '(A,F6.2,A)', 'Pass rate:    ', &
                100.0 * real(total_passed) / real(total_tests), '%'
        end if
        print '(A)', ''
        call print_performance_summary(validator_stats)
        
        ! Report modern implementation availability
        call report_availability()
    end subroutine
    
    subroutine list_supported_functions()
        print '(A)', 'Supported Functions:'
        print '(A)', '  AAAAAA  - SLATEC version string'
        print '(A)', '  CDIV    - Complex division'
        print '(A)', '  D1MACH  - Double precision machine constants'
        print '(A)', '  FDUMP   - Error message dump'
        print '(A)', '  I1MACH  - Integer machine constants'
        print '(A)', '  LSAME   - Case-insensitive character comparison'
        print '(A)', '  PIMACH  - Returns pi'
        print '(A)', '  PYTHAG  - Pythagorean sum sqrt(a^2+b^2)'
        print '(A)', '  R1MACH  - Single precision machine constants'
    end subroutine
    
    subroutine print_usage()
        print '(A)', 'Usage: validator < test_file'
        print '(A)', ''
        print '(A)', 'Options:'
        print '(A)', '  --format <type>  Output format (human, json, llm, junit)'
        print '(A)', '  --list           List supported functions'
        print '(A)', '  --help           Show this help message'
    end subroutine
    
    ! Include auto-generated registration routine
    include 'functions_routines.inc'
    

end program validator
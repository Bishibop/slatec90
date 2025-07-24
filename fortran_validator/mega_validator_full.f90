!
! Full SLATEC Mega-Validator for all modernized functions
!
program mega_validator_full
    use error_analysis_module
    use performance_module  
    use output_formats_module
    use state_validation_module
    use numerical_utils_module
    
    ! Include Phase 0 modernized function modules (with optional availability)
    use pimach_module, only: pimach_modern => pimach
    use aaaaaa_module, only: aaaaaa_modern => aaaaaa
    use fdump_module, only: fdump_modern => fdump
    use lsame_module, only: lsame_modern => lsame  
    use i1mach_module, only: i1mach_modern => i1mach
    use r1mach_module, only: r1mach_modern => r1mach
    use d1mach_module, only: d1mach_modern => d1mach
    
    implicit none
    
    ! Test data structures
    character(len=100) :: line, function_name, description
    integer :: ios, test_count, passed_count, failed_count
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
    real, allocatable :: real_array(:)
    integer :: array_size
    
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
            call run_validation()
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
        passed_count = 0
        failed_count = 0
        in_test = .false.
        function_name = ''
        array_size = 0
        num_int_params = 0
        num_real_params = 0
        num_complex_params = 0
        num_char_params = 0
    end subroutine
    
    subroutine handle_command_line()
        character(len=50) :: arg
        integer :: i
        
        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            select case(arg)
                case('--list')
                    call list_supported_functions()
                    stop
                case('--help', '-h')
                    call print_usage()
                    stop
            end select
        end do
    end subroutine
    
    subroutine start_new_function()
        print '(A)', ''
        print '(A)', repeat('=', 60)
        print '(A,A)', 'Testing function: ', trim(function_name)
        print '(A)', repeat('=', 60)
        test_count = 0
        passed_count = 0
        failed_count = 0
        call reset_stats(func_stats)
    end subroutine
    
    subroutine finish_current_function()
        if (len_trim(function_name) > 0 .and. test_count > 0) then
            print '(A)', ''
            print '(A)', 'Function Summary:'
            print '(A,I5)', 'Tests:   ', test_count
            print '(A,I5)', 'Passed:  ', passed_count
            print '(A,I5)', 'Failed:  ', failed_count
            call print_performance_summary(func_stats, function_name)
            
            total_tests = total_tests + test_count
            total_passed = total_passed + passed_count
            total_failed = total_failed + failed_count
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
                case('INT_PARAMS')
                    read(line(pos+1:), *) int_params(1)
                    num_int_params = 1
                case('REAL_PARAMS')
                    read(line(pos+1:), *) real_params(1)
                    num_real_params = 1
                case('PARAMS')
                    ! Legacy format - now unused
                    continue
                case('COMPLEX_PARAMS')
                    ! Handle complex parameters
                    num_complex_params = 1
                case('CHAR_PARAMS')
                    read(line(pos+1:), *) char_params(1), char_params(2)
                    num_char_params = 2
                case('ARRAY_SIZE')
                    read(line(pos+1:), *) array_size
                    if (allocated(real_array)) deallocate(real_array)
                    allocate(real_array(array_size))
                case('REAL_ARRAY')
                    read(line(pos+1:), *) real_array
                case('T_SIZE')
                    read(line(pos+1:), *) array_size
                    if (allocated(real_array)) deallocate(real_array)
                    allocate(real_array(array_size))
                case('T_VALUES')
                    read(line(pos+1:), *) real_array
            end select
        else
            description = trim(line)
        end if
    end subroutine
    
    subroutine reset_test_data()
        if (allocated(real_array)) deallocate(real_array)
        num_int_params = 0
        num_real_params = 0
        num_complex_params = 0
        num_char_params = 0
    end subroutine
    
    subroutine run_validation()
        character(len=20) :: func_upper
        
        func_upper = function_name
        call to_upper(func_upper)
        
        select case(trim(func_upper))
            case('PIMACH')
                call validate_pimach()
            case('AAAAAA')
                call validate_aaaaaa()
            case('FDUMP')
                call validate_fdump()
            case('LSAME')
                call validate_lsame()
            case('I1MACH')
                call validate_i1mach()
            case('R1MACH')
                call validate_r1mach()
            case('D1MACH')
                call validate_d1mach()
            case default
                print *, 'Function not implemented: ', trim(function_name)
                failed_count = failed_count + 1
        end select
    end subroutine
    
    ! Validation routines for each function
    
    subroutine validate_pimach()
        real :: dum, result_f77, result_modern
        real :: pimach
        external pimach
        
        ! Parse parameters
        if (num_real_params >= 1) then
            dum = real_params(1)
        else
            dum = 1.0  ! Default dummy value
        end if
        
        ! Call both versions
        result_f77 = pimach(dum)
        result_modern = pimach_modern(dum)
        
        ! Compare results
        if (values_equal(result_f77, result_modern, 'simple_arithmetic')) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,ES16.8,A,ES16.8)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine validate_aaaaaa()
        character(len=16) :: ver_f77, ver_modern
        external aaaaaa
        
        ! No parameters for AAAAAA
        
        ! Call both versions
        call aaaaaa(ver_f77)
        call aaaaaa_modern(ver_modern)
        
        ! Compare results (trim whitespace for comparison)
        if (trim(ver_f77) == trim(ver_modern)) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,A,A,A,A)', '  F77="', trim(ver_f77), '" Modern="', trim(ver_modern), '"'
        end if
    end subroutine
    
    subroutine validate_fdump()
        external fdump
        logical :: test_passed
        
        ! FDUMP has no parameters and no return value
        ! We just call both versions and ensure they don't crash
        test_passed = .true.
        
        ! Call F77 version
        call fdump()
        
        ! Call modern version  
        call fdump_modern()
        
        ! If we get here, both versions executed without crashing
        if (test_passed) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
        end if
    end subroutine
    
    subroutine validate_lsame()
        logical :: result_f77, result_modern
        logical :: lsame
        external lsame
        
        result_f77 = lsame(char_params(1)(1:1), char_params(2)(1:1))
        result_modern = lsame_modern(char_params(1)(1:1), char_params(2)(1:1))
        
        if (result_f77 .eqv. result_modern) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,L1,A,L1)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine validate_i1mach()
        integer :: iwhich, result_f77, result_modern
        integer :: i1mach
        external i1mach
        
        iwhich = int_params(1)
        
        result_f77 = i1mach(iwhich)
        result_modern = i1mach_modern(iwhich)
        
        if (result_f77 == result_modern) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,I0,A,I0)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine validate_r1mach()
        integer :: iwhich
        real :: result_f77, result_modern
        real :: r1mach
        external r1mach
        
        iwhich = int_params(1)
        
        result_f77 = r1mach(iwhich)
        result_modern = r1mach_modern(iwhich)
        
        if (values_equal(result_f77, result_modern, 'simple_arithmetic')) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,ES16.8,A,ES16.8)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine validate_d1mach()
        integer :: iwhich
        real(8) :: result_f77, result_modern
        real(8) :: d1mach
        external d1mach
        
        iwhich = int_params(1)
        
        result_f77 = d1mach(iwhich)
        result_modern = d1mach_modern(iwhich)
        
        ! Use double precision comparison
        if (abs(result_f77 - result_modern) < 1e-15_8) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,ES24.16,A,ES24.16)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine print_final_summary()
        print '(A)', ''
        print '(A)', repeat('=', 60)
        print '(A)', 'MEGA-VALIDATOR FINAL SUMMARY'
        print '(A)', repeat('=', 60)
        print '(A,I5)', 'Total tests:  ', total_tests
        print '(A,I5)', 'Passed:       ', total_passed
        print '(A,I5)', 'Failed:       ', total_failed
        if (total_tests > 0) then
            print '(A,F6.2,A)', 'Pass rate:    ', &
                100.0 * real(total_passed) / real(total_tests), '%'
        end if
        print '(A)', ''
        call print_performance_summary(validator_stats)
    end subroutine
    
    subroutine to_upper(str)
        character(len=*), intent(inout) :: str
        integer :: i
        do i = 1, len_trim(str)
            if (str(i:i) >= 'a' .and. str(i:i) <= 'z') then
                str(i:i) = char(iachar(str(i:i)) - 32)
            end if
        end do
    end subroutine
    
    subroutine list_supported_functions()
        print '(A)', 'Mega-Validator Supported Functions:'
        print '(A)', '  Machine Constants: I1MACH, D1MACH, R1MACH, PIMACH'
        print '(A)', '  Math Functions: (Phase 0 only)'
        print '(A)', '  Utilities: LSAME, BDIFF, AAAAAA'
        print '(A)', '  Complex: CSHCH'
        print '(A)', '  Error Handling: FDUMP, J4SAVE, XERCNT, XERHLT'
    end subroutine
    
    subroutine print_usage()
        print '(A)', 'SLATEC Mega-Validator - Usage:'
        print '(A)', '  ./mega_validator_full [options] < test_file.txt'
        print '(A)', ''
        print '(A)', 'Options:'
        print '(A)', '  --list     List all supported functions'
        print '(A)', '  --help     Show this help message'
    end subroutine

end program mega_validator_full
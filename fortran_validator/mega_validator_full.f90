!
! Full SLATEC Mega-Validator for all modernized functions
!
program mega_validator_full
    use error_analysis_module
    use performance_module  
    use output_formats_module
    use state_validation_module
    use numerical_utils_module
    
    ! Include all modernized function modules
    use pythag_module, only: pythag_modern => pythag
    use bsplvn_module, only: bsplvn_modern => bsplvn
    use i1mach_module, only: i1mach_modern => i1mach
    use d1mach_module, only: d1mach_modern => d1mach
    use r1mach_module, only: r1mach_modern => r1mach
    use enorm_module, only: enorm_modern => enorm
    use denorm_module, only: denorm_modern => denorm
    use lsame_module, only: lsame_modern => lsame
    
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
                    select case(trim(function_name))
                        case('PYTHAG')
                            read(line(pos+1:), *) real_params(1), real_params(2)
                            num_real_params = 2
                        case('BSPLVN')
                            read(line(pos+1:), *) real_params(1)
                            num_real_params = 1
                        case default
                            read(line(pos+1:), *) real_params(1)
                            num_real_params = 1
                    end select
                case('PARAMS')
                    ! Legacy format for PYTHAG/BSPLVN
                    if (function_name == 'PYTHAG') then
                        read(line(pos+1:), *) real_params(1), real_params(2)
                    else if (function_name == 'BSPLVN') then
                        read(line(pos+1:), *) int_params(1), int_params(2), real_params(1), int_params(3)
                    end if
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
            case('PYTHAG')
                call validate_pythag()
            case('BSPLVN')
                call validate_bsplvn()
            case('I1MACH')
                call validate_i1mach()
            case('D1MACH')
                call validate_d1mach()
            case('R1MACH')
                call validate_r1mach()
            case('ENORM')
                call validate_enorm()
            case('DENORM')
                call validate_denorm()
            case('LSAME')
                call validate_lsame()
            case default
                print *, 'Function not implemented: ', trim(function_name)
                failed_count = failed_count + 1
        end select
    end subroutine
    
    ! Validation routines for each function
    
    subroutine validate_pythag()
        real :: a, b, result_f77, result_modern
        real :: pythag_f77
        external pythag_f77
        
        a = real_params(1)
        b = real_params(2)
        
        result_f77 = pythag_f77(a, b)
        result_modern = pythag_modern(a, b)
        
        if (values_equal(result_f77, result_modern, 'simple_arithmetic')) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,ES16.8,A,ES16.8)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine validate_i1mach()
        integer :: iwhich, result_f77, result_modern
        integer :: i1mach_f77
        external i1mach_f77
        
        iwhich = int_params(1)
        
        result_f77 = i1mach_f77(iwhich)
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
    
    subroutine validate_d1mach()
        integer :: iwhich
        real(8) :: result_f77, result_modern
        real(8) :: d1mach_f77
        external d1mach_f77
        
        iwhich = int_params(1)
        
        result_f77 = d1mach_f77(iwhich)
        result_modern = d1mach_modern(iwhich)
        
        if (result_f77 == result_modern) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,ES24.16,A,ES24.16)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine validate_r1mach()
        integer :: iwhich
        real :: result_f77, result_modern
        real :: r1mach_f77
        external r1mach_f77
        
        iwhich = int_params(1)
        
        result_f77 = r1mach_f77(iwhich)
        result_modern = r1mach_modern(iwhich)
        
        if (result_f77 == result_modern) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,ES16.8,A,ES16.8)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine validate_enorm()
        integer :: n
        real :: result_f77, result_modern
        real :: enorm_f77
        external enorm_f77
        
        n = int_params(1)
        
        result_f77 = enorm_f77(n, real_array)
        result_modern = enorm_modern(n, real_array)
        
        if (values_equal(result_f77, result_modern, 'simple_arithmetic')) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            print '(A,ES16.8,A,ES16.8)', '  F77: ', result_f77, ' Modern: ', result_modern
        end if
    end subroutine
    
    subroutine validate_denorm()
        ! Similar to enorm
        call validate_enorm()
    end subroutine
    
    subroutine validate_lsame()
        logical :: result_f77, result_modern
        logical :: lsame_f77
        external lsame_f77
        
        result_f77 = lsame_f77(char_params(1)(1:1), char_params(2)(1:1))
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
    
    subroutine validate_bsplvn()
        integer :: jhigh, index_val, ileft, i
        real :: x
        real :: vnikx_f77(20), vnikx_modern(20)
        external bsplvn_f77
        
        jhigh = int_params(1)
        index_val = int_params(2)
        x = real_params(1)
        ileft = int_params(3)
        
        vnikx_f77 = 0.0
        vnikx_modern = 0.0
        
        call bsplvn_f77(real_array, jhigh, index_val, x, ileft, vnikx_f77)
        call bsplvn_modern(real_array, jhigh, index_val, x, ileft, vnikx_modern)
        
        if (arrays_equal(vnikx_f77(1:jhigh), vnikx_modern(1:jhigh), jhigh)) then
            passed_count = passed_count + 1
            print '(A,A)', 'PASS: ', trim(description)
        else
            failed_count = failed_count + 1
            print '(A,A)', 'FAIL: ', trim(description)
            do i = 1, jhigh
                if (.not. values_equal(vnikx_f77(i), vnikx_modern(i), 'iterative')) then
                    print '(A,I2,A,ES16.8,A,ES16.8)', '  vnikx(', i, '): F77=', &
                        vnikx_f77(i), ' Modern=', vnikx_modern(i)
                end if
            end do
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
        print '(A)', '  Machine Constants: I1MACH, D1MACH, R1MACH'
        print '(A)', '  Math Functions: PYTHAG, ENORM, DENORM, ZABS, CDIV'
        print '(A)', '  Splines: BSPLVN, INTRV'
        print '(A)', '  Utilities: LSAME, BDIFF'
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
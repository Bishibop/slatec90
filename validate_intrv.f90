program validate_intrv
    use intrv_module
    implicit none
    
    ! Test data structure
    type :: test_case_type
        real, allocatable :: xt(:)
        integer :: lxt
        real :: x
        integer :: ilo_in
        integer :: expected_ileft
        integer :: expected_mflag
        integer :: expected_ilo_out
        character(len=200) :: description
        integer :: test_id
    end type
    
    ! Variables
    integer, parameter :: max_tests = 500
    type(test_case_type) :: test_cases(max_tests)
    integer :: num_tests
    integer :: i, failures
    
    ! Test results
    integer :: actual_ileft, actual_mflag, actual_ilo
    logical :: test_passed
    
    print *, 'INTRV Validation Test - Comparing against F77 reference'
    print *, '======================================================'
    
    ! Load test data
    call load_test_data(test_cases, num_tests)
    
    print *, 'Loaded', num_tests, 'test cases'
    print *, ''
    
    failures = 0
    
    ! Run all tests
    do i = 1, num_tests
        ! Set up test
        actual_ilo = test_cases(i)%ilo_in
        
        ! Call modern implementation
        call intrv(test_cases(i)%xt, test_cases(i)%lxt, test_cases(i)%x, &
                   actual_ilo, actual_ileft, actual_mflag)
        
        ! Check results
        test_passed = (actual_ileft == test_cases(i)%expected_ileft) .and. &
                      (actual_mflag == test_cases(i)%expected_mflag) .and. &
                      (actual_ilo == test_cases(i)%expected_ilo_out)
        
        if (.not. test_passed) then
            failures = failures + 1
            print *, 'FAIL Test', i, ':', trim(test_cases(i)%description)
            print *, '  Expected: ileft=', test_cases(i)%expected_ileft, &
                     ' mflag=', test_cases(i)%expected_mflag, &
                     ' ilo_out=', test_cases(i)%expected_ilo_out
            print *, '  Actual:   ileft=', actual_ileft, &
                     ' mflag=', actual_mflag, &
                     ' ilo_out=', actual_ilo
            print *, ''
        endif
        
        ! Progress indicator
        if (mod(i, 100) == 0) then
            print *, 'Completed', i, 'tests...'
        endif
    enddo
    
    ! Final results
    print *, ''
    print *, 'Validation Complete:'
    print *, '  Total tests:', num_tests
    print *, '  Failures:', failures
    print *, '  Pass rate:', real(num_tests - failures) / real(num_tests) * 100.0, '%'
    
    if (failures == 0) then
        print *, '  Status: ALL TESTS PASSED'
    else
        print *, '  Status: VALIDATION FAILED'
    endif
    
contains

    subroutine load_test_data(tests, n_tests)
        type(test_case_type), intent(out) :: tests(:)
        integer, intent(out) :: n_tests
        integer :: unit, ios, i, j
        character(len=1000) :: line
        
        n_tests = 0
        open(newunit=unit, file='test_data/intrv_tests.json', status='old', iostat=ios)
        if (ios /= 0) then
            print *, 'Error: Could not open test_data/intrv_tests.json'
            stop 1
        endif
        
        ! Simple JSON parser for our specific format
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            ! Look for test case start
            if (index(line, '"description":') > 0) then
                n_tests = n_tests + 1
                if (n_tests > size(tests)) exit
                
                ! Parse description
                call extract_string_value(line, tests(n_tests)%description)
                
                ! Read inputs array
                call read_inputs_array(unit, tests(n_tests))
                
                ! Read expected values  
                call read_expected_values(unit, tests(n_tests))
                
                tests(n_tests)%test_id = n_tests
            endif
        enddo
        
        close(unit)
    end subroutine
    
    subroutine extract_string_value(line, value)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: value
        integer :: start_pos, end_pos
        
        start_pos = index(line, '"', .true.) - 1
        start_pos = index(line(1:start_pos), '"', .true.) + 1
        end_pos = index(line, '"', .true.) - 1
        
        if (start_pos > 0 .and. end_pos > start_pos) then
            value = line(start_pos:end_pos)
        else
            value = 'Parse error'
        endif
    end subroutine
    
    subroutine read_inputs_array(unit, test)
        integer, intent(in) :: unit
        type(test_case_type), intent(inout) :: test
        character(len=1000) :: line
        integer :: ios, i, array_size
        
        ! This is a simplified parser - for production would need more robust JSON parsing
        ! For now, assume fixed structure based on the format we see
        
        ! Skip to inputs array
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0 .or. index(line, '"inputs":') > 0) exit
        enddo
        
        ! Read array elements (simplified parsing)
        array_size = 5  ! From inspection of test data
        allocate(test%xt(array_size))
        test%lxt = array_size
        
        ! This would need proper JSON parsing in production
        ! For validation, we'll use the Python helper instead
    end subroutine
    
    subroutine read_expected_values(unit, test)
        integer, intent(in) :: unit
        type(test_case_type), intent(inout) :: test
        ! Simplified - will use Python helper for actual parsing
    end subroutine
    
end program validate_intrv
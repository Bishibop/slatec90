program validate_intrv_complete
    use intrv_module
    use, intrinsic :: iso_fortran_env, only: real32
    implicit none
    
    ! Maximum array size and number of tests
    integer, parameter :: max_array_size = 100
    integer, parameter :: max_tests = 500
    
    ! Test data storage
    real :: xt_array(max_array_size)
    integer :: lxt, ilo_in, ilo_out
    real :: x_value
    integer :: ileft, mflag
    
    ! Expected values
    integer :: expected_ileft, expected_mflag, expected_ilo
    
    ! Counters and statistics
    integer :: test_id, i, array_size
    integer :: total_tests = 0
    integer :: passed_tests = 0
    integer :: failed_tests = 0
    
    ! Failure tracking
    integer :: extreme_value_failures = 0
    integer :: boundary_failures = 0
    integer :: binary_search_failures = 0
    integer :: mflag_failures = 0
    
    ! File handling
    integer :: unit_in = 10
    integer :: ios
    character(len=1000) :: line
    character(len=100) :: dummy
    
    ! Open results file
    open(unit=20, file='intrv_validation_results.txt', status='replace')
    write(20,'(A)') 'INTRV Modern Implementation Validation Report'
    write(20,'(A)') '============================================='
    write(20,'(A)') ''
    
    ! Read test data
    open(unit=unit_in, file='test_data/intrv_tests.json', status='old', iostat=ios)
    if (ios /= 0) then
        write(*,*) 'Error: Cannot open test_data/intrv_tests.json'
        stop 1
    end if
    
    ! Skip header lines until we find test_cases
    do
        read(unit_in, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, '"test_cases"') > 0) exit
    end do
    
    ! Process each test case
    do test_id = 1, max_tests
        ! Skip to next test
        do
            read(unit_in, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, '"test_id":') > 0) then
                ! Found a test, now parse it
                call parse_test_case()
                exit
            end if
        end do
        
        if (ios /= 0) exit
        
        ! Run the test
        ilo_out = ilo_in
        call intrv(xt_array, lxt, x_value, ilo_out, ileft, mflag)
        
        total_tests = total_tests + 1
        
        ! Check results
        if (ileft == expected_ileft .and. mflag == expected_mflag .and. &
            ilo_out == expected_ilo) then
            passed_tests = passed_tests + 1
        else
            failed_tests = failed_tests + 1
            
            ! Analyze failure type
            call analyze_failure()
            
            ! Report failure details for extreme value tests
            if (is_extreme_value_test()) then
                write(20,'(A,I3,A)') 'Test ', test_id, ' (Extreme Value Test) FAILED:'
                write(20,'(A,ES12.5)') '  X value: ', x_value
                write(20,'(A)') '  Array values:'
                do i = 1, lxt
                    write(20,'(A,I2,A,ES12.5)') '    XT(', i, ') = ', xt_array(i)
                end do
                write(20,'(A,I3,A,I3,A,I3)') '  Expected: ILEFT=', expected_ileft, &
                    ', MFLAG=', expected_mflag, ', ILO=', expected_ilo
                write(20,'(A,I3,A,I3,A,I3)') '  Got:      ILEFT=', ileft, &
                    ', MFLAG=', mflag, ', ILO=', ilo_out
                write(20,'(A)') ''
            end if
        end if
    end do
    
    close(unit_in)
    
    ! Write summary
    write(20,'(A)') ''
    write(20,'(A)') 'Summary Statistics:'
    write(20,'(A)') '==================='
    write(20,'(A,I4)') 'Total tests:     ', total_tests
    write(20,'(A,I4)') 'Passed tests:    ', passed_tests
    write(20,'(A,I4)') 'Failed tests:    ', failed_tests
    write(20,'(A,F6.2,A)') 'Pass rate:       ', &
        real(passed_tests) / real(total_tests) * 100.0, '%'
    write(20,'(A)') ''
    write(20,'(A)') 'Failure Analysis:'
    write(20,'(A,I4)') 'Extreme value failures:    ', extreme_value_failures
    write(20,'(A,I4)') 'Boundary condition failures:', boundary_failures
    write(20,'(A,I4)') 'Binary search failures:    ', binary_search_failures
    write(20,'(A,I4)') 'MFLAG failures:            ', mflag_failures
    
    close(20)
    
    ! Console output
    write(*,'(A)') 'INTRV validation complete.'
    write(*,'(A,I4,A,I4,A,F6.2,A)') 'Results: ', passed_tests, '/', total_tests, &
        ' passed (', real(passed_tests) / real(total_tests) * 100.0, '%)'
    write(*,'(A)') 'Detailed results written to intrv_validation_results.txt'
    
contains

    subroutine parse_test_case()
        ! Parse array values
        read(unit_in, '(A)') line  ! skip "inputs" line
        read(unit_in, '(A)') line  ! skip "[" line
        
        ! Read array values
        array_size = 0
        do
            read(unit_in, '(A)') line
            if (index(line, ']') > 0) exit
            array_size = array_size + 1
            read(line, *) xt_array(array_size)
        end do
        
        ! Read LXT
        read(unit_in, '(A)') line
        read(line, *) lxt
        
        ! Read X value
        read(unit_in, '(A)') line
        read(line, *) x_value
        
        ! Read initial ILO
        read(unit_in, '(A)') line
        read(line, *) ilo_in
        
        ! Skip to expected values
        do
            read(unit_in, '(A)') line
            if (index(line, '"expected"') > 0) exit
        end do
        
        ! Read expected ILEFT
        read(unit_in, '(A)') line
        read(line(index(line,':')+1:), *) expected_ileft
        
        ! Read expected MFLAG
        read(unit_in, '(A)') line
        read(line(index(line,':')+1:), *) expected_mflag
        
        ! Read expected ILO_out
        read(unit_in, '(A)') line
        read(line(index(line,':')+1:), *) expected_ilo
    end subroutine
    
    logical function is_extreme_value_test()
        integer :: i
        logical :: has_extreme
        
        has_extreme = .false.
        
        ! Check for very small values
        do i = 1, lxt
            if (abs(xt_array(i)) < 1.0e-20 .and. xt_array(i) /= 0.0) then
                has_extreme = .true.
                exit
            end if
        end do
        
        ! Check for very large values
        do i = 1, lxt
            if (abs(xt_array(i)) > 1.0e20) then
                has_extreme = .true.
                exit
            end if
        end do
        
        ! Check if x is extreme
        if (abs(x_value) < 1.0e-20 .and. x_value /= 0.0) has_extreme = .true.
        if (abs(x_value) > 1.0e20) has_extreme = .true.
        
        is_extreme_value_test = has_extreme
    end function
    
    subroutine analyze_failure()
        ! Categorize the failure
        
        ! Check if it's an extreme value issue
        if (is_extreme_value_test()) then
            extreme_value_failures = extreme_value_failures + 1
        end if
        
        ! Check if it's a boundary condition
        if (x_value < xt_array(1) .or. x_value >= xt_array(lxt)) then
            boundary_failures = boundary_failures + 1
        end if
        
        ! Check if MFLAG is wrong
        if (mflag /= expected_mflag) then
            mflag_failures = mflag_failures + 1
        end if
        
        ! Otherwise it's likely a binary search issue
        if (.not. is_extreme_value_test() .and. &
            x_value >= xt_array(1) .and. x_value < xt_array(lxt)) then
            binary_search_failures = binary_search_failures + 1
        end if
    end subroutine

end program validate_intrv_complete
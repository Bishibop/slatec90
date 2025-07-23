program validate_denorm_enhanced
    use denorm_module
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use, intrinsic :: ieee_arithmetic
    implicit none
    
    ! Constants
    integer, parameter :: MAX_TESTS = 300
    integer, parameter :: MAX_VECTOR_SIZE = 10000
    real(real64), parameter :: TOLERANCE = 1.0e-10
    
    ! Test data arrays
    integer :: test_ids(MAX_TESTS)
    character(len=200) :: test_descriptions(MAX_TESTS)
    integer :: test_n_values(MAX_TESTS)
    real(real64) :: test_vectors(MAX_VECTOR_SIZE, MAX_TESTS)
    real(real64) :: reference_values(MAX_TESTS)
    
    ! Results tracking
    integer :: total_tests, passed_tests, failed_tests
    real(real64) :: computed_values(MAX_TESTS)
    real(real64) :: relative_errors(MAX_TESTS)
    logical :: test_passed(MAX_TESTS)
    
    ! Category tracking
    integer :: edge_case_count, edge_case_passed
    integer :: precision_count, precision_passed
    integer :: stress_count, stress_passed
    integer :: boundary_count, boundary_passed
    integer :: subnormal_count, subnormal_passed
    integer :: overflow_count, overflow_passed
    integer :: underflow_count, underflow_passed
    integer :: mixed_count, mixed_passed
    integer :: special_count, special_passed
    
    ! File handling
    integer :: input_unit, output_unit, report_unit
    integer :: ios, i, j
    character(len=1000) :: line
    character(len=50) :: category
    
    ! Open files
    open(newunit=input_unit, file='test_data/denorm_tests_enhanced.json', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening enhanced test data file"
        stop 1
    end if
    
    open(newunit=output_unit, file='test_data/denorm_output.json', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening reference output file"
        stop 1
    end if
    
    open(newunit=report_unit, file='validation/denorm_enhanced_validation_report.txt', &
         status='replace', action='write')
    
    ! Initialize counters
    total_tests = 0
    passed_tests = 0
    failed_tests = 0
    
    edge_case_count = 0; edge_case_passed = 0
    precision_count = 0; precision_passed = 0
    stress_count = 0; stress_passed = 0
    boundary_count = 0; boundary_passed = 0
    subnormal_count = 0; subnormal_passed = 0
    overflow_count = 0; overflow_passed = 0
    underflow_count = 0; underflow_passed = 0
    mixed_count = 0; mixed_passed = 0
    special_count = 0; special_passed = 0
    
    ! Read test data - simplified JSON parsing
    call read_test_data()
    
    ! Read reference values
    call read_reference_values()
    
    ! Write report header
    write(report_unit, '(A)') "=================================================="
    write(report_unit, '(A)') "DENORM ENHANCED VALIDATION REPORT"
    write(report_unit, '(A)') "=================================================="
    write(report_unit, '(A)') ""
    write(report_unit, '(A, I0)') "Total number of tests: ", total_tests
    write(report_unit, '(A, E25.17)') "Tolerance: ", TOLERANCE
    write(report_unit, '(A)') ""
    
    ! Run all tests
    do i = 1, total_tests
        computed_values(i) = denorm(test_n_values(i), test_vectors(1:test_n_values(i), i))
        
        ! Check for NaN or Inf
        if (ieee_is_nan(computed_values(i)) .or. .not. ieee_is_finite(computed_values(i))) then
            test_passed(i) = .false.
            relative_errors(i) = huge(1.0_real64)
        else
            ! Calculate relative error
            if (reference_values(i) == 0.0_real64) then
                if (computed_values(i) == 0.0_real64) then
                    relative_errors(i) = 0.0_real64
                    test_passed(i) = .true.
                else
                    relative_errors(i) = abs(computed_values(i))
                    test_passed(i) = relative_errors(i) < TOLERANCE
                end if
            else
                relative_errors(i) = abs((computed_values(i) - reference_values(i)) / reference_values(i))
                test_passed(i) = relative_errors(i) < TOLERANCE
            end if
        end if
        
        if (test_passed(i)) then
            passed_tests = passed_tests + 1
        else
            failed_tests = failed_tests + 1
        end if
        
        ! Categorize test
        call categorize_and_count(i)
    end do
    
    ! Write summary results
    write(report_unit, '(A)') "OVERALL RESULTS:"
    write(report_unit, '(A)') "----------------"
    write(report_unit, '(A, I0, A, F6.2, A)') "Passed: ", passed_tests, &
        " (", 100.0 * real(passed_tests) / real(total_tests), "%)"
    write(report_unit, '(A, I0, A, F6.2, A)') "Failed: ", failed_tests, &
        " (", 100.0 * real(failed_tests) / real(total_tests), "%)"
    write(report_unit, '(A)') ""
    
    ! Write category results
    write(report_unit, '(A)') "RESULTS BY CATEGORY:"
    write(report_unit, '(A)') "--------------------"
    call write_category_result("Edge Cases", edge_case_count, edge_case_passed)
    call write_category_result("Precision Tests", precision_count, precision_passed)
    call write_category_result("Stress Tests", stress_count, stress_passed)
    call write_category_result("Boundary Cases", boundary_count, boundary_passed)
    call write_category_result("Subnormal Values", subnormal_count, subnormal_passed)
    call write_category_result("Overflow Protection", overflow_count, overflow_passed)
    call write_category_result("Underflow Protection", underflow_count, underflow_passed)
    call write_category_result("Mixed Magnitude", mixed_count, mixed_passed)
    call write_category_result("Special Patterns", special_count, special_passed)
    write(report_unit, '(A)') ""
    
    ! Write detailed failure analysis
    if (failed_tests > 0) then
        write(report_unit, '(A)') "DETAILED FAILURE ANALYSIS:"
        write(report_unit, '(A)') "--------------------------"
        
        do i = 1, total_tests
            if (.not. test_passed(i)) then
                write(report_unit, '(A)') ""
                write(report_unit, '(A, I0)') "Test ID: ", test_ids(i)
                write(report_unit, '(A, A)') "Description: ", trim(test_descriptions(i))
                write(report_unit, '(A, I0)') "Vector size: ", test_n_values(i)
                write(report_unit, '(A, E25.17)') "Expected: ", reference_values(i)
                write(report_unit, '(A, E25.17)') "Computed: ", computed_values(i)
                write(report_unit, '(A, E25.17)') "Relative error: ", relative_errors(i)
                
                ! Check for special conditions
                if (ieee_is_nan(computed_values(i))) then
                    write(report_unit, '(A)') "ERROR: Result is NaN"
                else if (.not. ieee_is_finite(computed_values(i))) then
                    write(report_unit, '(A)') "ERROR: Result is Infinity"
                end if
                
                ! Show first few vector elements
                write(report_unit, '(A)', advance='no') "Vector elements: ["
                do j = 1, min(5, test_n_values(i))
                    write(report_unit, '(E12.5)', advance='no') test_vectors(j, i)
                    if (j < min(5, test_n_values(i))) write(report_unit, '(A)', advance='no') ", "
                end do
                if (test_n_values(i) > 5) write(report_unit, '(A)', advance='no') ", ..."
                write(report_unit, '(A)') "]"
            end if
        end do
    end if
    
    ! Statistical analysis
    write(report_unit, '(A)') ""
    write(report_unit, '(A)') "STATISTICAL ANALYSIS:"
    write(report_unit, '(A)') "---------------------"
    call compute_statistics()
    
    ! Performance notes
    write(report_unit, '(A)') ""
    write(report_unit, '(A)') "SPECIAL CASE ANALYSIS:"
    write(report_unit, '(A)') "----------------------"
    call analyze_special_cases()
    
    ! Close files
    close(input_unit)
    close(output_unit)
    close(report_unit)
    
    ! Print summary to console
    print *, ""
    print *, "DENORM Enhanced Validation Complete"
    print *, "==================================="
    print '(A, I0, A, I0)', "Tests passed: ", passed_tests, " / ", total_tests
    print '(A, F6.2, A)', "Success rate: ", 100.0 * real(passed_tests) / real(total_tests), "%"
    print *, ""
    print *, "Detailed report written to: validation/denorm_enhanced_validation_report.txt"
    
contains

    subroutine read_test_data()
        integer :: test_count, vec_idx
        character(len=1000) :: desc
        real(real64) :: val
        
        test_count = 0
        
        ! Skip header lines
        do i = 1, 6
            read(input_unit, '(A)')
        end do
        
        ! Read each test case
        do
            read(input_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            if (index(line, '"test_id"') > 0) then
                test_count = test_count + 1
                read(line(index(line, ':')+1:), *) test_ids(test_count)
            else if (index(line, '"description"') > 0) then
                desc = line(index(line, '": "')+4:)
                desc = desc(1:index(desc, '"')-1)
                test_descriptions(test_count) = desc
            else if (index(line, '"n"') > 0 .and. index(line, '"inputs"') == 0) then
                read(line(index(line, ':')+1:), *) test_n_values(test_count)
                vec_idx = 0
            else if (index(line, '],') == 0 .and. index(line, ',') > 0 .and. vec_idx >= 0) then
                ! Reading vector elements
                vec_idx = vec_idx + 1
                read(line(1:index(line, ',')-1), *) val
                test_vectors(vec_idx, test_count) = val
            end if
        end do
        
        total_tests = test_count
    end subroutine read_test_data
    
    subroutine read_reference_values()
        integer :: ref_count
        
        ! Skip header lines
        do i = 1, 3
            read(output_unit, '(A)')
        end do
        
        ref_count = 0
        do
            read(output_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, ',') > 0 .or. index(line, ']') == 0) then
                ref_count = ref_count + 1
                line = adjustl(line)
                if (line(len_trim(line):len_trim(line)) == ',') then
                    line = line(1:len_trim(line)-1)
                end if
                read(line, *) reference_values(ref_count)
            end if
        end do
    end subroutine read_reference_values
    
    subroutine categorize_and_count(idx)
        integer, intent(in) :: idx
        character(len=200) :: desc
        
        desc = test_descriptions(idx)
        
        if (index(desc, 'zero') > 0 .or. index(desc, 'empty') > 0 .or. &
            index(desc, 'single') > 0) then
            edge_case_count = edge_case_count + 1
            if (test_passed(idx)) edge_case_passed = edge_case_passed + 1
        else if (index(desc, 'precision') > 0 .or. index(desc, 'ULP') > 0 .or. &
                 index(desc, 'bit pattern') > 0) then
            precision_count = precision_count + 1
            if (test_passed(idx)) precision_passed = precision_passed + 1
        else if (index(desc, 'large vector') > 0 .or. test_n_values(idx) > 1000) then
            stress_count = stress_count + 1
            if (test_passed(idx)) stress_passed = stress_passed + 1
        else if (index(desc, 'IEEE') > 0 .or. index(desc, 'boundary') > 0) then
            boundary_count = boundary_count + 1
            if (test_passed(idx)) boundary_passed = boundary_passed + 1
        else if (index(desc, 'subnormal') > 0 .or. index(desc, 'denormal') > 0) then
            subnormal_count = subnormal_count + 1
            if (test_passed(idx)) subnormal_passed = subnormal_passed + 1
        else if (index(desc, 'overflow') > 0 .or. index(desc, 'giant') > 0) then
            overflow_count = overflow_count + 1
            if (test_passed(idx)) overflow_passed = overflow_passed + 1
        else if (index(desc, 'underflow') > 0 .or. index(desc, 'tiny') > 0 .or. &
                 index(desc, 'dwarf') > 0) then
            underflow_count = underflow_count + 1
            if (test_passed(idx)) underflow_passed = underflow_passed + 1
        else if (index(desc, 'mixed') > 0 .or. index(desc, 'cascade') > 0) then
            mixed_count = mixed_count + 1
            if (test_passed(idx)) mixed_passed = mixed_passed + 1
        else
            special_count = special_count + 1
            if (test_passed(idx)) special_passed = special_passed + 1
        end if
    end subroutine categorize_and_count
    
    subroutine write_category_result(category_name, count, passed)
        character(len=*), intent(in) :: category_name
        integer, intent(in) :: count, passed
        
        if (count > 0) then
            write(report_unit, '(A, A20, I4, A, I4, A, F6.2, A)') &
                "  ", category_name, passed, " / ", count, &
                " (", 100.0 * real(passed) / real(count), "%)"
        end if
    end subroutine write_category_result
    
    subroutine compute_statistics()
        real(real64) :: max_error, avg_error, std_dev
        integer :: n_errors
        
        max_error = 0.0_real64
        avg_error = 0.0_real64
        n_errors = 0
        
        do i = 1, total_tests
            if (.not. ieee_is_nan(relative_errors(i)) .and. &
                ieee_is_finite(relative_errors(i))) then
                if (relative_errors(i) > max_error) max_error = relative_errors(i)
                avg_error = avg_error + relative_errors(i)
                n_errors = n_errors + 1
            end if
        end do
        
        if (n_errors > 0) then
            avg_error = avg_error / real(n_errors, real64)
            
            ! Compute standard deviation
            std_dev = 0.0_real64
            do i = 1, total_tests
                if (.not. ieee_is_nan(relative_errors(i)) .and. &
                    ieee_is_finite(relative_errors(i))) then
                    std_dev = std_dev + (relative_errors(i) - avg_error)**2
                end if
            end do
            std_dev = sqrt(std_dev / real(n_errors, real64))
            
            write(report_unit, '(A, E25.17)') "Maximum relative error: ", max_error
            write(report_unit, '(A, E25.17)') "Average relative error: ", avg_error
            write(report_unit, '(A, E25.17)') "Standard deviation: ", std_dev
        end if
    end subroutine compute_statistics
    
    subroutine analyze_special_cases()
        integer :: subnormal_tests, overflow_tests, large_vec_tests
        integer :: subnormal_ok, overflow_ok, large_vec_ok
        
        subnormal_tests = 0; subnormal_ok = 0
        overflow_tests = 0; overflow_ok = 0
        large_vec_tests = 0; large_vec_ok = 0
        
        do i = 1, total_tests
            ! Check for subnormal handling
            if (any(abs(test_vectors(1:test_n_values(i), i)) < tiny(1.0_real64))) then
                subnormal_tests = subnormal_tests + 1
                if (test_passed(i)) subnormal_ok = subnormal_ok + 1
            end if
            
            ! Check for overflow protection
            if (any(abs(test_vectors(1:test_n_values(i), i)) > 1.0e18_real64)) then
                overflow_tests = overflow_tests + 1
                if (test_passed(i)) overflow_ok = overflow_ok + 1
            end if
            
            ! Check large vector performance
            if (test_n_values(i) > 1000) then
                large_vec_tests = large_vec_tests + 1
                if (test_passed(i)) large_vec_ok = large_vec_ok + 1
            end if
        end do
        
        write(report_unit, '(A, I0, A, I0)') &
            "Subnormal value handling: ", subnormal_ok, " / ", subnormal_tests
        write(report_unit, '(A, I0, A, I0)') &
            "Overflow protection: ", overflow_ok, " / ", overflow_tests
        write(report_unit, '(A, I0, A, I0)') &
            "Large vector accuracy: ", large_vec_ok, " / ", large_vec_tests
    end subroutine analyze_special_cases

end program validate_denorm_enhanced
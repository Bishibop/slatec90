program validate_denorm_full
    use denorm_module
    use, intrinsic :: iso_fortran_env, only: real64, int32, int64
    use, intrinsic :: ieee_arithmetic
    implicit none
    
    ! Constants
    real(real64), parameter :: TOLERANCE = 1.0e-10
    integer, parameter :: TOTAL_TESTS = 257
    integer, parameter :: MAX_N = 10000
    
    ! Test data
    real(real64) :: test_vectors(MAX_N)
    real(real64) :: ref_values(TOTAL_TESTS)
    integer :: test_n(TOTAL_TESTS)
    character(len=200) :: test_desc(TOTAL_TESTS)
    
    ! Results
    real(real64) :: computed_values(TOTAL_TESTS)
    real(real64) :: rel_errors(TOTAL_TESTS)
    logical :: test_passed(TOTAL_TESTS)
    
    ! Counters
    integer :: passed, failed, i, j
    integer :: edge_count, edge_passed
    integer :: precision_count, precision_passed
    integer :: stress_count, stress_passed
    integer :: boundary_count, boundary_passed
    integer :: subnormal_count, subnormal_passed
    integer :: overflow_count, overflow_passed
    integer :: underflow_count, underflow_passed
    integer :: mixed_count, mixed_passed
    integer :: special_count, special_passed
    
    ! Timing
    real(real64) :: start_time, end_time, total_time
    integer(int64) :: count_rate, count_max
    
    ! File I/O
    integer :: json_unit, ref_unit, report_unit, ios
    character(len=1000) :: line
    real(real64) :: val
    integer :: current_test, current_n, vec_idx
    logical :: in_inputs
    
    ! Initialize
    passed = 0
    failed = 0
    edge_count = 0; edge_passed = 0
    precision_count = 0; precision_passed = 0
    stress_count = 0; stress_passed = 0
    boundary_count = 0; boundary_passed = 0
    subnormal_count = 0; subnormal_passed = 0
    overflow_count = 0; overflow_passed = 0
    underflow_count = 0; underflow_passed = 0
    mixed_count = 0; mixed_passed = 0
    special_count = 0; special_passed = 0
    
    ! Open files
    open(newunit=json_unit, file='test_data/denorm_tests_enhanced.json', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) stop "Cannot open test data file"
    
    open(newunit=ref_unit, file='validation/denorm_reference_complete.json', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) stop "Cannot open reference file"
    
    open(newunit=report_unit, file='validation/denorm_full_validation_report.txt', &
         status='replace', action='write')
    
    ! Write header
    write(report_unit, '(A)') "=================================================="
    write(report_unit, '(A)') "DENORM ENHANCED VALIDATION REPORT (Full)"
    write(report_unit, '(A)') "=================================================="
    write(report_unit, '(A)') ""
    write(report_unit, '(A, I0)') "Total tests: ", TOTAL_TESTS
    write(report_unit, '(A, E25.17)') "Tolerance: ", TOLERANCE
    write(report_unit, '(A)') ""
    
    ! Read reference values
    call read_reference_values()
    
    ! Read and process test data
    call read_and_run_tests()
    
    ! Write summary
    write(report_unit, '(A)') ""
    write(report_unit, '(A)') "OVERALL RESULTS:"
    write(report_unit, '(A)') "----------------"
    write(report_unit, '(A, I0, A, F6.2, A)') "Passed: ", passed, &
        " (", 100.0 * real(passed) / real(TOTAL_TESTS), "%)"
    write(report_unit, '(A, I0, A, F6.2, A)') "Failed: ", failed, &
        " (", 100.0 * real(failed) / real(TOTAL_TESTS), "%)"
    write(report_unit, '(A, F12.6, A)') "Total time: ", total_time, " seconds"
    write(report_unit, '(A, F12.3, A)') "Average time per test: ", &
        1000.0 * total_time / real(TOTAL_TESTS), " ms"
    
    ! Category results
    write(report_unit, '(A)') ""
    write(report_unit, '(A)') "RESULTS BY CATEGORY:"
    write(report_unit, '(A)') "--------------------"
    call write_category("Edge Cases", edge_count, edge_passed)
    call write_category("Precision", precision_count, precision_passed)
    call write_category("Stress Tests", stress_count, stress_passed)
    call write_category("IEEE Boundary", boundary_count, boundary_passed)
    call write_category("Subnormal", subnormal_count, subnormal_passed)
    call write_category("Overflow", overflow_count, overflow_passed)
    call write_category("Underflow", underflow_count, underflow_passed)
    call write_category("Mixed Magnitude", mixed_count, mixed_passed)
    call write_category("Special Patterns", special_count, special_passed)
    
    ! Failure details
    if (failed > 0) then
        write(report_unit, '(A)') ""
        write(report_unit, '(A)') "FAILED TESTS:"
        write(report_unit, '(A)') "-------------"
        do i = 1, TOTAL_TESTS
            if (.not. test_passed(i)) then
                write(report_unit, '(A)') ""
                write(report_unit, '(A, I0, A, A)') "Test ", i, ": ", trim(test_desc(i))
                write(report_unit, '(A, I0)') "  Vector size: ", test_n(i)
                write(report_unit, '(A, E25.17)') "  Expected: ", ref_values(i)
                write(report_unit, '(A, E25.17)') "  Computed: ", computed_values(i)
                write(report_unit, '(A, E25.17)') "  Rel Error: ", rel_errors(i)
            end if
        end do
    end if
    
    ! Statistical analysis
    call write_statistics()
    
    ! Special case analysis
    call analyze_special_cases()
    
    ! Close files
    close(json_unit)
    close(ref_unit)
    close(report_unit)
    
    ! Console output
    print *, ""
    print *, "DENORM Enhanced Validation Complete"
    print *, "==================================="
    print '(A, I0, A, I0)', "Tests passed: ", passed, " / ", TOTAL_TESTS
    print '(A, F6.2, A)', "Success rate: ", 100.0 * real(passed) / real(TOTAL_TESTS), "%"
    print '(A, F12.6, A)', "Total time: ", total_time, " seconds"
    print *, ""
    print *, "Report: validation/denorm_full_validation_report.txt"
    
contains

    subroutine read_reference_values()
        integer :: count
        
        ! Skip header
        do i = 1, 3
            read(ref_unit, '(A)')
        end do
        
        count = 0
        do
            read(ref_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line = adjustl(line)
            if (len_trim(line) > 0 .and. line(1:1) /= ']' .and. line(1:1) /= '}') then
                count = count + 1
                if (count > TOTAL_TESTS) exit
                ! Remove trailing comma if present
                if (line(len_trim(line):len_trim(line)) == ',') then
                    line = line(1:len_trim(line)-1)
                end if
                read(line, *) ref_values(count)
            end if
        end do
    end subroutine read_reference_values
    
    subroutine read_and_run_tests()
        integer :: test_id
        real(real64) :: t1, t2
        
        current_test = 0
        in_inputs = .false.
        total_time = 0.0_real64
        
        do
            read(json_unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            
            ! Parse test ID
            if (index(line, '"test_id"') > 0) then
                current_test = current_test + 1
                if (current_test > TOTAL_TESTS) exit
                
                ! Extract test ID
                i = index(line, ':')
                read(line(i+1:), *) test_id
                
                ! Reset for new test
                vec_idx = 0
                in_inputs = .false.
                
            ! Parse description
            else if (index(line, '"description"') > 0 .and. current_test > 0) then
                i = index(line, '": "') + 4
                j = index(line(i:), '"') - 1
                test_desc(current_test) = line(i:i+j-1)
                
            ! Parse n
            else if (index(line, '"n"') > 0 .and. index(line, '"inputs"') == 0 .and. current_test > 0) then
                i = index(line, ':')
                read(line(i+1:), *) test_n(current_test)
                
            ! Parse inputs array start
            else if (index(line, '"inputs"') > 0 .and. current_test > 0) then
                in_inputs = .true.
                vec_idx = 0
                
            ! Parse input values
            else if (in_inputs .and. current_test > 0) then
                if (index(line, ']') > 0) then
                    in_inputs = .false.
                    
                    ! Run the test
                    call cpu_time(t1)
                    computed_values(current_test) = denorm(test_n(current_test), test_vectors(1:test_n(current_test)))
                    call cpu_time(t2)
                    total_time = total_time + (t2 - t1)
                    
                    ! Check result
                    call check_result(current_test)
                    call categorize_test(current_test)
                    
                else
                    ! Read a vector element
                    line = adjustl(line)
                    if (len_trim(line) > 0 .and. line(1:1) /= '[') then
                        vec_idx = vec_idx + 1
                        if (vec_idx <= MAX_N) then
                            ! Remove comma if present
                            if (line(len_trim(line):len_trim(line)) == ',') then
                                line = line(1:len_trim(line)-1)
                            end if
                            read(line, *) test_vectors(vec_idx)
                        end if
                    end if
                end if
            end if
        end do
    end subroutine read_and_run_tests
    
    subroutine check_result(idx)
        integer, intent(in) :: idx
        real(real64) :: computed, expected
        
        computed = computed_values(idx)
        expected = ref_values(idx)
        
        ! Check for special values
        if (ieee_is_nan(computed)) then
            test_passed(idx) = .false.
            rel_errors(idx) = huge(1.0_real64)
            failed = failed + 1
        else if (.not. ieee_is_finite(computed)) then
            test_passed(idx) = .false.
            rel_errors(idx) = huge(1.0_real64)
            failed = failed + 1
        else
            ! Calculate relative error
            if (expected == 0.0_real64) then
                if (computed == 0.0_real64) then
                    rel_errors(idx) = 0.0_real64
                    test_passed(idx) = .true.
                else
                    rel_errors(idx) = abs(computed)
                    test_passed(idx) = rel_errors(idx) < TOLERANCE
                end if
            else
                rel_errors(idx) = abs((computed - expected) / expected)
                test_passed(idx) = rel_errors(idx) < TOLERANCE
            end if
            
            if (test_passed(idx)) then
                passed = passed + 1
            else
                failed = failed + 1
            end if
        end if
    end subroutine check_result
    
    subroutine categorize_test(idx)
        integer, intent(in) :: idx
        character(len=200) :: desc
        
        desc = test_desc(idx)
        
        if (index(desc, 'zero') > 0 .or. index(desc, 'empty') > 0 .or. &
            index(desc, 'single') > 0) then
            edge_count = edge_count + 1
            if (test_passed(idx)) edge_passed = edge_passed + 1
        else if (index(desc, 'precision') > 0 .or. index(desc, 'ULP') > 0 .or. &
                 index(desc, 'bit pattern') > 0) then
            precision_count = precision_count + 1
            if (test_passed(idx)) precision_passed = precision_passed + 1
        else if (index(desc, 'Large vector') > 0 .or. test_n(idx) > 1000) then
            stress_count = stress_count + 1
            if (test_passed(idx)) stress_passed = stress_passed + 1
        else if (index(desc, 'IEEE') > 0 .or. index(desc, 'boundary') > 0) then
            boundary_count = boundary_count + 1
            if (test_passed(idx)) boundary_passed = boundary_passed + 1
        else if (index(desc, 'subnormal') > 0 .or. index(desc, 'denormal') > 0) then
            subnormal_count = subnormal_count + 1
            if (test_passed(idx)) subnormal_passed = subnormal_passed + 1
        else if (index(desc, 'overflow') > 0 .or. index(desc, 'Values near RGIANT') > 0) then
            overflow_count = overflow_count + 1
            if (test_passed(idx)) overflow_passed = overflow_passed + 1
        else if (index(desc, 'underflow') > 0 .or. index(desc, 'tiny') > 0 .or. &
                 index(desc, 'Values near RDWARF') > 0) then
            underflow_count = underflow_count + 1
            if (test_passed(idx)) underflow_passed = underflow_passed + 1
        else if (index(desc, 'mixed') > 0 .or. index(desc, 'cascade') > 0) then
            mixed_count = mixed_count + 1
            if (test_passed(idx)) mixed_passed = mixed_passed + 1
        else
            special_count = special_count + 1
            if (test_passed(idx)) special_passed = special_passed + 1
        end if
    end subroutine categorize_test
    
    subroutine write_category(name, count, pass)
        character(len=*), intent(in) :: name
        integer, intent(in) :: count, pass
        
        if (count > 0) then
            write(report_unit, '(A, A20, I4, A, I4, A, F6.2, A)') &
                "  ", name, pass, " / ", count, &
                " (", 100.0 * real(pass) / real(count), "%)"
        end if
    end subroutine write_category
    
    subroutine write_statistics()
        real(real64) :: max_err, avg_err, std_dev
        integer :: n_valid
        
        write(report_unit, '(A)') ""
        write(report_unit, '(A)') "STATISTICAL ANALYSIS:"
        write(report_unit, '(A)') "--------------------"
        
        max_err = 0.0_real64
        avg_err = 0.0_real64
        n_valid = 0
        
        do i = 1, TOTAL_TESTS
            if (ieee_is_finite(rel_errors(i))) then
                n_valid = n_valid + 1
                avg_err = avg_err + rel_errors(i)
                if (rel_errors(i) > max_err) max_err = rel_errors(i)
            end if
        end do
        
        if (n_valid > 0) then
            avg_err = avg_err / real(n_valid, real64)
            
            std_dev = 0.0_real64
            do i = 1, TOTAL_TESTS
                if (ieee_is_finite(rel_errors(i))) then
                    std_dev = std_dev + (rel_errors(i) - avg_err)**2
                end if
            end do
            std_dev = sqrt(std_dev / real(n_valid, real64))
            
            write(report_unit, '(A, E25.17)') "Maximum relative error: ", max_err
            write(report_unit, '(A, E25.17)') "Average relative error: ", avg_err
            write(report_unit, '(A, E25.17)') "Standard deviation: ", std_dev
            write(report_unit, '(A, I0)') "Valid results: ", n_valid
        end if
    end subroutine write_statistics
    
    subroutine analyze_special_cases()
        integer :: huge_vec, tiny_vec, mixed_vec
        integer :: perfect_accuracy
        
        write(report_unit, '(A)') ""
        write(report_unit, '(A)') "SPECIAL CASE ANALYSIS:"
        write(report_unit, '(A)') "---------------------"
        
        huge_vec = 0
        tiny_vec = 0
        mixed_vec = 0
        perfect_accuracy = 0
        
        do i = 1, TOTAL_TESTS
            if (rel_errors(i) == 0.0_real64) perfect_accuracy = perfect_accuracy + 1
            
            if (test_n(i) > 1000) huge_vec = huge_vec + 1
            if (test_n(i) == 1) tiny_vec = tiny_vec + 1
            
            if (index(test_desc(i), 'mixed') > 0) mixed_vec = mixed_vec + 1
        end do
        
        write(report_unit, '(A, I0, A, F6.2, A)') "Perfect accuracy: ", perfect_accuracy, &
            " (", 100.0 * real(perfect_accuracy) / real(TOTAL_TESTS), "%)"
        write(report_unit, '(A, I0)') "Large vectors (n > 1000): ", huge_vec
        write(report_unit, '(A, I0)') "Single element vectors: ", tiny_vec
        write(report_unit, '(A, I0)') "Mixed magnitude tests: ", mixed_vec
    end subroutine analyze_special_cases

end program validate_denorm_full
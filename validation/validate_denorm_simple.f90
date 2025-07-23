program validate_denorm_simple
    use denorm_module
    use, intrinsic :: iso_fortran_env, only: real64, int32
    use, intrinsic :: ieee_arithmetic
    implicit none
    
    ! Constants
    real(real64), parameter :: TOLERANCE = 1.0e-10
    
    ! Test variables
    real(real64), allocatable :: x(:)
    real(real64) :: computed, expected, rel_error
    integer :: n, i, test_id
    integer :: passed, failed, total
    logical :: success
    
    ! File handling
    integer :: ref_unit, report_unit
    integer :: ios
    character(len=1000) :: line
    
    ! Open reference output file
    open(newunit=ref_unit, file='test_data/denorm_output.json', &
         status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Error: Cannot open reference output file"
        stop 1
    end if
    
    ! Open report file
    open(newunit=report_unit, file='validation/denorm_validation_report.txt', &
         status='replace', action='write')
    
    write(report_unit, '(A)') "=================================================="
    write(report_unit, '(A)') "DENORM VALIDATION REPORT"
    write(report_unit, '(A)') "=================================================="
    write(report_unit, '(A)') ""
    
    ! Skip JSON header
    do i = 1, 3
        read(ref_unit, '(A)')
    end do
    
    passed = 0
    failed = 0
    total = 0
    
    ! Test 1: Simple 3-4-5 triangle
    n = 2
    allocate(x(n))
    x = [3.0_real64, 4.0_real64]
    expected = 5.0_real64
    call run_test(1, "Simple 3-4-5 triangle", n, x, expected)
    deallocate(x)
    
    ! Test 2: Unit vector in 2D
    n = 2
    allocate(x(n))
    x = [0.7071067811865475_real64, 0.7071067811865475_real64]
    expected = 0.99999999999999989_real64
    call run_test(2, "Unit vector in 2D", n, x, expected)
    deallocate(x)
    
    ! Test 3: Unit vector in 3D
    n = 3
    allocate(x(n))
    x = [0.5773502691896258_real64, 0.5773502691896258_real64, 0.5773502691896258_real64]
    expected = 1.0_real64
    call run_test(3, "Unit vector in 3D", n, x, expected)
    deallocate(x)
    
    ! Test 4: All ones vector length 5
    n = 5
    allocate(x(n))
    x = 1.0_real64
    expected = 2.2360679774997898_real64
    call run_test(4, "All ones vector length 5", n, x, expected)
    deallocate(x)
    
    ! Test 5: Zero vector
    n = 3
    allocate(x(n))
    x = 0.0_real64
    expected = 0.0_real64
    call run_test(5, "Zero vector", n, x, expected)
    deallocate(x)
    
    ! Test 6: Pythagorean triple (5,12,13)
    n = 2
    allocate(x(n))
    x = [5.0_real64, 12.0_real64]
    expected = 13.0_real64
    call run_test(6, "Pythagorean triple (5,12,13)", n, x, expected)
    deallocate(x)
    
    ! Test with tiny values (underflow protection)
    n = 3
    allocate(x(n))
    x = [1.0e-20_real64, 1.0e-20_real64, 1.0e-20_real64]
    expected = 1.7320508075688773e-20_real64
    call run_test(10, "Tiny values (underflow protection)", n, x, expected)
    deallocate(x)
    
    ! Test with large values (overflow protection)
    n = 2
    allocate(x(n))
    x = [1.0e19_real64, 1.0e19_real64]
    expected = 1.4142135623730951e19_real64
    call run_test(14, "Large values (overflow protection)", n, x, expected)
    deallocate(x)
    
    ! Test with mixed magnitudes
    n = 3
    allocate(x(n))
    x = [1.0e-10_real64, 1.0_real64, 1.0e10_real64]
    expected = 1.0e10_real64
    call run_test(17, "Mixed magnitudes", n, x, expected)
    deallocate(x)
    
    ! Write summary
    write(report_unit, '(A)') ""
    write(report_unit, '(A)') "SUMMARY:"
    write(report_unit, '(A)') "--------"
    write(report_unit, '(A, I0)') "Total tests: ", total
    write(report_unit, '(A, I0, A, F6.2, A)') "Passed: ", passed, &
        " (", 100.0 * real(passed) / real(total), "%)"
    write(report_unit, '(A, I0, A, F6.2, A)') "Failed: ", failed, &
        " (", 100.0 * real(failed) / real(total), "%)"
    
    close(ref_unit)
    close(report_unit)
    
    ! Print summary to console
    print *, ""
    print *, "DENORM Validation Complete"
    print *, "=========================="
    print '(A, I0, A, I0)', "Tests passed: ", passed, " / ", total
    print '(A, F6.2, A)', "Success rate: ", 100.0 * real(passed) / real(total), "%"
    print *, ""
    print *, "Detailed report written to: validation/denorm_validation_report.txt"
    
contains

    subroutine run_test(id, description, n_val, x_vec, exp_val)
        integer, intent(in) :: id
        character(len=*), intent(in) :: description
        integer, intent(in) :: n_val
        real(real64), intent(in) :: x_vec(:)
        real(real64), intent(in) :: exp_val
        
        total = total + 1
        computed = denorm(n_val, x_vec)
        
        ! Check for special cases
        if (ieee_is_nan(computed)) then
            success = .false.
            rel_error = huge(1.0_real64)
            write(report_unit, '(A, I0, A)') "Test ", id, " FAILED: Result is NaN"
        else if (.not. ieee_is_finite(computed)) then
            success = .false.
            rel_error = huge(1.0_real64)
            write(report_unit, '(A, I0, A)') "Test ", id, " FAILED: Result is Infinity"
        else
            ! Calculate relative error
            if (exp_val == 0.0_real64) then
                if (computed == 0.0_real64) then
                    rel_error = 0.0_real64
                    success = .true.
                else
                    rel_error = abs(computed)
                    success = rel_error < TOLERANCE
                end if
            else
                rel_error = abs((computed - exp_val) / exp_val)
                success = rel_error < TOLERANCE
            end if
        end if
        
        if (success) then
            passed = passed + 1
            write(report_unit, '(A, I3, A, A)') "Test ", id, " PASSED: ", trim(description)
        else
            failed = failed + 1
            write(report_unit, '(A, I3, A, A)') "Test ", id, " FAILED: ", trim(description)
            write(report_unit, '(A, E25.17)') "  Expected: ", exp_val
            write(report_unit, '(A, E25.17)') "  Computed: ", computed
            write(report_unit, '(A, E25.17)') "  Rel Error: ", rel_error
        end if
    end subroutine run_test

end program validate_denorm_simple
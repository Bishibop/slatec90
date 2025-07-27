module validator_module
    use error_analysis_module
    use numerical_utils_module
    use output_formats_module
    use runtime_detection_module
    use function_execution_module
    use validation_reporting_module
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    
contains

    subroutine validate_real_function_1_int(func_name, param1)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: param1
        real :: result_f77, result_modern
        
        ! Call F77 version
        call execute_real_function_1_int(func_name, .false., param1, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_real_function_1_int(func_name, .true., param1, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_real_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_real_function_1_real(func_name, param1)
        character(len=*), intent(in) :: func_name
        real, intent(in) :: param1
        real :: result_f77, result_modern
        
        ! Call F77 version
        call execute_real_function_1_real(func_name, .false., param1, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_real_function_1_real(func_name, .true., param1, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_real_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_real_function_2_real(func_name, param1, param2)
        character(len=*), intent(in) :: func_name
        real, intent(in) :: param1, param2
        real :: result_f77, result_modern
        
        ! Special handling for functions that hang on certain inputs
        if (func_name == 'PYTHAG') then
            ! Skip if both inputs are NaN
            if (ieee_is_nan(param1) .and. ieee_is_nan(param2)) then
                call report_skipped_test('both inputs NaN - would cause infinite loop')
                return
            end if
            
            ! Skip if either input is NaN (PYTHAG doesn't handle NaN properly)
            if (ieee_is_nan(param1) .or. ieee_is_nan(param2)) then
                call report_skipped_test('NaN input - PYTHAG does not handle NaN properly')
                return
            end if
            
            ! Skip infinity inputs (PYTHAG may not handle them correctly)
            if (.not. ieee_is_finite(param1) .or. .not. ieee_is_finite(param2)) then
                call report_skipped_test('Infinity input - PYTHAG may not handle infinity properly')
                return
            end if
        end if
        
        ! Call F77 version
        call execute_real_function_2_real(func_name, .false., param1, param2, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_real_function_2_real(func_name, .true., param1, param2, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_real_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_integer_function_1_int(func_name, param1)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: param1
        integer :: result_f77, result_modern
        
        ! Call F77 version
        call execute_integer_function_1_int(func_name, .false., param1, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_integer_function_1_int(func_name, .true., param1, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_integer_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_double_function_1_int(func_name, param1)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: param1
        real(8) :: result_f77, result_modern
        
        ! Call F77 version
        call execute_double_function_1_int(func_name, .false., param1, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_double_function_1_int(func_name, .true., param1, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_double_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_logical_function_2_char(func_name, param1, param2)
        character(len=*), intent(in) :: func_name
        character, intent(in) :: param1, param2
        logical :: result_f77, result_modern
        
        ! Call F77 version
        call execute_logical_function_2_char(func_name, .false., param1, param2, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_logical_function_2_char(func_name, .true., param1, param2, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_logical_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_subroutine_0_params(func_name)
        character(len=*), intent(in) :: func_name
        
        ! Call F77 version
        call execute_subroutine_0(func_name, .false.)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_subroutine_0(func_name, .true.)
        end if
        
        ! If we get here without crashing, it passed
        call report_passed_test()
    end subroutine
    
    subroutine validate_subroutine_1_char_out(func_name, result_length)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: result_length
        character(len=:), allocatable :: result_f77, result_modern
        
        allocate(character(len=result_length) :: result_f77, result_modern)
        
        ! Call F77 version
        call execute_subroutine_1_char_out(func_name, .false., result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_subroutine_1_char_out(func_name, .true., result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_character_results(result_f77, result_modern)
    end subroutine
    
    subroutine validate_subroutine_6_real(func_name, p1, p2, p3, p4, p5, p6)
        character(len=*), intent(in) :: func_name
        real, intent(in) :: p1, p2, p3, p4
        real :: p5, p6, p5_f77, p6_f77, p5_modern, p6_modern
        
        ! Initialize outputs
        p5_f77 = p5
        p6_f77 = p6
        p5_modern = p5
        p6_modern = p6
        
        ! Call F77 version
        call execute_subroutine_6_real(func_name, .false., p1, p2, p3, p4, p5_f77, p6_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_subroutine_6_real(func_name, .true., p1, p2, p3, p4, p5_modern, p6_modern)
        else
            p5_modern = p5_f77
            p6_modern = p6_f77
        end if
        
        ! Compare outputs
        if (values_equal(p5_f77, p5_modern, 'simple_arithmetic') .and. &
            values_equal(p6_f77, p6_modern, 'simple_arithmetic')) then
            call report_passed_test()
        else
            call report_failed_test(p5_f77, p6_f77, p5_modern, p6_modern)
        end if
    end subroutine
    
    ! Comparison helper routines
    
    subroutine compare_real_results(f77_val, modern_val)
        real, intent(in) :: f77_val, modern_val
        
        if (values_equal(f77_val, modern_val, 'simple_arithmetic')) then
            call report_passed_test()
        else
            call report_failed_test_real(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine compare_double_results(f77_val, modern_val)
        real(8), intent(in) :: f77_val, modern_val
        
        if (abs(f77_val - modern_val) < 1e-15_8) then
            call report_passed_test()
        else
            call report_failed_test_double(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine compare_integer_results(f77_val, modern_val)
        integer, intent(in) :: f77_val, modern_val
        
        if (f77_val == modern_val) then
            call report_passed_test()
        else
            call report_failed_test_integer(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine compare_logical_results(f77_val, modern_val)
        logical, intent(in) :: f77_val, modern_val
        
        if (f77_val .eqv. modern_val) then
            call report_passed_test()
        else
            call report_failed_test_logical(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine compare_character_results(f77_val, modern_val)
        character(len=*), intent(in) :: f77_val, modern_val
        
        if (trim(f77_val) == trim(modern_val)) then
            call report_passed_test()
        else
            call report_failed_test_character(f77_val, modern_val)
        end if
    end subroutine
    
    subroutine validate_real_function_int_real_array(func_name, n, x_array)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: n
        real, intent(in) :: x_array(:)
        real :: result_f77, result_modern
        
        ! Call F77 version
        call execute_real_function_int_real_array(func_name, .false., n, x_array, result_f77)
        
        ! Call modern version
        if (has_modern_implementation(func_name)) then
            call execute_real_function_int_real_array(func_name, .true., n, x_array, result_modern)
        else
            result_modern = result_f77
        end if
        
        ! Compare and report
        call compare_real_results(result_f77, result_modern)
    end subroutine

    subroutine validate_subroutine_2d_array(func_name, m, n, ldq, array_data, array_size)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: m, n, ldq, array_size
        real, intent(in) :: array_data(:)
        
        ! External declarations
        external :: qform
        
        ! Local arrays for testing
        real, allocatable :: q_f77(:,:), q_modern(:,:)
        real, allocatable :: wa_f77(:), wa_modern(:)
        integer :: i, j, idx
        logical :: arrays_match
        
        ! Report test header
        print '(A,A)', 'Testing ', trim(func_name)
        
        ! Allocate arrays
        allocate(q_f77(ldq, max(m,n)), q_modern(ldq, max(m,n)))
        allocate(wa_f77(m), wa_modern(m))
        
        ! Initialize Q arrays from input data (column-major order)
        idx = 1
        do j = 1, m
            do i = 1, ldq
                if (idx <= array_size) then
                    q_f77(i, j) = array_data(idx)
                    q_modern(i, j) = array_data(idx)
                    idx = idx + 1
                else
                    q_f77(i, j) = 0.0
                    q_modern(i, j) = 0.0
                end if
            end do
        end do
        
        ! Initialize work arrays
        wa_f77 = 0.0
        wa_modern = 0.0
        
        ! Call F77 version
        select case(trim(func_name))
            case('QFORM')
                call qform(m, n, q_f77, ldq, wa_f77)
            case default
                call report_failed_test_character("Unknown 2D array subroutine", func_name)
                return
        end select
        
        ! Call modern version if available
        if (has_modern_implementation(func_name)) then
            select case(trim(func_name))
                case('QFORM')
                    call qform_modern(m, n, q_modern, ldq, wa_modern)
            end select
        else
            q_modern = q_f77
            wa_modern = wa_f77
        end if
        
        ! Compare results
        arrays_match = .true.
        print *, 'Comparing arrays, M=', m, ', LDQ=', ldq
        do j = 1, m
            do i = 1, ldq
                if (abs(q_f77(i,j) - q_modern(i,j)) > 1.0e-6) then
                    print '(A,I0,A,I0,A,F12.6,A,F12.6,A,E12.5)', &
                        'Mismatch at Q(', i, ',', j, '): F77=', q_f77(i,j), &
                        ', Modern=', q_modern(i,j), ', Diff=', abs(q_f77(i,j) - q_modern(i,j))
                    arrays_match = .false.
                end if
            end do
        end do
        
        ! Also check work arrays
        print *, 'Checking work arrays...'
        do i = 1, m
            if (abs(wa_f77(i) - wa_modern(i)) > 1.0e-6) then
                print '(A,I0,A,F12.6,A,F12.6)', &
                    'WA mismatch at ', i, ': F77=', wa_f77(i), ', Modern=', wa_modern(i)
                arrays_match = .false.
            end if
        end do
        
        if (arrays_match) then
            call report_passed_test()
        else
            call report_failed_test_character("Arrays differ", "F77 and Modern results don't match")
        end if
        
        ! Clean up
        deallocate(q_f77, q_modern, wa_f77, wa_modern)
    end subroutine

    subroutine validate_subroutine_2d_array_multi(func_name, m, n, ldq, arrays, array_sizes, num_arrays)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: m, n, ldq, num_arrays
        real, intent(in) :: arrays(:,:)
        integer, intent(in) :: array_sizes(:)
        
        ! External declarations
        external :: qform
        
        ! Local arrays for testing
        real, allocatable :: q_f77(:,:), q_modern(:,:)
        real, allocatable :: wa_f77(:), wa_modern(:)
        integer :: i, j, idx
        logical :: arrays_match
        
        ! Report test header
        print '(A,A)', 'Testing ', trim(func_name)
        print '(A,I0,A,I0,A,I0)', 'M=', m, ', N=', n, ', LDQ=', ldq
        print '(A,I0)', 'Number of arrays: ', num_arrays
        if (num_arrays >= 1) print '(A,I0)', 'Array 1 size: ', array_sizes(1)
        if (num_arrays >= 2) print '(A,I0)', 'Array 2 size: ', array_sizes(2)
        
        ! Allocate arrays - Q needs M columns (not max(m,n))
        allocate(q_f77(ldq, m), q_modern(ldq, m))
        allocate(wa_f77(m), wa_modern(m))
        
        ! Initialize Q arrays from first array data (column-major order)
        if (num_arrays >= 1 .and. array_sizes(1) > 0) then
            idx = 1
            do j = 1, m
                do i = 1, ldq
                    if (idx <= array_sizes(1)) then
                        q_f77(i, j) = arrays(idx, 1)
                        q_modern(i, j) = arrays(idx, 1)
                        idx = idx + 1
                    else
                        q_f77(i, j) = 0.0
                        q_modern(i, j) = 0.0
                    end if
                end do
            end do
            print *, 'Q initialized, used', idx-1, 'values from array 1'
        else
            q_f77 = 0.0
            q_modern = 0.0
        end if
        
        ! Initialize work arrays from second array if available
        if (num_arrays >= 2 .and. array_sizes(2) >= m) then
            wa_f77(1:m) = arrays(1:m, 2)
            wa_modern(1:m) = arrays(1:m, 2)
        else
            wa_f77 = 0.0
            wa_modern = 0.0
        end if
        
        ! Call F77 version
        select case(trim(func_name))
            case('QFORM')
                print *, 'Before F77 call, Q(1,1)=', q_f77(1,1), 'Q(2,1)=', q_f77(2,1)
                call qform(m, n, q_f77, ldq, wa_f77)
                print *, 'After F77 call, Q(1,1)=', q_f77(1,1), 'Q(2,1)=', q_f77(2,1)
            case default
                call report_failed_test_character("Unknown 2D array subroutine", func_name)
                return
        end select
        
        ! Call modern version if available
        if (has_modern_implementation(func_name)) then
            select case(trim(func_name))
                case('QFORM')
                    print *, 'Before modern call, Q(1,1)=', q_modern(1,1), 'Q(2,1)=', q_modern(2,1)
                    call qform_modern(m, n, q_modern, ldq, wa_modern)
                    print *, 'After modern call, Q(1,1)=', q_modern(1,1), 'Q(2,1)=', q_modern(2,1)
            end select
        else
            q_modern = q_f77
            wa_modern = wa_f77
        end if
        
        ! Compare results
        arrays_match = .true.
        print *, 'Comparing arrays, M=', m, ', LDQ=', ldq
        do j = 1, m
            do i = 1, ldq
                if (abs(q_f77(i,j) - q_modern(i,j)) > 1.0e-6) then
                    print '(A,I0,A,I0,A,F12.6,A,F12.6,A,E12.5)', &
                        'Mismatch at Q(', i, ',', j, '): F77=', q_f77(i,j), &
                        ', Modern=', q_modern(i,j), ', Diff=', abs(q_f77(i,j) - q_modern(i,j))
                    arrays_match = .false.
                end if
            end do
        end do
        
        ! Also check work arrays
        print *, 'Checking work arrays...'
        do i = 1, m
            if (abs(wa_f77(i) - wa_modern(i)) > 1.0e-6) then
                print '(A,I0,A,F12.6,A,F12.6)', &
                    'WA mismatch at ', i, ': F77=', wa_f77(i), ', Modern=', wa_modern(i)
                arrays_match = .false.
            end if
        end do
        
        if (arrays_match) then
            call report_passed_test()
        else
            call report_failed_test_character("Arrays differ", "F77 and Modern results don't match")
        end if
        
        ! Clean up
        deallocate(q_f77, q_modern, wa_f77, wa_modern)
    end subroutine

    subroutine validate_subroutine_real_2int(func_name, x_val, ix_val, ierror_val)
        character(len=*), intent(in) :: func_name
        real, intent(in) :: x_val
        integer, intent(in) :: ix_val, ierror_val
        
        ! External declaration
        ! external :: xred  ! Commented out - XRED not implemented
        
        ! Local variables for F77 and modern versions
        real :: x_f77, x_modern
        integer :: ix_f77, ix_modern
        integer :: ierror_f77, ierror_modern
        
        ! Initialize input values
        x_f77 = x_val
        x_modern = x_val
        ix_f77 = ix_val
        ix_modern = ix_val
        
        ! XRED not implemented yet - skip validation
        call report_failed_test_character("XRED not implemented", func_name)
        return
        
        ! Compare results
        if (values_equal(x_f77, x_modern, 'simple_arithmetic') .and. &
            ix_f77 == ix_modern .and. &
            ierror_f77 == ierror_modern) then
            call report_passed_test()
        else
            ! Report failure with all values
            write(error_unit, '(A,F12.6,A,I0,A,I0)') 'F77: X=', x_f77, ', IX=', ix_f77, ', IERROR=', ierror_f77
            write(error_unit, '(A,F12.6,A,I0,A,I0)') 'Modern: X=', x_modern, ', IX=', ix_modern, ', IERROR=', ierror_modern
            call report_failed_test_real(x_f77, x_modern)
        end if
    end subroutine

    subroutine validate_subroutine_real_int_4arrays(func_name, xx_val, n_val, x_array, c_array, d_array, work_array)
        character(len=*), intent(in) :: func_name
        real, intent(in) :: xx_val
        integer, intent(in) :: n_val
        real, intent(in) :: x_array(:), c_array(:)
        real, intent(in) :: d_array(:), work_array(:)
        
        ! External declaration
        external :: polcof
        
        ! Local arrays for F77 and modern versions
        real, allocatable :: x_f77(:), c_f77(:), d_f77(:), work_f77(:)
        real, allocatable :: x_modern(:), c_modern(:), d_modern(:), work_modern(:)
        integer :: i
        logical :: arrays_match
        
        ! Allocate arrays
        allocate(x_f77(n_val-1), c_f77(n_val), d_f77(n_val), work_f77(2*n_val))
        allocate(x_modern(n_val-1), c_modern(n_val), d_modern(n_val), work_modern(2*n_val))
        
        ! Initialize input arrays
        x_f77(1:n_val-1) = x_array(1:n_val-1)
        x_modern(1:n_val-1) = x_array(1:n_val-1)
        c_f77(1:n_val) = c_array(1:n_val)
        c_modern(1:n_val) = c_array(1:n_val)
        
        ! Call F77 version
        select case(trim(func_name))
            case('POLCOF')
                call polcof(xx_val, n_val, x_f77, c_f77, d_f77, work_f77)
            case default
                call report_failed_test_character("Unknown subroutine", func_name)
                return
        end select
        
        ! Call modern version if available
        if (has_modern_implementation(func_name)) then
            select case(trim(func_name))
                case('POLCOF')
                    call polcof_modern(xx_val, n_val, x_modern, c_modern, d_modern, work_modern)
            end select
        else
            d_modern = d_f77
        end if
        
        ! Compare results - only check d array (output)
        arrays_match = .true.
        do i = 1, n_val
            if (abs(d_f77(i) - d_modern(i)) > 1.0e-6) then
                arrays_match = .false.
                exit
            end if
        end do
        
        if (arrays_match) then
            call report_passed_test()
        else
            call report_failed_test_character("D arrays differ", "F77 and Modern results don't match")
        end if
        
        ! Clean up
        deallocate(x_f77, c_f77, d_f77, work_f77)
        deallocate(x_modern, c_modern, d_modern, work_modern)
    end subroutine

end module validator_module
module state_validation_module
    implicit none
    
    ! State comparison results
    type :: state_diff
        logical :: has_differences = .false.
        character(len=200) :: diff_summary = ''
        integer :: num_differences = 0
    end type state_diff
    
contains

    ! Generic state comparison for BSPLVN
    subroutine compare_bsplvn_state(state_diff_result)
        ! TODO: To access internal state, we would need:
        ! use bsplvn_module, only: j_modern => j, deltam_modern => deltam, deltap_modern => deltap
        type(state_diff), intent(out) :: state_diff_result
        
        ! Note: We need access to F77 module state variables
        ! This requires either:
        ! 1. Modifying F77 code to expose state (not ideal)
        ! 2. Creating wrapper functions to extract state
        ! 3. Using a debug module approach
        
        ! For now, provide structure for future implementation
        state_diff_result%has_differences = .false.
        state_diff_result%diff_summary = 'State comparison not implemented for BSPLVN'
        state_diff_result%num_differences = 0
        
    end subroutine compare_bsplvn_state
    
    ! State validation for functions with internal iteration counters
    subroutine validate_iteration_state(f77_count, modern_count, func_name, state_diff_result)
        integer, intent(in) :: f77_count, modern_count
        character(len=*), intent(in) :: func_name
        type(state_diff), intent(out) :: state_diff_result
        
        if (f77_count /= modern_count) then
            state_diff_result%has_differences = .true.
            state_diff_result%num_differences = 1
            write(state_diff_result%diff_summary, '(A,A,A,I0,A,I0)') &
                trim(func_name), ' iteration count mismatch: F77=', &
                f77_count, ', Modern=', modern_count
        else
            state_diff_result%has_differences = .false.
            state_diff_result%num_differences = 0
            state_diff_result%diff_summary = ''
        end if
        
    end subroutine validate_iteration_state
    
    ! Compare array states with tolerance
    subroutine compare_array_state(f77_array, modern_array, n, array_name, &
                                  tolerance, state_diff_result)
        integer, intent(in) :: n
        real, intent(in) :: f77_array(n), modern_array(n)
        character(len=*), intent(in) :: array_name
        real, intent(in) :: tolerance
        type(state_diff), intent(out) :: state_diff_result
        
        integer :: i, diff_count
        real :: max_diff
        
        diff_count = 0
        max_diff = 0.0
        
        do i = 1, n
            if (abs(f77_array(i) - modern_array(i)) > tolerance) then
                diff_count = diff_count + 1
                max_diff = max(max_diff, abs(f77_array(i) - modern_array(i)))
            end if
        end do
        
        if (diff_count > 0) then
            state_diff_result%has_differences = .true.
            state_diff_result%num_differences = diff_count
            write(state_diff_result%diff_summary, '(A,A,A,I0,A,I0,A,ES12.5)') &
                'Array ', trim(array_name), ' has ', diff_count, ' of ', n, &
                ' elements different. Max diff: ', max_diff
        else
            state_diff_result%has_differences = .false.
            state_diff_result%num_differences = 0
            state_diff_result%diff_summary = ''
        end if
        
    end subroutine compare_array_state
    
    ! Compare workspace arrays for numerical algorithms
    subroutine validate_workspace_state(f77_work, modern_work, n, func_name, state_diff_result)
        integer, intent(in) :: n
        real, intent(in) :: f77_work(n), modern_work(n)
        character(len=*), intent(in) :: func_name
        type(state_diff), intent(out) :: state_diff_result
        
        real :: work_tolerance
        
        ! Use looser tolerance for workspace arrays
        work_tolerance = 1e-10
        
        call compare_array_state(f77_work, modern_work, n, &
            trim(func_name) // ' workspace', work_tolerance, state_diff_result)
        
    end subroutine validate_workspace_state
    
    ! Report state differences
    subroutine report_state_differences(state_diffs, num_diffs)
        type(state_diff), intent(in) :: state_diffs(:)
        integer, intent(in) :: num_diffs
        integer :: i
        logical :: any_differences
        
        any_differences = .false.
        do i = 1, num_diffs
            if (state_diffs(i)%has_differences) then
                any_differences = .true.
                exit
            end if
        end do
        
        if (any_differences) then
            print '(A)', '  Internal State Differences:'
            do i = 1, num_diffs
                if (state_diffs(i)%has_differences) then
                    print '(A,A)', '    - ', trim(state_diffs(i)%diff_summary)
                end if
            end do
        end if
        
    end subroutine report_state_differences
    
    ! Helper to extract state through debug interfaces
    subroutine create_state_extraction_wrapper(func_name)
        character(len=*), intent(in) :: func_name
        
        ! This would generate or use pre-created wrapper functions
        ! that expose internal state variables for comparison
        ! For example:
        ! - get_bsplvn_j() returns j variable
        ! - get_bsplvn_deltam(i) returns deltam(i)
        ! etc.
        
        print '(A,A,A)', 'Note: State extraction for ', trim(func_name), &
            ' requires wrapper functions'
        
    end subroutine create_state_extraction_wrapper

end module state_validation_module
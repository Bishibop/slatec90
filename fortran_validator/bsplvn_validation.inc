    subroutine validate_bsplvn(desc, t, t_size, params, passed, failed, stats)
        use bsplvn_module, only: bsplvn_modern => bsplvn
        use error_analysis_module
        use numerical_utils_module
        use state_validation_module
        implicit none
        character(len=*), intent(in) :: desc
        real, intent(in) :: t(:)
        integer, intent(in) :: t_size
        real, intent(in) :: params(:)
        integer, intent(inout) :: passed, failed
        type(performance_stats), intent(inout) :: stats
        
        integer :: jhigh, index_val, ileft, i
        real :: x
        real :: vnikx_f77(20), vnikx_modern(20)
        logical :: test_passed
        external bsplvn_f77
        character(len=500) :: error_summary
        type(state_diff) :: state_comparison
        integer :: diff_indices(20), num_diffs
        
        ! Extract parameters
        jhigh = int(params(1))
        index_val = int(params(2))
        x = params(3)
        ileft = int(params(4))
        
        ! Initialize output arrays
        vnikx_f77 = 0.0
        vnikx_modern = 0.0
        
        ! Time F77 version
        call checkpoint_timing(stats, 'START')
        call bsplvn_f77(t, jhigh, index_val, x, ileft, vnikx_f77)
        call checkpoint_timing(stats, 'F77')
        
        ! Time modern version  
        call bsplvn_modern(t, jhigh, index_val, x, ileft, vnikx_modern)
        call checkpoint_timing(stats, 'MODERN')
        
        ! Enhanced array comparison
        test_passed = arrays_equal(vnikx_f77(1:jhigh), vnikx_modern(1:jhigh), &
                                 jhigh, diffs=diff_indices)
        
        ! State validation (if available)
        call compare_bsplvn_state(state_comparison)
        
        if (test_passed .and. .not. state_comparison%has_differences) then
            passed = passed + 1
            call output_test_result(desc, .true.)
        else
            failed = failed + 1
            
            ! Detailed array error analysis
            call analyze_array_error(vnikx_f77(1:jhigh), vnikx_modern(1:jhigh), &
                                   jhigh, desc, error_summary)
            
            ! Output failure details
            if (output_format == FORMAT_HUMAN) then
                print '(A,A)', 'FAIL: ', trim(desc)
                print '(A)', '  Inputs:'
                print '(A,20F8.4)', '    T: ', t(1:t_size)
                print '(A,I3,A,I2,A,F8.4,A,I3)', &
                    '    JHIGH=', jhigh, ' INDEX=', index_val, ' X=', x, ' ILEFT=', ileft
                print '(A)', '  ' // trim(error_summary)
                
                ! Show specific differences
                do i = 1, jhigh
                    if (.not. values_equal(vnikx_f77(i), vnikx_modern(i), 'iterative')) then
                        print '(A,I2,A)', '    vnikx(', i, '):'
                        print '(A,ES16.8)', '      F77:    ', vnikx_f77(i)
                        print '(A,ES16.8)', '      Modern: ', vnikx_modern(i)
                        print '(A,I0,A)', '      ULP diff: ', &
                            ulp_distance(vnikx_f77(i), vnikx_modern(i)), ' ULPs'
                    end if
                end do
                
                ! Report state differences if any
                if (state_comparison%has_differences) then
                    print '(A,A)', '  State: ', trim(state_comparison%diff_summary)
                end if
            else
                call output_test_result(desc, .false., error_info=error_summary)
            end if
        end if
        
    end subroutine validate_bsplvn

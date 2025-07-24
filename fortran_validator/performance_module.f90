module performance_module
    implicit none
    
    type :: performance_stats
        real :: total_time = 0.0
        real :: min_time = huge(1.0)
        real :: max_time = 0.0
        real :: f77_time = 0.0
        real :: modern_time = 0.0
        integer :: test_count = 0
        real :: start_time = 0.0
        real :: last_checkpoint = 0.0
    end type performance_stats
    
    type(performance_stats), save :: global_stats
    
contains

    subroutine start_timing(stats)
        type(performance_stats), intent(inout) :: stats
        call cpu_time(stats%start_time)
        stats%last_checkpoint = stats%start_time
    end subroutine start_timing
    
    subroutine checkpoint_timing(stats, checkpoint_name)
        type(performance_stats), intent(inout) :: stats
        character(len=*), intent(in), optional :: checkpoint_name
        real :: current_time, elapsed
        
        call cpu_time(current_time)
        elapsed = current_time - stats%last_checkpoint
        
        if (present(checkpoint_name)) then
            if (checkpoint_name == 'F77') then
                stats%f77_time = stats%f77_time + elapsed
            else if (checkpoint_name == 'MODERN') then
                stats%modern_time = stats%modern_time + elapsed
            end if
        end if
        
        stats%last_checkpoint = current_time
    end subroutine checkpoint_timing
    
    subroutine end_timing(stats)
        type(performance_stats), intent(inout) :: stats
        real :: current_time, test_time
        
        call cpu_time(current_time)
        test_time = current_time - stats%start_time
        
        stats%total_time = stats%total_time + test_time
        stats%test_count = stats%test_count + 1
        stats%min_time = min(stats%min_time, test_time)
        stats%max_time = max(stats%max_time, test_time)
    end subroutine end_timing
    
    subroutine print_performance_summary(stats, function_name)
        type(performance_stats), intent(in) :: stats
        character(len=*), intent(in), optional :: function_name
        character(len=100) :: header
        
        if (present(function_name)) then
            header = 'Performance Summary for ' // trim(function_name) // ':'
        else
            header = 'Overall Performance Summary:'
        end if
        
        print '(A)', ''
        print '(A)', trim(header)
        print '(A)', repeat('-', len_trim(header))
        
        if (stats%test_count > 0) then
            print '(A,I6)', 'Total tests run:        ', stats%test_count
            print '(A,F10.6,A)', 'Total time:             ', stats%total_time, ' seconds'
            print '(A,F10.6,A)', 'Average time per test:  ', &
                stats%total_time / real(stats%test_count), ' seconds'
            print '(A,F10.6,A)', 'Min test time:          ', stats%min_time, ' seconds'
            print '(A,F10.6,A)', 'Max test time:          ', stats%max_time, ' seconds'
            
            if (stats%f77_time > 0.0 .or. stats%modern_time > 0.0) then
                print '(A)', ''
                print '(A)', 'Implementation breakdown:'
                print '(A,F10.6,A,F6.2,A)', '  F77 time:    ', stats%f77_time, &
                    ' seconds (', 100.0 * stats%f77_time / stats%total_time, '%)'
                print '(A,F10.6,A,F6.2,A)', '  Modern time: ', stats%modern_time, &
                    ' seconds (', 100.0 * stats%modern_time / stats%total_time, '%)'
                
                if (stats%f77_time > 0.0) then
                    print '(A,F6.2,A)', '  Speedup:     ', &
                        stats%f77_time / stats%modern_time, 'x'
                end if
            end if
        else
            print '(A)', 'No tests were run'
        end if
        
    end subroutine print_performance_summary
    
    subroutine reset_stats(stats)
        type(performance_stats), intent(inout) :: stats
        stats%total_time = 0.0
        stats%min_time = huge(1.0)
        stats%max_time = 0.0
        stats%f77_time = 0.0
        stats%modern_time = 0.0
        stats%test_count = 0
        stats%start_time = 0.0
        stats%last_checkpoint = 0.0
    end subroutine reset_stats
    
    function format_time(seconds) result(time_str)
        real, intent(in) :: seconds
        character(len=20) :: time_str
        
        if (seconds < 1e-6) then
            write(time_str, '(F8.3,A)') seconds * 1e9, ' ns'
        else if (seconds < 1e-3) then
            write(time_str, '(F8.3,A)') seconds * 1e6, ' μs'
        else if (seconds < 1.0) then
            write(time_str, '(F8.3,A)') seconds * 1e3, ' ms'
        else
            write(time_str, '(F8.3,A)') seconds, ' s'
        end if
        
    end function format_time

end module performance_module
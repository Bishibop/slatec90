module runtime_detection_module
    use iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_funloc
    implicit none
    private
    public :: has_modern_implementation, report_availability, register_modern
    
    ! Track which modern implementations are available
    logical :: modern_available(1000) = .false.
    character(len=32) :: modern_names(1000) = ''
    integer :: num_tracked = 0
    
contains
    
    subroutine register_modern(func_name, is_available)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: is_available
        integer :: i
        
        ! Check if already registered
        do i = 1, num_tracked
            if (modern_names(i) == func_name) then
                modern_available(i) = is_available
                return
            end if
        end do
        
        ! Add new registration
        if (num_tracked < 1000) then
            num_tracked = num_tracked + 1
            modern_names(num_tracked) = func_name
            modern_available(num_tracked) = is_available
        end if
    end subroutine
    
    function has_modern_implementation(func_name) result(available)
        character(len=*), intent(in) :: func_name
        logical :: available
        integer :: i
        
        available = .false.
        do i = 1, num_tracked
            if (modern_names(i) == func_name) then
                available = modern_available(i)
                return
            end if
        end do
    end function
    
    subroutine report_availability()
        integer :: i, avail_count, missing_count
        
        if (num_tracked == 0) return
        
        avail_count = 0
        missing_count = 0
        
        do i = 1, num_tracked
            if (modern_available(i)) then
                avail_count = avail_count + 1
            else
                missing_count = missing_count + 1
            end if
        end do
        
        print '(A)', ''
        print '(A)', '=== Modern Implementation Status ==='
        print '(A,I0,A)', 'Functions with modern implementations: ', avail_count, ' available'
        print '(A,I0,A)', 'Functions using F77 fallback: ', missing_count, ' functions'
        
        if (missing_count > 0) then
            print '(A)', ''
            print '(A)', 'Functions using F77 fallback:'
            do i = 1, num_tracked
                if (.not. modern_available(i)) then
                    print '(A,A)', '  - ', trim(modern_names(i))
                end if
            end do
        end if
        print '(A)', ''
    end subroutine
    
end module runtime_detection_module
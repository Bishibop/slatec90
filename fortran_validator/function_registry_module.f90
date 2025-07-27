module function_registry_module
    use, intrinsic :: iso_c_binding
    implicit none
    private
    
    ! Public interfaces
    public :: register_function_interfaces
    public :: call_f77_function
    public :: call_modern_function
    
    ! Function pointer types for different signatures
    abstract interface
        ! Real function with 1 real parameter
        real function real_func_1_real(x)
            real, intent(in) :: x
        end function
        
        ! Real function with 2 real parameters
        real function real_func_2_real(x, y)
            real, intent(in) :: x, y
        end function
        
        ! Real function with 1 integer parameter
        real function real_func_1_int(n)
            integer, intent(in) :: n
        end function
        
        ! Integer function with 1 integer parameter
        integer function int_func_1_int(n)
            integer, intent(in) :: n
        end function
        
        ! Logical function with 2 characters
        logical function logical_func_2_char(c1, c2)
            character, intent(in) :: c1, c2
        end function
        
        ! Subroutine with no parameters
        subroutine sub_0_params()
        end subroutine
        
        ! Subroutine with 1 character output
        subroutine sub_1_char_out(str)
            character(len=*), intent(out) :: str
        end subroutine
        
        ! Add more interfaces as needed...
    end interface
    
    ! Function registry entry type
    type :: function_entry
        character(len=20) :: name
        procedure(real_func_1_real), pointer, nopass :: f77_real_1_real => null()
        procedure(real_func_1_real), pointer, nopass :: modern_real_1_real => null()
        procedure(real_func_2_real), pointer, nopass :: f77_real_2_real => null()
        procedure(real_func_2_real), pointer, nopass :: modern_real_2_real => null()
        procedure(real_func_1_int), pointer, nopass :: f77_real_1_int => null()
        procedure(real_func_1_int), pointer, nopass :: modern_real_1_int => null()
        procedure(int_func_1_int), pointer, nopass :: f77_int_1_int => null()
        procedure(int_func_1_int), pointer, nopass :: modern_int_1_int => null()
        procedure(logical_func_2_char), pointer, nopass :: f77_logical_2_char => null()
        procedure(logical_func_2_char), pointer, nopass :: modern_logical_2_char => null()
        procedure(sub_0_params), pointer, nopass :: f77_sub_0 => null()
        procedure(sub_0_params), pointer, nopass :: modern_sub_0 => null()
        procedure(sub_1_char_out), pointer, nopass :: f77_sub_1_char => null()
        procedure(sub_1_char_out), pointer, nopass :: modern_sub_1_char => null()
    end type
    
    ! Registry storage
    integer, parameter :: MAX_FUNCTIONS = 1000
    type(function_entry) :: registry(MAX_FUNCTIONS)
    integer :: num_registered = 0
    
contains

    subroutine register_function_interfaces()
        ! This will be called once to register all function pointers
        ! Include the generated registration code
        include 'function_registrations.inc'
    end subroutine
    
    subroutine register_real_func_1_real(name, f77_ptr, modern_ptr)
        character(len=*), intent(in) :: name
        procedure(real_func_1_real), optional :: f77_ptr, modern_ptr
        integer :: idx
        
        idx = find_or_create_entry(name)
        if (present(f77_ptr)) registry(idx)%f77_real_1_real => f77_ptr
        if (present(modern_ptr)) registry(idx)%modern_real_1_real => modern_ptr
    end subroutine
    
    subroutine register_real_func_2_real(name, f77_ptr, modern_ptr)
        character(len=*), intent(in) :: name
        procedure(real_func_2_real), optional :: f77_ptr, modern_ptr
        integer :: idx
        
        idx = find_or_create_entry(name)
        if (present(f77_ptr)) registry(idx)%f77_real_2_real => f77_ptr
        if (present(modern_ptr)) registry(idx)%modern_real_2_real => modern_ptr
    end subroutine
    
    subroutine register_real_func_1_int(name, f77_ptr, modern_ptr)
        character(len=*), intent(in) :: name
        procedure(real_func_1_int), optional :: f77_ptr, modern_ptr
        integer :: idx
        
        idx = find_or_create_entry(name)
        if (present(f77_ptr)) registry(idx)%f77_real_1_int => f77_ptr
        if (present(modern_ptr)) registry(idx)%modern_real_1_int => modern_ptr
    end subroutine
    
    subroutine register_int_func_1_int(name, f77_ptr, modern_ptr)
        character(len=*), intent(in) :: name
        procedure(int_func_1_int), optional :: f77_ptr, modern_ptr
        integer :: idx
        
        idx = find_or_create_entry(name)
        if (present(f77_ptr)) registry(idx)%f77_int_1_int => f77_ptr
        if (present(modern_ptr)) registry(idx)%modern_int_1_int => modern_ptr
    end subroutine
    
    subroutine register_logical_func_2_char(name, f77_ptr, modern_ptr)
        character(len=*), intent(in) :: name
        procedure(logical_func_2_char), optional :: f77_ptr, modern_ptr
        integer :: idx
        
        idx = find_or_create_entry(name)
        if (present(f77_ptr)) registry(idx)%f77_logical_2_char => f77_ptr
        if (present(modern_ptr)) registry(idx)%modern_logical_2_char => modern_ptr
    end subroutine
    
    subroutine register_sub_0_params(name, f77_ptr, modern_ptr)
        character(len=*), intent(in) :: name
        procedure(sub_0_params), optional :: f77_ptr, modern_ptr
        integer :: idx
        
        idx = find_or_create_entry(name)
        if (present(f77_ptr)) registry(idx)%f77_sub_0 => f77_ptr
        if (present(modern_ptr)) registry(idx)%modern_sub_0 => modern_ptr
    end subroutine
    
    subroutine register_sub_1_char_out(name, f77_ptr, modern_ptr)
        character(len=*), intent(in) :: name
        procedure(sub_1_char_out), optional :: f77_ptr, modern_ptr
        integer :: idx
        
        idx = find_or_create_entry(name)
        if (present(f77_ptr)) registry(idx)%f77_sub_1_char => f77_ptr
        if (present(modern_ptr)) registry(idx)%modern_sub_1_char => modern_ptr
    end subroutine
    
    integer function find_or_create_entry(name) result(idx)
        character(len=*), intent(in) :: name
        integer :: i
        
        ! Search for existing entry
        do i = 1, num_registered
            if (registry(i)%name == name) then
                idx = i
                return
            end if
        end do
        
        ! Create new entry
        if (num_registered < MAX_FUNCTIONS) then
            num_registered = num_registered + 1
            idx = num_registered
            registry(idx)%name = name
        else
            idx = 1  ! Error case - registry full
        end if
    end function
    
    integer function find_entry(name) result(idx)
        character(len=*), intent(in) :: name
        integer :: i
        
        do i = 1, num_registered
            if (registry(i)%name == name) then
                idx = i
                return
            end if
        end do
        idx = 0  ! Not found
    end function
    
    ! Generic call interfaces
    subroutine call_f77_function(func_name, signature_type, &
                                int_params, real_params, logical_result, &
                                int_result, real_result, char_result)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: signature_type
        integer, intent(in), optional :: int_params(:)
        real, intent(in), optional :: real_params(:)
        logical, intent(out), optional :: logical_result
        integer, intent(out), optional :: int_result
        real, intent(out), optional :: real_result
        character(len=*), intent(out), optional :: char_result
        
        integer :: idx
        
        idx = find_entry(func_name)
        if (idx == 0) then
            print *, 'ERROR: Function not registered: ', trim(func_name)
            return
        end if
        
        ! Call appropriate function based on signature_type
        select case(signature_type)
            case(2)  ! SIG_REAL_FUNC_1_REAL
                if (associated(registry(idx)%f77_real_1_real)) then
                    real_result = registry(idx)%f77_real_1_real(real_params(1))
                end if
            case(3)  ! SIG_REAL_FUNC_2_REAL
                if (associated(registry(idx)%f77_real_2_real)) then
                    real_result = registry(idx)%f77_real_2_real(real_params(1), real_params(2))
                end if
            case(1)  ! SIG_REAL_FUNC_1_INT
                if (associated(registry(idx)%f77_real_1_int)) then
                    real_result = registry(idx)%f77_real_1_int(int_params(1))
                end if
            case(4)  ! SIG_INT_FUNC_1_INT
                if (associated(registry(idx)%f77_int_1_int)) then
                    int_result = registry(idx)%f77_int_1_int(int_params(1))
                end if
            case(6)  ! SIG_LOGICAL_FUNC_2_CHAR
                ! Need special handling for character parameters
            case(7)  ! SIG_SUB_0_PARAMS
                if (associated(registry(idx)%f77_sub_0)) then
                    call registry(idx)%f77_sub_0()
                end if
            case(8)  ! SIG_SUB_1_CHAR_OUT
                if (associated(registry(idx)%f77_sub_1_char)) then
                    call registry(idx)%f77_sub_1_char(char_result)
                end if
        end select
    end subroutine
    
    subroutine call_modern_function(func_name, signature_type, &
                                   int_params, real_params, logical_result, &
                                   int_result, real_result, char_result)
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: signature_type
        integer, intent(in), optional :: int_params(:)
        real, intent(in), optional :: real_params(:)
        logical, intent(out), optional :: logical_result
        integer, intent(out), optional :: int_result
        real, intent(out), optional :: real_result
        character(len=*), intent(out), optional :: char_result
        
        integer :: idx
        
        idx = find_entry(func_name)
        if (idx == 0) then
            print *, 'ERROR: Function not registered: ', trim(func_name)
            return
        end if
        
        ! Call appropriate function based on signature_type
        select case(signature_type)
            case(2)  ! SIG_REAL_FUNC_1_REAL
                if (associated(registry(idx)%modern_real_1_real)) then
                    real_result = registry(idx)%modern_real_1_real(real_params(1))
                end if
            case(3)  ! SIG_REAL_FUNC_2_REAL
                if (associated(registry(idx)%modern_real_2_real)) then
                    real_result = registry(idx)%modern_real_2_real(real_params(1), real_params(2))
                end if
            case(1)  ! SIG_REAL_FUNC_1_INT
                if (associated(registry(idx)%modern_real_1_int)) then
                    real_result = registry(idx)%modern_real_1_int(int_params(1))
                end if
            case(4)  ! SIG_INT_FUNC_1_INT
                if (associated(registry(idx)%modern_int_1_int)) then
                    int_result = registry(idx)%modern_int_1_int(int_params(1))
                end if
            case(6)  ! SIG_LOGICAL_FUNC_2_CHAR
                ! Need special handling for character parameters
            case(7)  ! SIG_SUB_0_PARAMS
                if (associated(registry(idx)%modern_sub_0)) then
                    call registry(idx)%modern_sub_0()
                end if
            case(8)  ! SIG_SUB_1_CHAR_OUT
                if (associated(registry(idx)%modern_sub_1_char)) then
                    call registry(idx)%modern_sub_1_char(char_result)
                end if
        end select
    end subroutine

end module function_registry_module
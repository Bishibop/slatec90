module function_execution_module
    ! Module to execute SLATEC functions dynamically
    use, intrinsic :: iso_c_binding
    
    ! Include modern function modules (must be before implicit none)
    include 'functions.inc'
    
    implicit none
    
contains

    subroutine execute_real_function_1_real(func_name, use_modern, param1, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: param1
        real, intent(out) :: result
        
        ! External declarations for F77 functions
        real :: pimach
        external pimach
        
        select case(trim(func_name))
            case('PIMACH')
                if (use_modern) then
                    result = pimach_modern(param1)
                else
                    result = pimach(param1)
                end if
            case default
                print *, 'ERROR: Unknown real function with 1 real param: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_real_function_2_real(func_name, use_modern, param1, param2, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: param1, param2
        real, intent(out) :: result
        
        ! External declarations for F77 functions
        real :: pythag
        external pythag
        
        select case(trim(func_name))
            case('PYTHAG')
                if (use_modern) then
                    result = pythag_modern(param1, param2)
                else
                    result = pythag(param1, param2)
                end if
            case default
                print *, 'ERROR: Unknown real function with 2 real params: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_real_function_1_int(func_name, use_modern, param1, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: param1
        real, intent(out) :: result
        
        ! External declarations for F77 functions
        real :: r1mach
        external r1mach
        
        select case(trim(func_name))
            case('R1MACH')
                if (use_modern) then
                    result = r1mach_modern(param1)
                else
                    result = r1mach(param1)
                end if
            case default
                print *, 'ERROR: Unknown real function with 1 int param: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_integer_function_1_int(func_name, use_modern, param1, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: param1
        integer, intent(out) :: result
        
        ! External declarations for F77 functions
        integer :: i1mach
        external i1mach
        
        select case(trim(func_name))
            case('I1MACH')
                if (use_modern) then
                    result = i1mach_modern(param1)
                else
                    result = i1mach(param1)
                end if
            case default
                print *, 'ERROR: Unknown integer function with 1 int param: ', trim(func_name)
                result = 0
        end select
    end subroutine
    
    subroutine execute_double_function_1_int(func_name, use_modern, param1, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: param1
        real(8), intent(out) :: result
        
        ! External declarations for F77 functions
        real(8) :: d1mach
        external d1mach
        
        select case(trim(func_name))
            case('D1MACH')
                if (use_modern) then
                    result = d1mach_modern(param1)
                else
                    result = d1mach(param1)
                end if
            case default
                print *, 'ERROR: Unknown double function with 1 int param: ', trim(func_name)
                result = 0.0d0
        end select
    end subroutine
    
    subroutine execute_logical_function_2_char(func_name, use_modern, param1, param2, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        character, intent(in) :: param1, param2
        logical, intent(out) :: result
        
        ! External declarations for F77 functions
        logical :: lsame
        external lsame
        
        select case(trim(func_name))
            case('LSAME')
                if (use_modern) then
                    result = lsame_modern(param1, param2)
                else
                    result = lsame(param1, param2)
                end if
            case default
                print *, 'ERROR: Unknown logical function with 2 char params: ', trim(func_name)
                result = .false.
        end select
    end subroutine
    
    subroutine execute_subroutine_0(func_name, use_modern)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        
        ! External declarations for F77 subroutines
        external fdump
        
        select case(trim(func_name))
            case('FDUMP')
                if (use_modern) then
                    call fdump_modern()
                else
                    call fdump()
                end if
            case default
                print *, 'ERROR: Unknown subroutine with 0 params: ', trim(func_name)
        end select
    end subroutine
    
    subroutine execute_subroutine_1_char_out(func_name, use_modern, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        character(len=*), intent(out) :: result
        
        ! External declarations for F77 subroutines
        external aaaaaa
        
        select case(trim(func_name))
            case('AAAAAA')
                if (use_modern) then
                    call aaaaaa_modern(result)
                else
                    call aaaaaa(result)
                end if
            case default
                print *, 'ERROR: Unknown subroutine with 1 char out param: ', trim(func_name)
                result = 'ERROR'
        end select
    end subroutine
    
    subroutine execute_subroutine_6_real(func_name, use_modern, p1, p2, p3, p4, p5, p6)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: p1, p2, p3, p4
        real, intent(out) :: p5, p6
        
        ! External declarations for F77 subroutines
        external cdiv
        
        select case(trim(func_name))
            case('CDIV')
                if (use_modern) then
                    call cdiv_modern(p1, p2, p3, p4, p5, p6)
                else
                    call cdiv(p1, p2, p3, p4, p5, p6)
                end if
            case default
                print *, 'ERROR: Unknown subroutine with 6 real params: ', trim(func_name)
                p5 = 0.0
                p6 = 0.0
        end select
    end subroutine
    
    subroutine execute_real_function_int_real_array(func_name, use_modern, n, x_array, result)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        integer, intent(in) :: n
        real, intent(in) :: x_array(:)
        real, intent(out) :: result
        
        ! External declarations for F77 functions
        real :: enorm
        external enorm
        
        select case(trim(func_name))
            case('ENORM')
                if (use_modern) then
                    result = enorm_modern(n, x_array)
                else
                    result = enorm(n, x_array)
                end if
            case default
                print *, 'ERROR: Unknown real function with int and real array params: ', trim(func_name)
                result = 0.0
        end select
    end subroutine
    
    subroutine execute_real_function_real_int_out(func_name, use_modern, z, result, ierr)
        character(len=*), intent(in) :: func_name
        logical, intent(in) :: use_modern
        real, intent(in) :: z
        real, intent(out) :: result
        integer, intent(out) :: ierr
        
        ! External declarations for F77 functions
        real :: gamln
        external gamln
        
        select case(trim(func_name))
            case('GAMLN')
                if (use_modern) then
                    result = gamln_modern(z, ierr)
                else
                    result = gamln(z, ierr)
                end if
            case default
                print *, 'ERROR: Unknown real function with real input and int out: ', trim(func_name)
                result = 0.0
                ierr = -1
        end select
    end subroutine

end module function_execution_module
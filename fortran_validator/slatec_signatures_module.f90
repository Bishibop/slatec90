module slatec_signatures_module
    implicit none
    
    ! Public parameters
    public :: SIG_UNKNOWN, SIG_REAL_FUNC_1_INT, SIG_REAL_FUNC_1_REAL
    public :: SIG_REAL_FUNC_2_REAL, SIG_INT_FUNC_1_INT, SIG_DOUBLE_FUNC_1_INT
    public :: SIG_LOGICAL_FUNC_2_CHAR, SIG_SUB_0_PARAMS, SIG_SUB_1_CHAR_OUT
    public :: SIG_SUB_6_REAL, SIG_REAL_FUNC_INT_REAL_ARRAY
    public :: function_info, get_function_info, get_signature_type
    public :: TYPE_INTEGER, TYPE_REAL, TYPE_CHARACTER, TYPE_DOUBLE, TYPE_LOGICAL
    public :: INTENT_IN, INTENT_OUT, INTENT_INOUT
    
    ! Signature type constants
    integer, parameter :: SIG_UNKNOWN = 0
    integer, parameter :: SIG_REAL_FUNC_1_INT = 1
    integer, parameter :: SIG_REAL_FUNC_1_REAL = 2
    integer, parameter :: SIG_REAL_FUNC_2_REAL = 3
    integer, parameter :: SIG_INT_FUNC_1_INT = 4
    integer, parameter :: SIG_DOUBLE_FUNC_1_INT = 5
    integer, parameter :: SIG_LOGICAL_FUNC_2_CHAR = 6
    integer, parameter :: SIG_SUB_0_PARAMS = 7
    integer, parameter :: SIG_SUB_1_CHAR_OUT = 8
    integer, parameter :: SIG_SUB_6_REAL = 9
    integer, parameter :: SIG_REAL_FUNC_INT_REAL_ARRAY = 10
    
    ! Function information type
    type :: function_info
        character(len=20) :: name
        integer :: signature_type
        logical :: is_function  ! true for function, false for subroutine
        integer :: num_params
        integer :: param_types(10)  ! 1=int, 2=real, 3=char, 4=double, 5=logical
        integer :: param_intents(10) ! 1=in, 2=out, 3=inout
        integer :: return_type      ! for functions only
    end type function_info
    
    ! Parameter type constants
    integer, parameter :: TYPE_INTEGER = 1
    integer, parameter :: TYPE_REAL = 2
    integer, parameter :: TYPE_CHARACTER = 3
    integer, parameter :: TYPE_DOUBLE = 4
    integer, parameter :: TYPE_LOGICAL = 5
    
    ! Intent constants
    integer, parameter :: INTENT_IN = 1
    integer, parameter :: INTENT_OUT = 2
    integer, parameter :: INTENT_INOUT = 3
    
    ! Number of registered functions
    integer, parameter :: NUM_FUNCTIONS = 19
    
    ! Function registry
    type(function_info), parameter :: FUNCTION_REGISTRY(NUM_FUNCTIONS) = [ &
        function_info( &
            name='AAAAAA              ', &
            signature_type=8, &
            is_function=.false., &
            num_params=1, &
            param_types=[3,0,0,0,0,0,0,0,0,0], &
            param_intents=[2,0,0,0,0,0,0,0,0,0], &
            return_type=0 &
        ), &
        function_info( &
            name='CDIV                ', &
            signature_type=9, &
            is_function=.false., &
            num_params=6, &
            param_types=[2,2,2,2,2,2,0,0,0,0], &
            param_intents=[1,1,1,1,2,2,0,0,0,0], &
            return_type=0 &
        ), &
        function_info( &
            name='D1MACH              ', &
            signature_type=5, &
            is_function=.true., &
            num_params=1, &
            param_types=[1,0,0,0,0,0,0,0,0,0], &
            param_intents=[1,0,0,0,0,0,0,0,0,0], &
            return_type=4 &
        ), &
        function_info( &
            name='FDUMP               ', &
            signature_type=7, &
            is_function=.false., &
            num_params=0, &
            param_types=[0,0,0,0,0,0,0,0,0,0], &
            param_intents=[0,0,0,0,0,0,0,0,0,0], &
            return_type=0 &
        ), &
        function_info( &
            name='I1MACH              ', &
            signature_type=4, &
            is_function=.true., &
            num_params=1, &
            param_types=[1,0,0,0,0,0,0,0,0,0], &
            param_intents=[1,0,0,0,0,0,0,0,0,0], &
            return_type=1 &
        ), &
        function_info( &
            name='LSAME               ', &
            signature_type=6, &
            is_function=.true., &
            num_params=2, &
            param_types=[3,3,0,0,0,0,0,0,0,0], &
            param_intents=[1,1,0,0,0,0,0,0,0,0], &
            return_type=5 &
        ), &
        function_info( &
            name='PIMACH              ', &
            signature_type=2, &
            is_function=.true., &
            num_params=1, &
            param_types=[2,0,0,0,0,0,0,0,0,0], &
            param_intents=[1,0,0,0,0,0,0,0,0,0], &
            return_type=2 &
        ), &
        function_info( &
            name='PYTHAG              ', &
            signature_type=3, &
            is_function=.true., &
            num_params=2, &
            param_types=[2,2,0,0,0,0,0,0,0,0], &
            param_intents=[1,1,0,0,0,0,0,0,0,0], &
            return_type=2 &
        ), &
        function_info( &
            name='R1MACH              ', &
            signature_type=1, &
            is_function=.true., &
            num_params=1, &
            param_types=[1,0,0,0,0,0,0,0,0,0], &
            param_intents=[1,0,0,0,0,0,0,0,0,0], &
            return_type=2 &
        ), &
        function_info( &
            name='HYPOT               ', &
            signature_type=3, &
            is_function=.true., &
            num_params=2, &
            param_types=[2,2,0,0,0,0,0,0,0,0], &
            param_intents=[1,1,0,0,0,0,0,0,0,0], &
            return_type=2 &
        ), &
        function_info( &
            name='CSROOT              ', &
            signature_type=0, &
            is_function=.false., &
            num_params=4, &
            param_types=[2,2,2,2,0,0,0,0,0,0], &
            param_intents=[1,1,2,2,0,0,0,0,0,0], &
            return_type=0 &
        ), &
        function_info( &
            name='SVOUT               ', &
            signature_type=0, &
            is_function=.false., &
            num_params=4, &
            param_types=[2,2,3,2,0,0,0,0,0,0], &
            param_intents=[1,1,1,1,0,0,0,0,0,0], &
            return_type=0 &
        ), &
        function_info( &
            name='ZEXP                ', &
            signature_type=0, &
            is_function=.false., &
            num_params=4, &
            param_types=[4,4,4,4,0,0,0,0,0,0], &
            param_intents=[1,1,2,2,0,0,0,0,0,0], &
            return_type=0 &
        ), &
        function_info( &
            name='QWGTC               ', &
            signature_type=0, &
            is_function=.true., &
            num_params=6, &
            param_types=[2,2,2,2,2,1,0,0,0,0], &
            param_intents=[1,1,1,1,1,1,0,0,0,0], &
            return_type=2 &
        ), &
        function_info( &
            name='ZABS                ', &
            signature_type=0, &
            is_function=.true., &
            num_params=2, &
            param_types=[4,4,0,0,0,0,0,0,0,0], &
            param_intents=[1,1,0,0,0,0,0,0,0,0], &
            return_type=4 &
        ), &
        function_info( &
            name='ENORM               ', &
            signature_type=10, &
            is_function=.true., &
            num_params=2, &
            param_types=[1,2,0,0,0,0,0,0,0,0], &
            param_intents=[1,1,0,0,0,0,0,0,0,0], &
            return_type=2 &
        ), &
        function_info( &
            name='POLCOF              ', &
            signature_type=9, &
            is_function=.false., &
            num_params=6, &
            param_types=[2,2,2,2,2,2,0,0,0,0], &
            param_intents=[1,1,1,1,3,3,0,0,0,0], &
            return_type=0 &
        ), &
        function_info( &
            name='XRED                ', &
            signature_type=0, &
            is_function=.false., &
            num_params=3, &
            param_types=[2,1,2,0,0,0,0,0,0,0], &
            param_intents=[1,1,2,0,0,0,0,0,0,0], &
            return_type=0 &
        ), &
        function_info( &
            name='QFORM               ', &
            signature_type=0, &
            is_function=.false., &
            num_params=5, &
            param_types=[1,1,2,1,2,0,0,0,0,0], &
            param_intents=[1,1,3,1,3,0,0,0,0,0], &
            return_type=0 &
        ) &
    ]
    
contains

    function get_function_info(func_name) result(info)
        character(len=*), intent(in) :: func_name
        type(function_info) :: info
        integer :: i
        character(len=20) :: upper_name
        
        ! Convert to uppercase for comparison
        upper_name = func_name
        call to_upper(upper_name)
        
        ! Search registry
        do i = 1, NUM_FUNCTIONS
            if (trim(FUNCTION_REGISTRY(i)%name) == trim(upper_name)) then
                info = FUNCTION_REGISTRY(i)
                return
            end if
        end do
        
        ! Not found - return unknown
        info%name = upper_name
        info%signature_type = SIG_UNKNOWN
        info%is_function = .false.
        info%num_params = 0
        info%param_types = 0
        info%param_intents = 0
        info%return_type = 0
    end function
    
    function get_signature_type(func_name) result(sig_type)
        character(len=*), intent(in) :: func_name
        integer :: sig_type
        type(function_info) :: info
        
        info = get_function_info(func_name)
        sig_type = info%signature_type
    end function
    
    subroutine to_upper(str)
        character(len=*), intent(inout) :: str
        integer :: i
        
        do i = 1, len_trim(str)
            if (str(i:i) >= 'a' .and. str(i:i) <= 'z') then
                str(i:i) = char(iachar(str(i:i)) - 32)
            end if
        end do
    end subroutine

end module slatec_signatures_module

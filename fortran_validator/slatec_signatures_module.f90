module slatec_signatures_module
    implicit none
    
    ! Public parameters
    public :: SIG_UNKNOWN, function_info, get_function_info, get_signature_type
    public :: TYPE_INTEGER, TYPE_REAL, TYPE_CHARACTER, TYPE_DOUBLE, TYPE_LOGICAL
    public :: INTENT_IN, INTENT_OUT, INTENT_INOUT
    
    ! Signature type constants - AUTO-GENERATED
    integer, parameter :: SIG_UNKNOWN = 0
    integer, parameter :: SIG_SUB_0_PARAMS = 1
    integer, parameter :: SIG_SUB_REAL_REAL_REAL_REAL_REAL_REAL = 2
    integer, parameter :: SIG_SUB_REAL_REAL_REAL_REAL = 3
    integer, parameter :: SIG_FUNC_DOUBLE_INTEGER = 4
    integer, parameter :: SIG_FUNC_REAL_INTEGER_ARR_REAL_ARR = 5
    integer, parameter :: SIG_FUNC_REAL_REAL_INTEGER = 6
    integer, parameter :: SIG_FUNC_INTEGER_INTEGER = 7
    integer, parameter :: SIG_SUB_REAL_ARR_INTEGER_ARR_REAL_ARR_INTEGE_B84D1923 = 8
    integer, parameter :: SIG_FUNC_LOGICAL_0_PARAMS = 9
    integer, parameter :: SIG_FUNC_REAL_REAL = 10
    integer, parameter :: SIG_SUB_REAL_ARR_INTEGER_ARR_REAL_ARR_REAL_A_65000675 = 11
    integer, parameter :: SIG_FUNC_REAL_REAL_REAL = 12
    integer, parameter :: SIG_SUB_INTEGER_ARR_INTEGER_ARR_REAL_ARR_REAL_ARR = 13
    integer, parameter :: SIG_FUNC_REAL_REAL_REAL_REAL_REAL_REAL_INTEGER = 14
    integer, parameter :: SIG_FUNC_REAL_INTEGER = 15
    integer, parameter :: SIG_FUNC_DOUBLE_DOUBLE_DOUBLE = 16
    
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
    
    ! Function registry - AUTO-GENERATED
    integer, parameter :: NUM_FUNCTIONS = 16

    type(function_info), parameter :: FUNCTION_REGISTRY(NUM_FUNCTIONS) = [ &
            function_info( &
            name='AAAAAA              ', &
            signature_type=1, &
            is_function=.false., &
            num_params=0, &
            param_types=[0,0,0,0,0,0,0,0,0,0], &
            param_intents=[0,0,0,0,0,0,0,0,0,0], &
            return_type=0 &
        ), &
            function_info( &
            name='CDIV                ', &
            signature_type=2, &
            is_function=.false., &
            num_params=6, &
            param_types=[TYPE_REAL,TYPE_REAL,TYPE_REAL,TYPE_REAL,TYPE_REAL,TYPE_REAL,0,0,0,0], &
            param_intents=[1,1,1,1,2,2,0,0,0,0], &
            return_type=0 &
        ), &
            function_info( &
            name='CSROOT              ', &
            signature_type=3, &
            is_function=.false., &
            num_params=4, &
            param_types=[TYPE_REAL,TYPE_REAL,TYPE_REAL,TYPE_REAL,0,0,0,0,0,0], &
            param_intents=[1,1,1,1,0,0,0,0,0,0], &
            return_type=0 &
        ), &
            function_info( &
            name='D1MACH              ', &
            signature_type=4, &
            is_function=.true., &
            num_params=1, &
            param_types=[TYPE_INTEGER,0,0,0,0,0,0,0,0,0], &
            param_intents=[1,0,0,0,0,0,0,0,0,0], &
            return_type=TYPE_DOUBLE &
        ), &
            function_info( &
            name='ENORM               ', &
            signature_type=5, &
            is_function=.true., &
            num_params=2, &
            param_types=[TYPE_INTEGER,TYPE_REAL,0,0,0,0,0,0,0,0], &
            param_intents=[1,1,0,0,0,0,0,0,0,0], &
            return_type=TYPE_REAL &
        ), &
            function_info( &
            name='GAMLN               ', &
            signature_type=6, &
            is_function=.true., &
            num_params=2, &
            param_types=[TYPE_REAL,TYPE_INTEGER,0,0,0,0,0,0,0,0], &
            param_intents=[1,2,0,0,0,0,0,0,0,0], &
            return_type=TYPE_REAL &
        ), &
            function_info( &
            name='I1MACH              ', &
            signature_type=7, &
            is_function=.true., &
            num_params=1, &
            param_types=[TYPE_INTEGER,0,0,0,0,0,0,0,0,0], &
            param_intents=[1,0,0,0,0,0,0,0,0,0], &
            return_type=TYPE_INTEGER &
        ), &
            function_info( &
            name='INTRV               ', &
            signature_type=8, &
            is_function=.false., &
            num_params=6, &
            param_types=[TYPE_REAL,TYPE_INTEGER,TYPE_REAL,TYPE_INTEGER,TYPE_INTEGER,TYPE_INTEGER,0,0,0,0], &
            param_intents=[1,1,1,1,2,2,0,0,0,0], &
            return_type=0 &
        ), &
            function_info( &
            name='LSAME               ', &
            signature_type=9, &
            is_function=.true., &
            num_params=0, &
            param_types=[0,0,0,0,0,0,0,0,0,0], &
            param_intents=[0,0,0,0,0,0,0,0,0,0], &
            return_type=TYPE_LOGICAL &
        ), &
            function_info( &
            name='PIMACH              ', &
            signature_type=10, &
            is_function=.true., &
            num_params=1, &
            param_types=[TYPE_REAL,0,0,0,0,0,0,0,0,0], &
            param_intents=[1,0,0,0,0,0,0,0,0,0], &
            return_type=TYPE_REAL &
        ), &
            function_info( &
            name='POLCOF              ', &
            signature_type=11, &
            is_function=.false., &
            num_params=6, &
            param_types=[TYPE_REAL,TYPE_INTEGER,TYPE_REAL,TYPE_REAL,TYPE_REAL,TYPE_REAL,0,0,0,0], &
            param_intents=[1,1,1,1,2,2,0,0,0,0], &
            return_type=0 &
        ), &
            function_info( &
            name='PYTHAG              ', &
            signature_type=12, &
            is_function=.true., &
            num_params=2, &
            param_types=[TYPE_REAL,TYPE_REAL,0,0,0,0,0,0,0,0], &
            param_intents=[1,1,0,0,0,0,0,0,0,0], &
            return_type=TYPE_REAL &
        ), &
            function_info( &
            name='QFORM               ', &
            signature_type=13, &
            is_function=.false., &
            num_params=4, &
            param_types=[TYPE_INTEGER,TYPE_INTEGER,TYPE_REAL,TYPE_REAL,0,0,0,0,0,0], &
            param_intents=[1,1,2,2,0,0,0,0,0,0], &
            return_type=0 &
        ), &
            function_info( &
            name='QWGTC               ', &
            signature_type=14, &
            is_function=.true., &
            num_params=6, &
            param_types=[TYPE_REAL,TYPE_REAL,TYPE_REAL,TYPE_REAL,TYPE_REAL,TYPE_INTEGER,0,0,0,0], &
            param_intents=[1,1,1,1,1,1,0,0,0,0], &
            return_type=TYPE_REAL &
        ), &
            function_info( &
            name='R1MACH              ', &
            signature_type=15, &
            is_function=.true., &
            num_params=1, &
            param_types=[TYPE_INTEGER,0,0,0,0,0,0,0,0,0], &
            param_intents=[1,0,0,0,0,0,0,0,0,0], &
            return_type=TYPE_REAL &
        ), &
            function_info( &
            name='ZABS                ', &
            signature_type=16, &
            is_function=.true., &
            num_params=2, &
            param_types=[TYPE_DOUBLE,TYPE_DOUBLE,0,0,0,0,0,0,0,0], &
            param_intents=[1,1,0,0,0,0,0,0,0,0], &
            return_type=TYPE_DOUBLE &
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
program run_xerhlt_blind_tests
    use xerhlt_module
    implicit none
    
    integer :: test_number
    character(len=1000) :: test_msg
    character(len=10) :: test_arg
    
    ! Get test number from command line argument
    if (command_argument_count() /= 1) then
        print *, "ERROR: Must provide test number as argument"
        stop 1
    end if
    
    call get_command_argument(1, test_arg)
    read(test_arg, *) test_number
    
    ! Select test case based on test number
    select case (test_number)
        case (1)
            test_msg = ""
        case (2)
            test_msg = "ERROR"
        case (3)
            test_msg = "FATAL"
        case (4)
            test_msg = "ABORT"
        case (5)
            test_msg = "STOP"
        case (6)
            test_msg = "HALT"
        case (7)
            test_msg = "FAIL"
        case (8)
            test_msg = "DIV BY 0"
        case (9)
            test_msg = "OVERFLOW"
        case (10)
            test_msg = "UNDERFLOW"
        case (11)
            test_msg = "NAN"
        case (12)
            test_msg = "INF"
        case (13)
            test_msg = "SLATEC     PYTHAG     OVERFLOW HAS OCCURRED."
        case (14)
            test_msg = "SLATEC     GAMMA      X IS A NEGATIVE INTEGER."
        case (15)
            test_msg = "SLATEC     BESI       OVERFLOW, X TOO LARGE."
        case (16)
            test_msg = "SLATEC     BESJ       ORDER, ALPHA, LESS THAN ZERO."
        case (17)
            test_msg = "SLATEC     ENORM      N LESS THAN 1."
        case (18)
            test_msg = "SLATEC     CDIV       DIVISION BY ZERO."
        case (19)
            test_msg = "LINPACK    SGEFA      SINGULAR MATRIX ENCOUNTERED."
        case (20)
            test_msg = "EISPACK    RS         NO CONVERGENCE IN 30 ITERATIONS."
        case (21)
            test_msg = "BLAS       SAXPY      N IS LESS THAN OR EQUAL TO 0."
        case (22)
            test_msg = "LAPACK     DGESV      THE FACTOR U IS SINGULAR."
        case (23)
            test_msg = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        case (24)
            test_msg = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        case (25)
            test_msg = "ERROR: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        case (26)
            test_msg = "FATAL ERROR IN SUBROUTINE XYZ: INVALID PARAMETER VALUE DETECTED. THE INPUT PARAMETER N MUST BE POSITIVE BUT WAS NEGATIVE. PLEASE CHECK YOUR INPUT VALUES AND TRY AGAIN."
        case (27)
            test_msg = "CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. CATASTROPHIC FAILURE: MEMORY ALLOCATION ERROR. "
        case (28)
            test_msg = "ERROR: Value = -1.234E+56"
        case (29)
            test_msg = "HALT: Matrix(1,1) = NaN"
        case (30)
            test_msg = "ABORT: |X| > 1.0E+100"
        case (31)
            test_msg = "STOP: A(I,J) != B(J,I)"
        case (32)
            test_msg = "FATAL: X < 0.0 OR X > 1.0"
        case (33)
            test_msg = "ERROR: SQRT(-1) ATTEMPTED"
        case (34)
            test_msg = "HALT: 1/0 DIVISION"
        case (35)
            test_msg = "ABORT: LOG(0) UNDEFINED"
        case (36)
            test_msg = "STOP: SIN(X)/COS(X) WHERE COS(X)=0"
        case (37)
            test_msg = "FATAL: EXP(X) OVERFLOW, X=1000"
        case (38)
            test_msg = "   ERROR: LEADING SPACES"
        case (39)
            test_msg = "ERROR: TRAILING SPACES   "
        case (40)
            test_msg = "ERROR:    MULTIPLE    SPACES"
        case (41)
            test_msg = "ERROR:"//char(9)//"TAB"//char(9)//"CHARACTERS"
        case (42)
            test_msg = "ERROR:"//char(10)//"NEWLINE"//char(10)//"CHARACTERS"
        case (43)
            test_msg = "ERROR: MIXED   "//char(9)//"  WHITESPACE"
        case (44)
            test_msg = "*** ERROR ***"
        case (45)
            test_msg = ">>> FATAL ERROR <<<"
        case (46)
            test_msg = "!!! ABORT !!!"
        case (47)
            test_msg = "### SYSTEM HALT ###"
        case (48)
            test_msg = "ERROR CODE 1"
        case (49)
            test_msg = "ERROR CODE 12345"
        case (50)
            test_msg = "ERROR -999"
        case default
            print *, "ERROR: Test number out of range (1-156)"
            stop 1
    end select
    
    ! Output test info
    print *, "Test", test_number, ": Calling XERHLT with message:"
    print *, trim(test_msg)
    
    ! Call XERHLT - this should halt execution
    call xerhlt(test_msg)
    
    ! This should never be reached
    print *, "ERROR: XERHLT did not halt execution!"
    stop 2
    
end program run_xerhlt_blind_tests